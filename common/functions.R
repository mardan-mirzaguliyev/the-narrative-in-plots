library(httr2)
library(ollamar)
library(reticulate)
library(purrr)
library(jsonlite)
library(tidyverse)
library(pdftools)
library(stringr)
library(tibble)


# Local LLM connection
score_local_llm <- function(text_col, model = "llama3.2:latest") {
  
  system_prompt <- '
  You are a professional sentiment analysis engine for literary text.
  Analyze the emotional tone of the provided text and output a single JSON object matching this exact schema:
{
  "sentiment": string,   // exactly one of: "Trust" | "Fear" | "Sadness" | "Anger" | "Surprise" | "Disgust" | "Joy" | "Anticipation" | "Neutral"
  "confidence_score": number  // 0.0–1.0, your certainty that this is the single best-fitting label (not the intensity of the emotion)
}
Rules:
1. Output ONLY the raw JSON object. No markdown code fences, no backticks, no preamble, no explanation, no trailing text.
2. Choose exactly one sentiment label, even when the text expresses mixed emotions — pick the single most dominant one.
3. If the text is ambiguous or blended, still choose the most probable label, but assign confidence_score below 0.5.
4. If the input is empty, whitespace-only, or nonsensical (not natural language), output {"sentiment": "Neutral", "confidence_score": 0.0}.
5. Base your judgment on the emotional content of the text itself, not on real-world factual accuracy or your own opinion of the content.
6. Do not add, remove, or rename JSON keys. Do not wrap the object in an array.
Examples:
Input: "The old house creaked, and she felt the walls closing in around her."
Output: {"sentiment": "Fear", "confidence_score": 0.82}
Input: "asdkj 12341 !!!"
Output: {"sentiment": "Neutral", "confidence_score": 0.0}'
  
  valid_labels <- c("Trust", "Fear", "Sadness", "Anger", "Surprise", 
                    "Disgust", "Joy", "Anticipation", "Neutral")
  
  json_schema <- list(
    type = "object",
    properties = list(
      sentiment = list(type = "string", enum = valid_labels),
      confidence_score = list(type = "number")
    ),
    required = list("sentiment", "confidence_score")
  )
  
  tryCatch({
    
    res <- generate(
      model = model,
      prompt = {{ text_col }},
      system = system_prompt,
      format = json_schema,
      output = "text",
      temperature = 0
    )
    
    parsed <- fromJSON(res)
    
    if (is.null(parsed$sentiment) || !parsed$sentiment %in% valid_labels) {
      message("Invalid/missing sentiment label for input: ", 
              substr({{ text_col }}, 1, 60), " | got: ", parsed$sentiment %||% "NULL")
      parsed$sentiment <- "Neutral"
    }
    
    if (is.null(parsed$confidence_score) || !is.numeric(parsed$confidence_score)) {
      parsed$confidence_score <- 0.0
    } else {
      parsed$confidence_score <- pmin(pmax(parsed$confidence_score, 0), 1)  # clamp to [0,1]
    }
    
    return(parsed)
    
  }, error = function(e) {
    message("Local LLM call failed for input: ", substr({{ text_col }}, 1, 60), 
            " | ", conditionMessage(e))
    return(list(sentiment = "Neutral", confidence_score = 0.0))
  })
}


## Synchronous wrapper for local scoring — mirrors analyze_text_claude_synch()
analyze_text_local <- function(df, text_col, model = "llama3.2:latest") {
  df |>
    mutate(sentiment_data = map({{ text_col }}, ~score_local_llm(.x, model = model))) |> 
    unnest_wider(sentiment_data)
}


# Claude API Connection

Sys.getenv("CLAUDE_API_KEY")

### Get the list of Claude Models available
get_clean_model_list <- function() {
  req <- request("https://api.anthropic.com/v1/models") |> 
    req_headers(
      "x-api-key" = Sys.getenv("CLAUDE_API_KEY"),
      "anthropic-version" = "2023-06-01"
    )
  
  resp <- req_perform(req)
  data <- resp_body_json(resp)$data
  
  # Flatten and convert to a clean tibble
  model_df <- map_dfr(data, ~tibble(
    id = .x$id,
    display_name = .x$display_name,
    created_at = .x$created_at
  ))
  
  return(model_df)
}

get_clean_model_list()


## ================= SHARED =================

sentiment_system_prompt <- '
You are a professional sentiment analysis engine for literary text. Analyze the emotional tone of the provided text and output a single JSON object matching this exact schema:
{
  "sentiment": string,   // exactly one of: "Trust" | "Fear" | "Sadness" | "Anger" | "Surprise" | "Disgust" | "Joy" | "Anticipation" | "Neutral"
  "confidence_score": number  // 0.0-1.0, your certainty that this is the single best-fitting label (not the intensity of the emotion)
}
Rules:
1. Output ONLY the raw JSON object. No markdown code fences, no backticks, no preamble, no explanation, no trailing text.
2. Choose exactly one sentiment label, even when the text expresses mixed emotions - pick the single most dominant one.
3. If the text is ambiguous or blended, still choose the most probable label, but assign confidence_score below 0.5.
4. If the input is empty, whitespace-only, or nonsensical (not natural language), output {"sentiment": "Neutral", "confidence_score": 0.0}.
5. Base your judgment on the emotional content of the text itself, not on real-world factual accuracy or your own opinion of the content.
6. Do not add, remove, or rename JSON keys. Do not wrap the object in an array.
Examples:
Input: "The old house creaked, and she felt the walls closing in around her."
Output: {"sentiment": "Fear", "confidence_score": 0.82}
Input: "asdkj 12341 !!!"
Output: {"sentiment": "Neutral", "confidence_score": 0.0}'

## Pulls sentiment JSON out of a `content` block list.
## Used by both score_claude_synch() (sync) and parse_batch_result_line() (batch).
extract_sentiment_json <- function(content_blocks) {
  text_blocks <- Filter(function(b) b$type == "text", content_blocks)
  if (length(text_blocks) == 0) return(NULL)
  
  content_text <- text_blocks[[1]]$text
  clean_json <- gsub("(?s).*(\\{.*\\}).*", "\\1", content_text, perl = TRUE) |> trimws()
  
  fromJSON(clean_json)
}

## ================= SYNCHRONOUS PATH =================

score_claude_synch <- function(text_input, model = "claude-sonnet-5") {
  
  req <- request("https://api.anthropic.com/v1/messages") |> 
    req_headers(
      "x-api-key" = Sys.getenv("CLAUDE_API_KEY"),
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ) |> 
    req_body_json(list(
      model = model,
      max_tokens = 200,
      thinking = list(type = "disabled"),
      system = sentiment_system_prompt,
      messages = list(list(role = "user", content = text_input))
    )) |> 
    req_retry(
      max_tries = 4,
      backoff = ~ 2 ^ .x,
      is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503, 529)
    )
  
  tryCatch({
    
    resp <- req_perform(req)
    resp_body <- resp_body_json(resp)
    
    parsed <- extract_sentiment_json(resp_body$content)
    
    if (is.null(parsed)) {
      message("No text block returned for input: ", 
              substr(text_input, 1, 60), " | stop_reason: ", 
              resp_body$stop_reason %||% "unknown")
      return(list(sentiment = NA, confidence_score = NA))
    }
    
    parsed
    
  }, httr2_http = function(e) {
    status <- tryCatch(resp_status(e$resp), error = function(e2) NA)
    body_msg <- tryCatch(resp_body_json(e$resp)$error$message, error = function(e2) conditionMessage(e))
    message("HTTP error for input: ", substr(text_input, 1, 60), " | status: ", status, " | ", body_msg)
    list(sentiment = NA, confidence_score = NA)
    
  }, error = function(e) {
    message("Parse/other error for input: ", substr(text_input, 1, 60), " | ", conditionMessage(e))
    list(sentiment = NA, confidence_score = NA)
  })
}

analyze_text_claude_synch <- function(text_df, text_col, model = "claude-sonnet-5") {
  text_df |>
    mutate(sentiment_data = map({{ text_col }}, ~score_claude_synch(.x, model = model))) |> 
    unnest_wider(sentiment_data)
}


## ================= BATCH PATH =================

build_batch_requests <- function(df, text_col, id_col, model = "claude-sonnet-5") {
  pmap(list(pull(df, {{ id_col }}), pull(df, {{ text_col }})), function(id, txt) {
    list(
      custom_id = paste0("row_", id),
      params = list(
        model = model,
        max_tokens = 200,
        thinking = list(type = "disabled"),
        system = list(list(
          type = "text",
          text = sentiment_system_prompt,
          cache_control = list(type = "ephemeral", ttl = "1h")
        )),
        messages = list(list(role = "user", content = txt))
      )
    )
  })
}

submit_batch <- function(requests) {
  req <- request("https://api.anthropic.com/v1/messages/batches") |>
    req_headers(
      "x-api-key" = Sys.getenv("CLAUDE_API_KEY"),
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ) |>
    req_body_json(list(requests = requests)) |>
    req_retry(max_tries = 4, backoff = ~ 2 ^ .x,
              is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503, 529))
  
  tryCatch({
    req_perform(req) |> resp_body_json() |> pluck("id")
  }, error = function(e) stop("Batch submission failed: ", conditionMessage(e)))
}

poll_batch <- function(batch_id, poll_interval = 60) {
  status_req <- request(paste0("https://api.anthropic.com/v1/messages/batches/", batch_id)) |>
    req_headers("x-api-key" = Sys.getenv("CLAUDE_API_KEY"), "anthropic-version" = "2023-06-01") |>
    req_retry(max_tries = 4, backoff = ~ 2 ^ .x,
              is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503, 529))
  
  repeat {
    status <- req_perform(status_req) |> resp_body_json()
    message("Batch ", batch_id, " status: ", status$processing_status,
            " | succeeded: ", status$request_counts$succeeded %||% 0,
            " | errored: ", status$request_counts$errored %||% 0)
    if (status$processing_status == "ended") return(status)
    Sys.sleep(poll_interval)
  }
}

parse_batch_result_line <- function(line) {
  parsed <- fromJSON(line, simplifyVector = FALSE)
  custom_id <- parsed$custom_id
  base <- list(custom_id = custom_id, sentiment = NA, confidence_score = NA)
  
  if (parsed$result$type != "succeeded") {
    message("Batch request failed for ", custom_id, " | type: ", parsed$result$type,
            " | ", parsed$result$error$message %||% "no error message")
    return(base)
  }
  
  tryCatch({
    result <- extract_sentiment_json(parsed$result$message$content)
    
    if (is.null(result)) {
      message("No text block for ", custom_id)
      return(base)
    }
    
    list(custom_id = custom_id, sentiment = result$sentiment, confidence_score = result$confidence_score)
    
  }, error = function(e) {
    message("Parse error for ", custom_id, " | ", conditionMessage(e))
    base
  })
}

fetch_batch_results <- function(results_url) {
  results_req <- request(results_url) |>
    req_headers("x-api-key" = Sys.getenv("CLAUDE_API_KEY"), "anthropic-version" = "2023-06-01") |>
    req_retry(max_tries = 4, backoff = ~ 2 ^ .x,
              is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503, 529))
  
  raw_lines <- req_perform(results_req) |> resp_body_string() |> str_split("\n") |> pluck(1)
  raw_lines <- raw_lines[nzchar(raw_lines)]
  
  map(raw_lines, parse_batch_result_line)
}


## Wrapper Batch
analyze_text_claude_batch <- function(df, text_col, id_col = NULL, 
                                      output_name, model = "claude-sonnet-5") {
  
  generated_id <- is.null(rlang::enexpr(id_col))
  
  if (generated_id) {
    df <- df |> mutate(.row_id = row_number())
    id_col_sym <- rlang::sym(".row_id")
    id_is_integer <- TRUE
  } else {
    id_col_sym <- rlang::ensym(id_col)
    id_is_integer <- is.numeric(pull(df, !!id_col_sym))
  }
  
  requests <- build_batch_requests(df, {{ text_col }}, !!id_col_sym, model = model)
  batch_id <- submit_batch(requests)
  
  status <- poll_batch(batch_id)
  results_list <- fetch_batch_results(status$results_url)
  
  extracted_id <- map_chr(results_list, ~ str_remove(.x$custom_id, "^row_"))
  
  results_df <- map_dfr(results_list, as_tibble) |>
    mutate(!!rlang::as_name(id_col_sym) := if (id_is_integer) as.integer(extracted_id) else extracted_id) |>
    select(-custom_id) |> 
    arrange(!!id_col_sym)   # explicit sort — never trust join/batch return order
  
  output_path <- paste0(output_name, "_", str_remove(model, "^claude-"), ".rds")
  saveRDS(results_df, output_path)
  message("Batch results saved to: ", output_path)
  
  out <- df |> 
    left_join(results_df, by = rlang::as_name(id_col_sym)) |> 
    arrange(!!id_col_sym)
  
  if (generated_id) out <- out |> select(-.row_id)
  
  out
}


# Wrapper functions

# 1. analyze_text_local(df, text_col, model = "llama3.2:latest")

# 2. analyze_text_local(text_df, text_col, model = "claude-sonnet-5")

# 3. analyze_text_claude_batch(df, text_col, id_col = NULL, output_name, model = "claude-sonnet-5")


