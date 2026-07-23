# ==============================================================================
# Shared sentiment-analysis pipeline: local Ollama LLM + Claude API
# Supports dynamic label sets (e.g. matching a Hugging Face classifier's own
# vocabulary) on top of the default 9-label schema. No HF inference in this
# version — get_model_labels() only reads a model's config, it doesn't run it.
# ==============================================================================

library(httr2)
library(purrr)
library(jsonlite)
library(tidyverse)
library(pdftools)
library(stringr)
library(tibble)
library(rlang)
library(glue)
library(digest)

library(reticulate)
use_condaenv("C:/Users/Mardan/miniconda3/", required = TRUE)


# ============================================================================
# Shared objects: label vocabulary, colors, prompts, JSON handler
# ============================================================================

# Canonical default label vocabulary (9-label NRC-style schema). Single
# source of truth for factor levels, complete()/join keys, the default
# prompt's enum, and the color mapping.
sentiment_levels <- c("Joy", "Trust", "Anticipation", "Neutral",
                      "Surprise", "Fear", "Anger", "Disgust", "Sadness")

sentiment_colors <- c(
  "Joy"          = "#2a9d8f",
  "Trust"        = "#52b9ac",
  "Anticipation" = "#8ecfc4",
  "Neutral"      = "#cbe8f5",
  "Surprise"     = "#e9c46a",
  "Fear"         = "#e76f51",
  "Anger"        = "#bd1515",
  "Disgust"      = "#8c1c13",
  "Sadness"      = "#6b7b8c"
)

# Fixed default prompt — used whenever labels == sentiment_levels, so the
# original hand-tuned wording/examples are preserved for the common case.
sentiment_system_prompt <- '
You are a professional sentiment analysis engine for literary text. Analyze the emotional tone of the provided text and output a single JSON object matching this exact schema:
{
  "sentiment": string,   // exactly one of: "Trust" | "Fear" | "Sadness" | "Anger" | "Surprise" | "Disgust" | "Joy" | "Anticipation" | "Neutral"
  "confidence_score": number,  // 0.0-1.0, your certainty that this is the single best-fitting label (not the intensity of the emotion)
  "reasoning": string    // short phrase explaining why this label was chosen
}
Rules:
1. Output ONLY the raw JSON object. No markdown code fences, no backticks, no preamble, no explanation, no trailing text.
2. Choose exactly one sentiment label, even when the text expresses mixed emotions - pick the single most dominant one.
3. If the text is ambiguous or blended, still choose the most probable label, but assign confidence_score below 0.5.
4. If the input is empty, whitespace-only, or nonsensical (not natural language), output {"sentiment": "Neutral", "confidence_score": 0.0, "reasoning": "empty or nonsensical input"}.
5. Base your judgment on the emotional content of the text itself, not on real-world factual accuracy or your own opinion of the content.
6. Do not add, remove, or rename JSON keys. Do not wrap the object in an array.
Examples:
Input: "The old house creaked, and she felt the walls closing in around her."
Output: {"sentiment": "Fear", "confidence_score": 0.82, "reasoning": "imagery of an unsafe, threatening space"}
Input: "asdkj 12341 !!!"
Output: {"sentiment": "Neutral", "confidence_score": 0.0, "reasoning": "not natural language"}'

## Builds a system prompt for an arbitrary label set (e.g. pulled from a
## Hugging Face classifier's own config via get_model_labels()).
build_dynamic_sentiment_prompt <- function(labels) {
  labels_enum <- paste(sprintf('"%s"', labels), collapse = " | ")
  
  glue('
You are a professional sentiment analysis engine for literary text. Analyze the emotional tone of the provided text and output a single JSON object matching this exact schema:
{{
  "sentiment": string,   // exactly one of: {labels_enum}
  "confidence_score": number,  // 0.0-1.0, your certainty that this is the single best-fitting label (not the intensity of the emotion)
  "reasoning": string    // short phrase explaining why this label was chosen
}}
Rules:
1. Output ONLY the raw JSON object. No markdown code fences, no backticks, no preamble, no explanation, no trailing text.
2. Choose exactly one label from the list above, even when the text expresses mixed emotions - pick the single most dominant one.
3. If the text is ambiguous or blended, still choose the most probable label, but assign confidence_score below 0.5.
4. If the input is empty, whitespace-only, or nonsensical, choose whichever label above best represents a neutral/baseline state, and set confidence_score to 0.0.
5. Base your judgment on the emotional content of the text itself, not on real-world factual accuracy or your own opinion of the content.
6. Never output a label that is not in the list above. Do not add, remove, or rename JSON keys. Do not wrap the object in an array.
') |> as.character()
}

## Resolves the right prompt for a given label set — default hand-tuned
## prompt for the standard 9 labels, dynamically generated otherwise.
get_sentiment_system_prompt <- function(labels) {
  if (identical(labels, sentiment_levels)) sentiment_system_prompt
  else build_dynamic_sentiment_prompt(labels)
}

## Short, stable cache-key fragment for a label set — "default" for the
## standard schema, an 8-char hash otherwise, so get_or_run_*() never
## conflates results scored against two different vocabularies.
labels_tag_for <- function(labels) {
  if (identical(labels, sentiment_levels)) "default"
  else substr(digest(labels), 1, 8)
}

## Pulls a Hugging Face model's label vocabulary straight from its config —
## no inference call, just reads id2label. Use this to align a local LLM's
## dynamic prompt to a classifier's own labels (e.g. CEDR, cardiffnlp).
get_model_labels <- function(model_id) {
  hf_hub <- import("huggingface_hub")
  config_path <- hf_hub$hf_hub_download(repo_id = model_id, filename = "config.json")
  config <- jsonlite::fromJSON(config_path)
  unname(unlist(config$id2label))
}

## Pulls sentiment JSON out of a `content` block list.
## Used by score_claude_synch() (sync) and parse_batch_result_line() (batch).
extract_sentiment_json <- function(content_blocks) {
  text_blocks <- Filter(function(b) b$type == "text", content_blocks)
  if (length(text_blocks) == 0) return(NULL)
  
  content_text <- text_blocks[[1]]$text
  clean_json <- gsub("(?s).*(\\{.*\\}).*", "\\1", content_text, perl = TRUE) |> trimws()
  
  fromJSON(clean_json)
}


# ============================================================================
# Local LLM (Ollama)
# ============================================================================

score_local_llm <- function(text_col, model = "llama3.2:latest", labels = sentiment_levels) {
  
  system_prompt <- get_sentiment_system_prompt(labels)
  
  json_schema <- list(
    type = "object",
    properties = list(
      sentiment        = list(type = "string", enum = labels),
      confidence_score = list(type = "number"),
      reasoning        = list(type = "string")
    ),
    required = list("sentiment", "confidence_score", "reasoning")
  )
  
  tryCatch({
    
    res <- generate(
      model  = model,
      prompt = text_col,
      system = system_prompt,
      format = json_schema,
      output = "text",
      temperature = 0
    )
    
    parsed <- fromJSON(res)
    
    if (is.null(parsed$sentiment) || !parsed$sentiment %in% labels) {
      message("Invalid/missing sentiment label for input: ",
              substr(text_col, 1, 60), " | got: ", parsed$sentiment %||% "NULL")
      parsed$sentiment <- NA_character_
    }
    
    if (is.null(parsed$confidence_score) || !is.numeric(parsed$confidence_score)) {
      parsed$confidence_score <- 0.0
    } else {
      parsed$confidence_score <- pmin(pmax(parsed$confidence_score, 0), 1)  # clamp to [0,1]
    }
    
    if (is.null(parsed$reasoning)) parsed$reasoning <- NA_character_
    
    return(parsed)
    
  }, error = function(e) {
    message("Local LLM call failed for input: ", substr(text_col, 1, 60),
            " | ", conditionMessage(e))
    return(list(sentiment = NA_character_, confidence_score = 0.0, reasoning = NA_character_))
  })
}

## Synchronous wrapper for local scoring — mirrors analyze_text_claude_synch()
analyze_text_local <- function(df, text_col, model = "llama3.2:latest", labels = sentiment_levels) {
  df |>
    mutate(sentiment_data = map({{ text_col }}, ~score_local_llm(.x, model = model, labels = labels))) |>
    unnest_wider(sentiment_data)
}

## Cache-aware orchestrator. Cache key now includes both model and label set,
## so scoring the same model against two different vocabularies (e.g. the
## default schema, then a CEDR-matched schema) never collides in the cache.
get_or_run_local <- function(df, text_col, output_name, model = "llama3.2:latest",
                             labels = sentiment_levels) {
  
  model_tag  <- str_replace_all(model, "[:/]", "-")
  labels_tag <- labels_tag_for(labels)
  expected_path <- paste0(output_name, "_", model_tag, "_", labels_tag, ".rds")
  
  if (file.exists(expected_path)) {
    message("Found existing results at: ", expected_path, " — loading from disk.")
    readRDS(expected_path)
    
  } else {
    message("No existing results found at: ", expected_path, " — running local scoring.")
    result <- analyze_text_local(df, {{ text_col }}, model = model, labels = labels)
    
    saveRDS(result, expected_path)
    message("Results saved to: ", expected_path)
    
    result
  }
}


# ============================================================================
# Claude API
# ============================================================================

# NOTE: no bare Sys.getenv("CLAUDE_API_KEY") call here — unassigned, it
# echoes your key to console/log on every source(). Sanity-check instead with:
#   stopifnot(nzchar(Sys.getenv("CLAUDE_API_KEY")))

## Get the list of Claude models available. Call manually when you want it —
## not fired automatically on source(), since it's a live network call.
get_clean_model_list <- function() {
  req <- request("https://api.anthropic.com/v1/models") |>
    req_headers(
      "x-api-key" = Sys.getenv("CLAUDE_API_KEY"),
      "anthropic-version" = "2023-06-01"
    )
  
  resp <- req_perform(req)
  data <- resp_body_json(resp)$data
  
  map_dfr(data, ~tibble(
    id = .x$id,
    display_name = .x$display_name,
    created_at = .x$created_at
  ))
}


## ================= SYNCHRONOUS PATH =================

score_claude_synch <- function(text_input, model = "claude-sonnet-5", labels = sentiment_levels) {
  
  system_prompt <- get_sentiment_system_prompt(labels)
  
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
      system = system_prompt,
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
      return(list(sentiment = NA, confidence_score = NA, reasoning = NA_character_))
    }
    
    if (!parsed$sentiment %in% labels) {
      message("Sentiment outside provided label set for input: ",
              substr(text_input, 1, 60), " | got: ", parsed$sentiment)
      parsed$sentiment <- NA_character_
    }
    
    parsed
    
  }, httr2_http = function(e) {
    status <- tryCatch(resp_status(e$resp), error = function(e2) NA)
    body_msg <- tryCatch(resp_body_json(e$resp)$error$message, error = function(e2) conditionMessage(e))
    message("HTTP error for input: ", substr(text_input, 1, 60), " | status: ", status, " | ", body_msg)
    list(sentiment = NA, confidence_score = NA, reasoning = NA_character_)
    
  }, error = function(e) {
    message("Parse/other error for input: ", substr(text_input, 1, 60), " | ", conditionMessage(e))
    list(sentiment = NA, confidence_score = NA, reasoning = NA_character_)
  })
}

analyze_text_claude_synch <- function(text_df, text_col, model = "claude-sonnet-5", labels = sentiment_levels) {
  text_df |>
    mutate(sentiment_data = map({{ text_col }}, ~score_claude_synch(.x, model = model, labels = labels))) |>
    unnest_wider(sentiment_data)
}

## Cache-aware orchestrator for the synchronous path.
get_or_run_claude_synch <- function(df, text_col, output_name, model = "claude-sonnet-5",
                                    labels = sentiment_levels) {
  
  labels_tag <- labels_tag_for(labels)
  expected_path <- paste0(output_name, "_", str_remove(model, "^claude-"), "_", labels_tag, "_synch.rds")
  
  if (file.exists(expected_path)) {
    message("Found existing results at: ", expected_path, " — loading from disk.")
    readRDS(expected_path)
    
  } else {
    message("No existing results found at: ", expected_path, " — running synchronous scoring.")
    result <- analyze_text_claude_synch(df, {{ text_col }}, model = model, labels = labels)
    
    saveRDS(result, expected_path)
    message("Results saved to: ", expected_path)
    
    result
  }
}


## ================= BATCH PATH =================

build_batch_requests <- function(df, text_col, id_col, model = "claude-sonnet-5", labels = sentiment_levels) {
  system_prompt <- get_sentiment_system_prompt(labels)
  
  pmap(list(pull(df, {{ id_col }}), pull(df, {{ text_col }})), function(id, txt) {
    list(
      custom_id = paste0("row_", id),
      params = list(
        model = model,
        max_tokens = 200,
        thinking = list(type = "disabled"),
        system = list(list(
          type = "text",
          text = system_prompt,
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

parse_batch_result_line <- function(line, labels = sentiment_levels) {
  parsed <- fromJSON(line, simplifyVector = FALSE)
  custom_id <- parsed$custom_id
  base <- list(custom_id = custom_id, sentiment = NA, confidence_score = NA, reasoning = NA_character_)
  
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
    
    if (!is.null(result$sentiment) && !result$sentiment %in% labels) {
      message("Sentiment outside provided label set for ", custom_id, " | got: ", result$sentiment)
      result$sentiment <- NA_character_
    }
    
    list(custom_id = custom_id,
         sentiment = result$sentiment,
         confidence_score = result$confidence_score,
         reasoning = result$reasoning)
    
  }, error = function(e) {
    message("Parse error for ", custom_id, " | ", conditionMessage(e))
    base
  })
}

fetch_batch_results <- function(results_url, labels = sentiment_levels) {
  results_req <- request(results_url) |>
    req_headers("x-api-key" = Sys.getenv("CLAUDE_API_KEY"), "anthropic-version" = "2023-06-01") |>
    req_retry(max_tries = 4, backoff = ~ 2 ^ .x,
              is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503, 529))
  
  raw_lines <- req_perform(results_req) |> resp_body_string() |> str_split("\n") |> pluck(1)
  raw_lines <- raw_lines[nzchar(raw_lines)]
  
  map(raw_lines, parse_batch_result_line, labels = labels)
}

## Runs a batch job and saves results to `save_path` if given (used by
## get_or_run_claude_batch() so both functions agree on the exact same
## cache filename — no risk of the two computing it differently).
analyze_text_claude_batch <- function(df, text_col, id_col = NULL, output_name,
                                      model = "claude-sonnet-5", labels = sentiment_levels,
                                      save_path = NULL) {
  
  generated_id <- is.null(rlang::enexpr(id_col))
  
  if (generated_id) {
    df <- df |> mutate(.row_id = row_number())
    id_col_sym <- rlang::sym(".row_id")
    id_is_integer <- TRUE
  } else {
    id_col_sym <- rlang::ensym(id_col)
    id_is_integer <- is.numeric(pull(df, !!id_col_sym))
  }
  
  requests <- build_batch_requests(df, {{ text_col }}, !!id_col_sym, model = model, labels = labels)
  batch_id <- submit_batch(requests)
  
  status <- poll_batch(batch_id)
  results_list <- fetch_batch_results(status$results_url, labels = labels)
  
  extracted_id <- map_chr(results_list, ~ str_remove(.x$custom_id, "^row_"))
  
  results_df <- map_dfr(results_list, as_tibble) |>
    mutate(!!rlang::as_name(id_col_sym) := if (id_is_integer) as.integer(extracted_id) else extracted_id) |>
    select(-custom_id) |>
    arrange(!!id_col_sym)   # explicit sort — never trust join/batch return order
  
  if (is.null(save_path)) {
    save_path <- paste0(output_name, "_", str_remove(model, "^claude-"), "_",
                        labels_tag_for(labels), "_batch.rds")
  }
  saveRDS(results_df, save_path)
  message("Batch results saved to: ", save_path)
  
  out <- df |>
    left_join(results_df, by = rlang::as_name(id_col_sym)) |>
    arrange(!!id_col_sym)
  
  if (generated_id) out <- out |> select(-.row_id)
  
  out
}

## Cache-aware orchestrator for the batch path. Computes the cache path once
## and passes it into analyze_text_claude_batch() as save_path, so the two
## functions can never disagree about where results live.
get_or_run_claude_batch <- function(df, text_col, id_col = NULL, output_name,
                                    model = "claude-sonnet-5", labels = sentiment_levels) {
  
  expected_path <- paste0(output_name, "_", str_remove(model, "^claude-"), "_",
                          labels_tag_for(labels), "_batch.rds")
  
  generated_id <- is.null(rlang::enexpr(id_col))
  
  if (generated_id) {
    df <- df |> mutate(.row_id = row_number())
    id_col_sym <- rlang::sym(".row_id")
  } else {
    id_col_sym <- rlang::ensym(id_col)
  }
  
  if (file.exists(expected_path)) {
    message("Found existing results at: ", expected_path, " — loading from disk.")
    results_df <- readRDS(expected_path)
    
    out <- df |>
      left_join(results_df, by = rlang::as_name(id_col_sym)) |>
      arrange(!!id_col_sym)
    
  } else {
    message("No existing results found at: ", expected_path, " — running batch.")
    out <- analyze_text_claude_batch(df, {{ text_col }}, !!id_col_sym,
                                     output_name = output_name, model = model,
                                     labels = labels, save_path = expected_path)
  }
  
  if (generated_id) out <- out |> select(-.row_id)
  
  out
}



# ============================================================================
# Hugging Face
# ============================================================================
# Two distinct modes, since "Hugging Face" covers two different kinds of
# models here:
#   (a) Fixed-vocabulary classifiers (e.g. cardiffnlp/twitter-roberta-*) —
#       score_hf_inference() / analyze_text_hf() / get_or_run_hf(). No prompt,
#       no dynamic labels — the model dictates its own label set.
#   (b) HF-hosted instruct/chat models — score_hf_llm() / analyze_text_hf_llm()
#       / get_or_run_hf_llm(). Same dynamic-label mechanism as the local and
#       Claude LLM paths, via get_sentiment_system_prompt().

## ================= SETUP =================

hf_hub <- import("huggingface_hub")

hf_token <- Sys.getenv("HF_TOKEN")
if (hf_token == "") {
  stop("Error: HF_TOKEN not found! Please check your .Renviron file.")
}

hf_client <- hf_hub$InferenceClient(
  provider = "hf-inference",
  api_key = hf_token
)

## ================= (a) FIXED-VOCABULARY CLASSIFIERS =================

score_hf_inference <- function(text_input,
                               model = "cardiffnlp/twitter-roberta-base-emotion-multilabel-latest",
                               max_tries = 3, retry_delay = 5) {
  
  attempt <- 1
  
  repeat {
    result <- tryCatch({
      res <- hf_client$text_classification(
        text = text_input,
        model = model,
        top_k = 1L
      )
      
      list(sentiment = str_to_title(res[[1]]$label),
           confidence_score = as.numeric(res[[1]]$score),
           reasoning = NA_character_)  # classifiers don't generate rationale —
      # kept for schema parity with the LLM paths
      
    }, error = function(e) {
      # HF serverless models can return a transient error while "cold starting" —
      # worth distinguishing that from a genuine failure (or from the model
      # simply not being deployed on this provider at all — see the model's
      # Hub page / hf-inference support before assuming it's transient).
      is_loading <- str_detect(conditionMessage(e), regex("503|loading|currently loading", ignore_case = TRUE))
      list(error = TRUE, is_loading = is_loading, message = conditionMessage(e))
    })
    
    if (is.null(result$error)) return(result)  # success
    
    if (result$is_loading && attempt < max_tries) {
      message("Model loading, retrying (", attempt, "/", max_tries, ") for: ", substr(text_input, 1, 60))
      Sys.sleep(retry_delay)
      attempt <- attempt + 1
      next
    }
    
    message("Failed for input: ", substr(text_input, 1, 60), " | ", result$message)
    return(list(sentiment = NA, confidence_score = NA, reasoning = NA_character_))
  }
}

analyze_text_hf <- function(df, text_col,
                            model = "cardiffnlp/twitter-roberta-base-emotion-multilabel-latest") {
  df |>
    mutate(sentiment_data = map({{ text_col }}, ~score_hf_inference(.x, model = model))) |>
    unnest_wider(sentiment_data)
}

## Cache-aware orchestrator. No labels_tag here — a classifier's label set
## is fixed to the model itself, so model_tag alone is a sufficient cache key.
get_or_run_hf <- function(df, text_col, output_name,
                          model = "cardiffnlp/twitter-roberta-base-emotion-multilabel-latest") {
  
  model_tag <- str_replace_all(model, "[:/]", "-")
  expected_path <- paste0(output_name, "_", model_tag, ".rds")
  
  if (file.exists(expected_path)) {
    message("Found existing results at: ", expected_path, " — loading from disk.")
    readRDS(expected_path)
    
  } else {
    message("No existing results found at: ", expected_path, " — running HF scoring.")
    result <- analyze_text_hf(df, {{ text_col }}, model = model)
    
    saveRDS(result, expected_path)
    message("Results saved to: ", expected_path)
    
    result
  }
}

## ================= (b) HF-HOSTED INSTRUCT/CHAT MODELS (dynamic labels) =====

score_hf_llm <- function(text_input, model, labels = sentiment_levels) {
  
  system_prompt <- get_sentiment_system_prompt(labels)
  
  tryCatch({
    resp <- hf_client$chat_completion(
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = text_input)
      ),
      model = model,
      max_tokens = 200,
      temperature = 0
    )
    
    content_text <- resp$choices[[1]]$message$content
    
    # Reuse the shared extractor by wrapping the plain-text reply in the
    # same content-block shape Claude's API returns.
    parsed <- extract_sentiment_json(list(list(type = "text", text = content_text)))
    
    if (is.null(parsed) || is.null(parsed$sentiment) || !parsed$sentiment %in% labels) {
      message("Invalid/missing sentiment label for input: ", substr(text_input, 1, 60))
      return(list(sentiment = NA_character_, confidence_score = NA_real_, reasoning = NA_character_))
    }
    
    if (is.null(parsed$reasoning)) parsed$reasoning <- NA_character_
    
    parsed
    
  }, error = function(e) {
    message("HF LLM call failed for input: ", substr(text_input, 1, 60), " | ", conditionMessage(e))
    list(sentiment = NA_character_, confidence_score = NA_real_, reasoning = NA_character_)
  })
}

analyze_text_hf_llm <- function(df, text_col, model, labels = sentiment_levels) {
  df |>
    mutate(sentiment_data = map({{ text_col }}, ~score_hf_llm(.x, model = model, labels = labels))) |>
    unnest_wider(sentiment_data)
}

## Cache-aware orchestrator — labels_tag included, same reasoning as the
## local/Claude paths: this model's prompt (and therefore its output) depends
## on which label set it was constrained to.
get_or_run_hf_llm <- function(df, text_col, output_name, model, labels = sentiment_levels) {
  
  model_tag  <- str_replace_all(model, "[:/]", "-")
  labels_tag <- labels_tag_for(labels)
  expected_path <- paste0(output_name, "_", model_tag, "_", labels_tag, "_hfllm.rds")
  
  if (file.exists(expected_path)) {
    message("Found existing results at: ", expected_path, " — loading from disk.")
    readRDS(expected_path)
    
  } else {
    message("No existing results found at: ", expected_path, " — running HF LLM scoring.")
    result <- analyze_text_hf_llm(df, {{ text_col }}, model = model, labels = labels)
    
    saveRDS(result, expected_path)
    message("Results saved to: ", expected_path)
    
    result
  }
}

# ============================================================================
# Wrapper functions reference
# ============================================================================
# Analysis functions (return a scored df directly, no caching):
# 1. analyze_text_local(df, text_col, model = "llama3.2:latest", labels = sentiment_levels)
# 2. analyze_text_claude_synch(text_df, text_col, model = "claude-sonnet-5", labels = sentiment_levels)
# 3. analyze_text_claude_batch(df, text_col, id_col = NULL, output_name, model = "claude-sonnet-5", labels = sentiment_levels)
# 4. analyze_text_hf(df, text_col, model = "cardiffnlp/twitter-roberta-base-emotion-multilabel-latest")
# 5. analyze_text_hf_llm(df, text_col, model, labels = sentiment_levels)
#
# Cache-aware orchestrators (use these day to day):
# 1. get_or_run_local(df, text_col, output_name, model = "llama3.2:latest", labels = sentiment_levels)
# 2. get_or_run_claude_synch(df, text_col, output_name, model = "claude-sonnet-5", labels = sentiment_levels)
# 3. get_or_run_claude_batch(df, text_col, id_col = NULL, output_name, model = "claude-sonnet-5", labels = sentiment_levels)
# 4. get_or_run_hf(df, text_col, output_name, model = "cardiffnlp/twitter-roberta-base-emotion-multilabel-latest")
# 5. get_or_run_hf_llm(df, text_col, output_name, model, labels = sentiment_levels)
#
# Dynamic label workflow (align a local/Claude/HF-LLM to a HF classifier's vocab):
#   target_labels <- get_model_labels("Aniemore/rubert-base-emotion-russian-cedr-m7")
#   get_or_run_local(df, text, output_name = "changes_kino", labels = target_labels)
#   get_or_run_claude_synch(df, text, output_name = "changes_kino", labels = target_labels)


