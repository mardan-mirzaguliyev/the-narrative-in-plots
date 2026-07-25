source("00-shared_objects.R")


library(httr2)       # request(), req_headers(), req_body_json(), req_retry(), req_perform()
library(jsonlite)    # fromJSON()
library(dplyr)
library(purrr)        # map(), map_dfr(), map_chr(), pmap()
library(tidyr)         # unnest_wider()
library(stringr)      # str_remove(), str_split()
library(rlang)          # enexpr(), ensym(), as_name(), sym(), %||% — used throughout the id_col generalization



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
    
    parsed <- extract_json_response(resp_body$content)
    
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
    result <- extract_json_response(parsed$result$message$content)
    
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





