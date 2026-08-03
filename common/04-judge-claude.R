library(httr2)       # request(), req_headers(), req_body_json(), req_retry(), req_perform()
library(jsonlite)    # fromJSON()
library(dplyr)
library(purrr)        # pmap() for the two-column (text + proposed label) call
library(tidyr)         # unnest_wider()
library(stringr)      # str_remove()
library(rlang)          # %||%


# ============================================================================
# Claude Judging
# ============================================================================
# NOTE: judge_system_prompt / get_judge_system_prompt(labels) live in
# 00-shared_objects.R, alongside the other prompt/schema pairs. Judges
# whatever categorical label another method (NRC, Gemma4, Sonnet5, etc.)
# assigned to a piece of text, and either confirms it or proposes a
# correction with reasoning.

score_judge_claude <- function(text_input, proposed_sentiment, model = "claude-sonnet-5",
                               labels = NULL, system_prompt = NULL) {
  
  if (is.null(system_prompt)) {
    if (is.null(labels)) {
      stop("Must supply `labels` (for the default label-driven judge prompt) ",
           "or an explicit `system_prompt`.")
    }
    system_prompt <- get_judge_system_prompt(labels)
  }
  
  user_prompt <- paste0(
    'Text: "', text_input, '"\n',
    'Assigned label: "', proposed_sentiment, '"'
  )
  
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
      messages = list(list(role = "user", content = user_prompt))
    )) |> 
    req_retry(
      max_tries = 4,
      backoff = ~ 2 ^ .x,
      is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503, 529)
    )
  
  fallback <- list(agree = NA, corrected_sentiment = NA_character_, reasoning = NA_character_)
  
  tryCatch({
    
    resp <- req_perform(req)
    resp_body <- resp_body_json(resp)
    
    parsed <- extract_json_response(resp_body$content)  # schema-agnostic — just extracts + parses JSON
    
    if (is.null(parsed)) {
      message("No text block returned for input: ", substr(text_input, 1, 60), 
              " | stop_reason: ", resp_body$stop_reason %||% "unknown")
      return(fallback)
    }
    
    if (!is.null(labels) && !is.null(parsed$corrected_sentiment) && !parsed$corrected_sentiment %in% labels) {
      message("corrected_sentiment outside provided label set for input: ",
              substr(text_input, 1, 60), " | got: ", parsed$corrected_sentiment)
      parsed$corrected_sentiment <- NA_character_
    }
    
    parsed
    
  }, httr2_http = function(e) {
    status <- tryCatch(resp_status(e$resp), error = function(e2) NA)
    body_msg <- tryCatch(resp_body_json(e$resp)$error$message, error = function(e2) conditionMessage(e))
    message("HTTP error for input: ", substr(text_input, 1, 60), " | status: ", status, " | ", body_msg)
    fallback
    
  }, error = function(e) {
    message("Parse/other error for input: ", substr(text_input, 1, 60), " | ", conditionMessage(e))
    fallback
  })
}

analyze_judge_claude <- function(df, text_col, sentiment_col, model = "claude-sonnet-5",
                                 labels = NULL, system_prompt = NULL) {
  df |>
    mutate(judge_data = pmap(
      list({{ text_col }}, {{ sentiment_col }}),
      function(txt, sent) score_judge_claude(txt, sent, model = model, labels = labels, system_prompt = system_prompt)
    )) |> 
    unnest_wider(judge_data)
}

## Cache-aware orchestrator. Cache key includes model AND label set (or
## "nolabels" when a fully custom system_prompt was supplied instead), so
## judging the same text/label pairs against different schemas never
## collides in the cache.
get_or_run_judge_claude <- function(df, text_col, sentiment_col, output_name, model = "claude-sonnet-5",
                                    labels = NULL, system_prompt = NULL) {
  
  model_tag <- str_remove(model, "^claude-")
  labels_tag <- if (is.null(labels)) "nolabels" else labels_tag_for(labels)
  expected_path <- paste0(output_name, "_judge_", model_tag, "_", labels_tag, ".rds")
  
  if (file.exists(expected_path)) {
    message("Found existing results at: ", expected_path, " — loading from disk.")
    readRDS(expected_path)
    
  } else {
    message("No existing results found at: ", expected_path, " — running judge scoring.")
    result <- analyze_judge_claude(df, {{ text_col }}, {{ sentiment_col }}, model = model,
                                   labels = labels, system_prompt = system_prompt)
    
    saveRDS(result, expected_path)
    message("Results saved to: ", expected_path)
    
    result
  }
}


