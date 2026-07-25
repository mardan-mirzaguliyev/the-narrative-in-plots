source("00-shared_objects.R")


library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)       # pmap() for the two-column (text + proposed label) call
library(tidyr)
library(stringr)
library(rlang)         # %||%


score_judge_claude <- function(text_input, proposed_sentiment, model = "claude-sonnet-5") {
  
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
      system = judge_system_prompt,
      messages = list(list(role = "user", content = user_prompt))
    )) |> 
    req_retry(
      max_tries = 4,
      backoff = ~ 2 ^ .x,
      is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503, 529)
    )
  
  tryCatch({
    
    resp <- req_perform(req)
    resp_body <- resp_body_json(resp)
    
    parsed <- extract_json_response(resp_body$content)  # schema-agnostic — just extracts + parses JSON
    
    if (is.null(parsed)) {
      message("No text block returned for input: ", substr(text_input, 1, 60), 
              " | stop_reason: ", resp_body$stop_reason %||% "unknown")
      return(list(agree = NA, corrected_sentiment = NA, reasoning = NA))
    }
    
    parsed
    
  }, httr2_http = function(e) {
    status <- tryCatch(resp_status(e$resp), error = function(e2) NA)
    body_msg <- tryCatch(resp_body_json(e$resp)$error$message, error = function(e2) conditionMessage(e))
    message("HTTP error for input: ", substr(text_input, 1, 60), " | status: ", status, " | ", body_msg)
    list(agree = NA, corrected_sentiment = NA, reasoning = NA)
    
  }, error = function(e) {
    message("Parse/other error for input: ", substr(text_input, 1, 60), " | ", conditionMessage(e))
    list(agree = NA, corrected_sentiment = NA, reasoning = NA)
  })
}


analyze_judge_claude <- function(df, text_col, sentiment_col, model = "claude-sonnet-5") {
  df |>
    mutate(judge_data = pmap(
      list({{ text_col }}, {{ sentiment_col }}),
      function(txt, sent) score_judge_claude(txt, sent, model = model)
    )) |> 
    unnest_wider(judge_data)
}

get_or_run_judge_claude <- function(df, text_col, sentiment_col, output_name, model = "claude-sonnet-5") {
  
  model_tag <- str_remove(model, "^claude-")
  expected_path <- paste0(output_name, "_judge_", model_tag, ".rds")
  
  if (file.exists(expected_path)) {
    message("Found existing results at: ", expected_path, " — loading from disk.")
    readRDS(expected_path)
    
  } else {
    message("No existing results found at: ", expected_path, " — running judge scoring.")
    result <- analyze_judge_claude(df, {{ text_col }}, {{ sentiment_col }}, model = model)
    
    saveRDS(result, expected_path)
    message("Results saved to: ", expected_path)
    
    result
  }
}




