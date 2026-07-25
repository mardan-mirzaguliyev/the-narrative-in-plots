source("00-shared_objects.R")


library(ollamar)
library(jsonlite)
library(dplyr)
library(purrr)       # pmap() specifically, since judging takes two columns (text + proposed label)
library(tidyr)
library(stringr)
library(rlang)        # %||%, enexpr/ensym if you generalized this wrapper the same way as the Claude batch functions


score_judge_local <- function(text_input, proposed_sentiment, model = "llama3.2:latest") {
  
  valid_labels <- c("Trust", "Fear", "Sadness", "Anger", "Surprise", 
                    "Disgust", "Joy", "Anticipation", "Neutral")
  
  json_schema <- list(
    type = "object",
    properties = list(
      agree = list(type = "boolean"),
      corrected_sentiment = list(type = "string", enum = valid_labels),
      reasoning = list(type = "string")
    ),
    required = list("agree", "corrected_sentiment", "reasoning")
  )
  
  user_prompt <- paste0(
    'Text: "', text_input, '"\n',
    'Assigned label: "', proposed_sentiment, '"'
  )
  
  tryCatch({
    
    res <- generate(
      model = model,
      prompt = user_prompt,
      system = judge_system_prompt,
      format = json_schema,
      output = "text",
      temperature = 0
    )
    
    parsed <- fromJSON(res)
    
    if (is.null(parsed$corrected_sentiment) || !parsed$corrected_sentiment %in% valid_labels) {
      message("Invalid corrected_sentiment for text: ", substr(text_input, 1, 60), 
              " | got: ", parsed$corrected_sentiment %||% "NULL")
      parsed$corrected_sentiment <- proposed_sentiment
      parsed$agree <- TRUE
    }
    
    list(agree = parsed$agree, 
         corrected_sentiment = parsed$corrected_sentiment, 
         reasoning = parsed$reasoning)
    
  }, error = function(e) {
    message("Judge call failed for text: ", substr(text_input, 1, 60), " | ", conditionMessage(e))
    list(agree = NA, corrected_sentiment = NA, reasoning = NA)
  })
}


analyze_judge_local <- function(df, text_col, sentiment_col, model = "llama3.2:latest") {
  df |>
    mutate(judge_data = pmap(
      list({{ text_col }}, {{ sentiment_col }}),
      function(txt, sent) score_judge_local(txt, sent, model = model)
    )) |> 
    unnest_wider(judge_data)
}


get_or_run_judge_local <- function(df, text_col, sentiment_col, output_name, model = "llama3.2:latest") {
  
  model_tag <- str_replace_all(model, "[:/]", "-")
  expected_path <- paste0(output_name, "_judge_", model_tag, ".rds")
  
  if (file.exists(expected_path)) {
    message("Found existing results at: ", expected_path, " — loading from disk.")
    readRDS(expected_path)
    
  } else {
    message("No existing results found at: ", expected_path, " — running judge scoring.")
    result <- analyze_judge_local(df, {{ text_col }}, {{ sentiment_col }}, model = model)
    
    saveRDS(result, expected_path)
    message("Results saved to: ", expected_path)
    
    result
  }
}

