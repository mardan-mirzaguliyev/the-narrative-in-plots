source("00-shared_objects.R")

library(ollamar)    # generate() — the actual model call
library(jsonlite)   # fromJSON() — parsing the model's JSON output
library(dplyr)       # mutate(), pull()
library(purrr)       # map()
library(tidyr)       # unnest_wider()
library(stringr)     # str_replace_all() for filename sanitizing


# Local LLM (Ollama)
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



