source("00-shared_objects.R")
library(ollamar)
library(jsonlite)
library(dplyr)
library(purrr)        # pmap() for the two-column (text + proposed label) call
library(tidyr)
library(stringr)


score_judge_local <- function(text_input, proposed_sentiment, model = "llama3.2:latest",
                              labels = NULL, system_prompt = NULL, json_schema = NULL) {
  
  if (is.null(system_prompt) || is.null(json_schema)) {
    if (is.null(labels)) {
      stop("Must supply `labels` (for the default label-driven judge prompt) ",
           "or both an explicit `system_prompt` and `json_schema`.")
    }
    if (is.null(system_prompt)) system_prompt <- get_judge_system_prompt(labels)
    if (is.null(json_schema))   json_schema   <- judge_json_schema(labels)
  }
  
  expected_fields <- names(json_schema$properties)
  
  user_prompt <- paste0(
    'Text: "', text_input, '"\n',
    'Assigned label: "', proposed_sentiment, '"'
  )
  
  tryCatch({
    
    res <- generate(
      model  = model,
      prompt = user_prompt,
      system = system_prompt,
      format = json_schema,
      output = "text",
      temperature = 0
    )
    
    parsed <- fromJSON(res)
    
    if ("corrected_sentiment" %in% expected_fields) {
      if (is.null(parsed$corrected_sentiment) || (!is.null(labels) && !parsed$corrected_sentiment %in% labels)) {
        message("Invalid/missing corrected_sentiment for input: ", 
                substr(text_input, 1, 60), " | got: ", parsed$corrected_sentiment %||% "NULL")
        parsed$corrected_sentiment <- NA_character_
      }
    }
    
    if ("agree" %in% expected_fields && is.null(parsed$agree)) {
      parsed$agree <- NA
    }
    
    if ("reasoning" %in% expected_fields && is.null(parsed$reasoning)) {
      parsed$reasoning <- NA_character_
    }
    
    return(parsed)
    
  }, error = function(e) {
    message("Local judge call failed for input: ", substr(text_input, 1, 60),
            " | ", conditionMessage(e))
    
    fallback <- setNames(as.list(rep(NA, length(expected_fields))), expected_fields)
    return(fallback)
  })
}

analyze_judge_local <- function(df, text_col, sentiment_col, model = "llama3.2:latest",
                                labels = NULL, system_prompt = NULL, json_schema = NULL) {
  df |>
    mutate(judge_data = pmap(
      list({{ text_col }}, {{ sentiment_col }}),
      function(txt, sent) score_judge_local(txt, sent, model = model, labels = labels,
                                            system_prompt = system_prompt, json_schema = json_schema)
    )) |>
    unnest_wider(judge_data)
}

## Cache-aware orchestrator. Cache key includes model, label set (or
## "nolabels" when a fully custom system_prompt/json_schema pair is used),
## and a hash of the system prompt actually used.
get_or_run_judge_local <- function(df, text_col, sentiment_col, output_name, model = "llama3.2:latest",
                                   labels = NULL, system_prompt = NULL, json_schema = NULL) {
  
  model_tag  <- str_replace_all(model, "[:/]", "-")
  labels_tag <- if (is.null(labels)) "nolabels" else labels_tag_for(labels)
  
  prompt_for_tag <- if (!is.null(system_prompt)) {
    system_prompt
  } else if (!is.null(labels)) {
    get_judge_system_prompt(labels)
  } else {
    stop("Must supply `labels` or an explicit `system_prompt`.")
  }
  prompt_hash <- as.character(as.hexmode(sum(utf8ToInt(prompt_for_tag))))
  
  expected_path <- paste0(output_name, "_judge_", model_tag, "_", labels_tag, "_", prompt_hash, ".rds")
  
  if (file.exists(expected_path)) {
    message("Found existing results at: ", expected_path, " — loading from disk.")
    readRDS(expected_path)
    
  } else {
    message("No existing results found at: ", expected_path, " — running local judge scoring.")
    result <- analyze_judge_local(df, {{ text_col }}, {{ sentiment_col }}, model = model,
                                  labels = labels, system_prompt = system_prompt, json_schema = json_schema)
    
    saveRDS(result, expected_path)
    message("Results saved to: ", expected_path)
    
    result
  }
}


