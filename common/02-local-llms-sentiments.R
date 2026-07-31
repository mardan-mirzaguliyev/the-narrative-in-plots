source("00-shared_objects.R")
library(ollamar)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)


score_local_llm <- function(text_col, model = "llama3.2:latest", labels = NULL,
                            system_prompt = NULL, json_schema = NULL) {
  
  # Fall back to the generic label-driven prompt/schema only when neither
  # was explicitly supplied. A numeric-only call (no labels involved) must
  # pass its own system_prompt + json_schema instead.
  if (is.null(system_prompt) || is.null(json_schema)) {
    if (is.null(labels)) {
      stop("Must supply `labels` (for the default label-driven prompt) ",
           "or both an explicit `system_prompt` and `json_schema`.")
    }
    if (is.null(system_prompt)) system_prompt <- get_sentiment_system_prompt(labels)
    if (is.null(json_schema))   json_schema   <- categorical_json_schema(labels)
  }
  
  # Field names expected back, derived from the schema itself rather than
  # hardcoded — so any schema shape (categorical, categorical+numeric,
  # numeric-only) is handled the same way.
  expected_fields <- names(json_schema$properties)
  
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
    
    if ("sentiment" %in% expected_fields) {
      if (is.null(parsed$sentiment) || !parsed$sentiment %in% labels) {
        message("Invalid/missing sentiment label for input: ",
                substr(text_col, 1, 60), " | got: ", parsed$sentiment %||% "NULL")
        parsed$sentiment <- NA_character_
      }
    }
    
    if ("confidence_score" %in% expected_fields) {
      if (is.null(parsed$confidence_score) || !is.numeric(parsed$confidence_score)) {
        parsed$confidence_score <- 0.0
      } else {
        parsed$confidence_score <- pmin(pmax(parsed$confidence_score, 0), 1)
      }
    }
    
    if ("numeric_score" %in% expected_fields) {
      if (is.null(parsed$numeric_score) || !is.numeric(parsed$numeric_score)) {
        parsed$numeric_score <- NA_real_
      } else {
        parsed$numeric_score <- pmin(pmax(parsed$numeric_score, -1), 1)  # clamp to [-1, 1]
      }
    }
    
    if ("reasoning" %in% expected_fields && is.null(parsed$reasoning)) {
      parsed$reasoning <- NA_character_
    }
    
    return(parsed)
    
  }, error = function(e) {
    message("Local LLM call failed for input: ", substr(text_col, 1, 60),
            " | ", conditionMessage(e))
    
    # Build a matching NA-filled fallback based on whatever fields were expected
    fallback <- setNames(as.list(rep(NA, length(expected_fields))), expected_fields)
    if ("confidence_score" %in% expected_fields) fallback$confidence_score <- 0.0
    return(fallback)
  })
}

## Synchronous wrapper — passes system_prompt/json_schema/labels straight through.
## labels defaults to NULL; only required when system_prompt/json_schema aren't supplied.
analyze_text_local <- function(df, text_col, model = "llama3.2:latest", labels = NULL,
                               system_prompt = NULL, json_schema = NULL) {
  df |>
    mutate(sentiment_data = map({{ text_col }}, 
                                ~score_local_llm(.x, model = model, labels = labels,
                                                 system_prompt = system_prompt, json_schema = json_schema))) |>
    unnest_wider(sentiment_data)
}

## Cache-aware orchestrator. Cache key includes model, label set (or "nolabels"
## when none apply), and a hash of the system prompt actually used — so
## categorical, categorical+numeric, and numeric-only runs against the same
## model never collide in the cache, and a numeric-only run's filename
## doesn't misleadingly encode an unused label set.
get_or_run_local <- function(df, text_col, output_name, model = "llama3.2:latest",
                             labels = NULL, system_prompt = NULL, json_schema = NULL) {
  
  model_tag <- str_replace_all(model, "[:/]", "-")
  
  # Only tag by label set if one is actually relevant to this call
  labels_tag <- if (is.null(labels)) "nolabels" else labels_tag_for(labels)
  
  # Resolve the prompt actually being used, purely to hash it for the
  # filename — mirrors the fallback logic inside score_local_llm() without
  # duplicating its validation/error path.
  prompt_for_tag <- if (!is.null(system_prompt)) {
    system_prompt
  } else if (!is.null(labels)) {
    get_sentiment_system_prompt(labels)
  } else {
    stop("Must supply `labels` (for the default label-driven prompt) ",
         "or an explicit `system_prompt`.")
  }
  prompt_hash <- as.character(as.hexmode(sum(utf8ToInt(prompt_for_tag))))
  
  expected_path <- paste0(output_name, "_", model_tag, "_", labels_tag, "_", prompt_hash, ".rds")
  
  if (file.exists(expected_path)) {
    message("Found existing results at: ", expected_path, " — loading from disk.")
    readRDS(expected_path)
    
  } else {
    message("No existing results found at: ", expected_path, " — running local scoring.")
    result <- analyze_text_local(df, {{ text_col }}, model = model, labels = labels,
                                 system_prompt = system_prompt, json_schema = json_schema)
    
    saveRDS(result, expected_path)
    message("Results saved to: ", expected_path)
    
    result
  }
}





