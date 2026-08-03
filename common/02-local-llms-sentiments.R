library(ollamar)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(rlang)   # %||%


# ============================================================================
# Local LLM (Ollama) Scoring
# ============================================================================
# Supports three schema shapes via the shared `labels`/`system_prompt`/
# `json_schema` pattern used across every scoring path in this project:
#   1. Categorical only    — pass `labels`, nothing else
#   2. Categorical+numeric — pass `labels` + system_prompt = sentiment_numeric_system_prompt
#                             + json_schema = categorical_numeric_json_schema(labels)
#   3. Numeric only        — pass system_prompt = numeric_only_system_prompt
#                             + json_schema = numeric_only_json_schema(), no labels
#
# All numeric fields (confidence_score, numeric_score) are validated through
# safe_numeric() (see 00-shared_objects.R), which returns NA — not a fake
# plausible-looking 0 — for anything malformed, missing, non-numeric, or
# implausibly close to zero from a broken exponent (e.g.
# 0.75e-1612345678912345, which underflows to ~0 and would otherwise be
# indistinguishable from a genuine "the model said zero" answer).

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
      cleaned <- safe_numeric(parsed$confidence_score, 0, 1)
      if (is.na(cleaned)) {
        message("Invalid/implausible confidence_score for input: ", substr(text_col, 1, 60),
                " | raw value: ", parsed$confidence_score %||% "NULL")
      }
      parsed$confidence_score <- cleaned
    }
    
    if ("numeric_score" %in% expected_fields) {
      cleaned <- safe_numeric(parsed$numeric_score, -1, 1)
      if (is.na(cleaned)) {
        message("Invalid/implausible numeric_score for input: ", substr(text_col, 1, 60),
                " | raw value: ", parsed$numeric_score %||% "NULL")
      }
      parsed$numeric_score <- cleaned
    }
    
    if ("reasoning" %in% expected_fields && is.null(parsed$reasoning)) {
      parsed$reasoning <- NA_character_
    }
    
    return(parsed)
    
  }, error = function(e) {
    message("Local LLM call failed for input: ", substr(text_col, 1, 60),
            " | ", conditionMessage(e))
    
    # Build a matching NA-filled fallback based on whatever fields were expected.
    # NA everywhere, including confidence_score — no more silent 0.0 default,
    # since a real 0.0 and a failed call must stay distinguishable downstream.
    fallback <- setNames(as.list(rep(NA, length(expected_fields))), expected_fields)
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



