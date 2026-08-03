library(reticulate)  # import(), use_python()/use_condaenv() — bridges to Python for local classifiers
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)     # str_to_title() for label formatting

# Hugging Face
# Three distinct modes, since "Hugging Face" covers three different kinds of
# models here:
#   (a) Fixed-vocabulary classifiers (e.g. cardiffnlp/twitter-roberta-*), via
#       the hosted Inference API — score_hf_inference() / analyze_text_hf() /
#       get_or_run_hf(). No prompt, no dynamic labels — the model dictates
#       its own label set.
#   (b) HF-hosted instruct/chat models, via the hosted Inference API — same
#       dynamic-label mechanism as the local and Claude LLM paths, via
#       get_sentiment_system_prompt() — score_hf_llm() / analyze_text_hf_llm()
#       / get_or_run_hf_llm().
#   (c) Local transformers classifiers, run fully offline via reticulate/
#       torch — not through hf_client at all. For models not servable on
#       the hosted API (or that you'd rather run locally) —
#       setup_hf_local() / score_hf_local() / analyze_text_hf_local() /
#       get_or_run_hf_local(). Model-agnostic: swap models by calling
#       setup_hf_local(model = ...) again.

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

## ================= (a) FIXED-VOCABULARY CLASSIFIERS (hosted API) =================

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
    parsed <- extract_json_response(list(list(type = "text", text = content_text)))
    
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

## ================= (c) LOCAL TRANSFORMERS CLASSIFIERS (via reticulate) =====
## Runs fully locally via transformers/torch, not through hf_client — for
## models not servable on HF's hosted Inference API (or that you'd rather
## run locally). Has its own fragile, machine-specific conda/reticulate
## setup, so it's NOT run automatically on source() — call setup_hf_local()
## explicitly, with whichever model you want, before scoring. This keeps
## source()-ing this whole file safe even if conda isn't configured, since
## paths (a) and (b) above don't depend on it at all.
##
## Assumes the loaded model returns a single top label + score via a
## standard `text-classification` pipeline — true for most HF classification
## models regardless of their specific label set.

## Tracks which model is currently loaded, so score_hf_local()/get_or_run_hf_local()
## can warn if you're scoring against a different model than you think you are.
.hf_local_model_name <- NULL

setup_hf_local <- function(model = "j-hartmann/emotion-english-distilroberta-base",
                           conda_binary_path = "C:/Users/Mardan/miniconda3/condabin/conda.bat",
                           conda_env = "r-reticulate") {
  
  options(reticulate.conda_binary = conda_binary_path)
  reticulate::conda_binary()
  reticulate::use_condaenv(conda_env, required = TRUE)
  
  transformers <<- reticulate::import("transformers")
  
  hf_local_classifier <<- transformers$pipeline(
    "text-classification",
    model = model
  )
  
  .hf_local_model_name <<- model
  message("Local transformers model loaded: ", model)
}

score_hf_local <- function(text_input, model = NULL) {
  
  if (!exists("hf_local_classifier")) {
    stop("No local transformers model loaded — call setup_hf_local(model = ...) first.")
  }
  
  if (!is.null(model) && !identical(model, .hf_local_model_name)) {
    stop("Requested model '", model, "' does not match the currently loaded model '",
         .hf_local_model_name, "'. Call setup_hf_local(model = '", model, "') to switch.")
  }
  
  tryCatch({
    res <- hf_local_classifier(text_input)
    list(sentiment = str_to_title(res[[1]]$label),
         confidence_score = as.numeric(res[[1]]$score))
  }, error = function(e) {
    message("Failed for: ", substr(text_input, 1, 60), " | ", conditionMessage(e))
    list(sentiment = NA, confidence_score = NA)
  })
}

analyze_text_hf_local <- function(df, text_col, model = NULL) {
  df |>
    mutate(sentiment_data = map({{ text_col }}, ~score_hf_local(.x, model = model))) |> 
    unnest_wider(sentiment_data)
}

## Cache-aware orchestrator. model_tag is derived from whichever model is
## actually loaded (.hf_local_model_name), not from a hardcoded string —
## so switching models via setup_hf_local() automatically produces a
## distinct cache file, the same way get_or_run_local()/get_or_run_claude_synch()
## key on `model`.
get_or_run_hf_local <- function(df, text_col, output_name, model = NULL) {
  
  if (!exists(".hf_local_model_name") || is.null(.hf_local_model_name)) {
    stop("No local transformers model loaded — call setup_hf_local(model = ...) first.")
  }
  
  if (!is.null(model) && !identical(model, .hf_local_model_name)) {
    stop("Requested model '", model, "' does not match the currently loaded model '",
         .hf_local_model_name, "'. Call setup_hf_local(model = '", model, "') to switch.")
  }
  
  model_tag <- str_replace_all(.hf_local_model_name, "[:/]", "-")
  expected_path <- paste0(output_name, "_", model_tag, ".rds")
  
  if (file.exists(expected_path)) {
    message("Found existing results at: ", expected_path, " — loading from disk.")
    readRDS(expected_path)
  } else {
    message("No existing results found at: ", expected_path, " — running scoring.")
    result <- analyze_text_hf_local(df, {{ text_col }}, model = model)
    saveRDS(result, expected_path)
    message("Results saved to: ", expected_path)
    result
  }
}



