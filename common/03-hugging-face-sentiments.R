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
#       / get_or_run_hf_llm(). Requires a `client` pointed at a provider that
#       actually serves chat models — hf-inference itself mostly serves
#       classification/embeddings/NER, not chat, so a separate client
#       (hf_client_chat, provider = "featherless-ai") is used by default
#       for this mode. Verify any new model's actual provider via
#       huggingface_hub::model_info(model, expand = "inferenceProviderMapping")
#       before assuming it works on a given provider — provider support
#       shifts over time and per-model.
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

hf_client      <- hf_hub$InferenceClient(provider = "hf-inference",   api_key = hf_token)  # classification models (mode a)
hf_client_chat <- hf_hub$InferenceClient(provider = "featherless-ai", api_key = hf_token)  # chat/instruct models (mode b)

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
      is_loading <- str_detect(conditionMessage(e), regex("503|loading|currently loading", ignore_case = TRUE))
      list(error = TRUE, is_loading = is_loading, message = conditionMessage(e))
    })
    
    if (is.null(result$error)) return(result)
    
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

score_hf_llm <- function(text_input, model, labels = sentiment_levels, client = hf_client_chat) {
  
  system_prompt <- get_sentiment_system_prompt(labels)
  
  # Some providers (notably featherless-ai serving Gemma models) reject a
  # dedicated system role entirely — fold it into the user message instead,
  # which works universally regardless of whether the model/provider
  # supports a system role.
  combined_prompt <- paste0(system_prompt, "\n\n", text_input)
  
  tryCatch({
    resp <- client$chat_completion(
      messages = list(
        list(role = "user", content = combined_prompt)
      ),
      model = model,
      max_tokens = 200,
      temperature = 0
    )
    
    content_text <- resp$choices[[1]]$message$content
    
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


get_or_run_hf_llm <- function(df, text_col, output_name, model, labels = sentiment_levels, client = hf_client_chat) {
  
  model_tag  <- str_replace_all(model, "[:/]", "-")
  labels_tag <- labels_tag_for(labels)
  expected_path <- paste0(output_name, "_", model_tag, "_", labels_tag, "_hfllm.rds")
  
  if (file.exists(expected_path)) {
    message("Found existing results at: ", expected_path, " — loading from disk.")
    readRDS(expected_path)
    
  } else {
    message("No existing results found at: ", expected_path, " — running HF LLM scoring.")
    result <- analyze_text_hf_llm(df, {{ text_col }}, model = model, labels = labels, client = client)
    
    saveRDS(result, expected_path)
    message("Results saved to: ", expected_path)
    
    result
  }
}

## ================= (c) LOCAL TRANSFORMERS CLASSIFIERS (via reticulate) =====

.hf_local_model_name <- NULL

setup_hf_local <- function(model = "j-hartmann/emotion-english-distilroberta-base",
                           conda_env = "r-reticulate") {
  
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
         confidence_score = as.numeric(res[[1]]$score),
         reasoning = NA_character_)
  }, error = function(e) {
    message("Failed for: ", substr(text_input, 1, 60), " | ", conditionMessage(e))
    list(sentiment = NA, confidence_score = NA, reasoning = NA_character_)
  })
}

analyze_text_hf_local <- function(df, text_col, model = NULL) {
  df |>
    mutate(sentiment_data = map({{ text_col }}, ~score_hf_local(.x, model = model))) |> 
    unnest_wider(sentiment_data)
}

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


