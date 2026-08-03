library(dplyr)
library(rlang)    # %||% used inside extract_json_response()


## ================= LABEL SET =================
sentiment_levels <- c("Joy", "Trust", "Anticipation", "Surprise",
                      "Anger", "Disgust", "Fear", "Sadness", "Neutral")

sentiment_colors <- c(
  "Neutral"      = "#adb5bd",
  "Trust"        = "#2a9d8f",
  "Anticipation" = "#e9c46a",
  "Fear"         = "#264653",
  "Joy"          = "#f4a261",
  "Anger"        = "#e76f51",
  "Sadness"      = "#6d6875",
  "Surprise"     = "#84a59d",
  "Disgust"      = "#bc6c25"
)


## ================= SCHEMAS =================

## 1. Categorical only: sentiment + confidence_score + reasoning
categorical_json_schema <- function(labels) {
  list(
    type = "object",
    properties = list(
      sentiment        = list(type = "string", enum = labels),
      confidence_score = list(type = "number"),
      reasoning        = list(type = "string")
    ),
    required = list("sentiment", "confidence_score", "reasoning")
  )
}

## 2. Categorical + numeric: sentiment + numeric_score + confidence_score + reasoning
categorical_numeric_json_schema <- function(labels) {
  list(
    type = "object",
    properties = list(
      sentiment        = list(type = "string", enum = labels),
      numeric_score    = list(type = "number"),
      confidence_score = list(type = "number"),
      reasoning        = list(type = "string")
    ),
    required = list("sentiment", "numeric_score", "confidence_score", "reasoning")
  )
}

## 3. Numeric only, no category at all
numeric_only_json_schema <- function() {
  list(
    type = "object",
    properties = list(
      numeric_score    = list(type = "number"),
      confidence_score = list(type = "number"),
      reasoning        = list(type = "string")
    ),
    required = list("numeric_score", "confidence_score", "reasoning")
  )
}

## 4. Judge schema: agree + corrected_sentiment + reasoning
## Only needed by the local (Ollama) judging path, which requires an actual
## schema object for `format=`. Claude's judging path has no format-
## enforcement API, so it only ever needs the prompt text, not this.
judge_json_schema <- function(labels) {
  list(
    type = "object",
    properties = list(
      agree               = list(type = "boolean"),
      corrected_sentiment = list(type = "string", enum = labels),
      reasoning           = list(type = "string")
    ),
    required = list("agree", "corrected_sentiment", "reasoning")
  )
}

## ================= PROMPTS =================
## Each prompt has exactly ONE generating function as its source of truth.
## "Fixed default" versions (used directly by scripts that don't need a
## variable label set) are built by CALLING that function with
## sentiment_levels, then appending worked examples where applicable —
## never by hand-typing a second, separately-maintained copy of the text.

## --- 1. Categorical only ---

get_sentiment_system_prompt <- function(labels) {
  labels_enum <- paste0('"', labels, '"', collapse = " | ")
  labels_list <- paste(labels, collapse = ", ")
  
  paste0(
    "You are a professional sentiment analysis engine for literary text. ",
    "Analyze the emotional tone of the provided text and output a single JSON object matching this exact schema:\n",
    "{\n",
    "  \"sentiment\": string,   // exactly one of: ", labels_enum, "\n",
    "  \"confidence_score\": number,  // 0.0-1.0, your certainty that this is the single best-fitting label (not the intensity of the emotion)\n",
    "  \"reasoning\": string  // one short sentence explaining the judgment\n",
    "}\n",
    "Rules:\n",
    "1. Output ONLY the raw JSON object. No markdown code fences, no backticks, no preamble, no explanation, no trailing text.\n",
    "2. Choose exactly one sentiment label, even when the text expresses mixed emotions - pick the single most dominant one.\n",
    "3. If the text is ambiguous or blended, still choose the most probable label, but assign confidence_score below 0.5.\n",
    "4. If the input is empty, whitespace-only, or nonsensical (not natural language), pick whichever label in this set represents a neutral or no-signal state, and set confidence_score to 0.0.\n",
    "5. Base your judgment on the emotional content of the text itself, not on real-world factual accuracy or your own opinion of the content.\n",
    "6. Do not add, remove, or rename JSON keys. Do not wrap the object in an array.\n",
    "Allowed sentiment categories: ", labels_list
  )
}


categorical_prompt_examples <- r"(
Examples:
Input: "The old house creaked, and she felt the walls closing in around her."
Output: {"sentiment": "Fear", "confidence_score": 0.82, "reasoning": "The creaking house and closing walls create a strong sense of dread."}
Input: "asdkj 12341 !!!"
Output: {"sentiment": "Neutral", "confidence_score": 0.0, "reasoning": "No interpretable emotional content."}
)"

## Fixed default (9-category, with worked examples) — derived, not duplicated.
sentiment_system_prompt <- paste0(get_sentiment_system_prompt(sentiment_levels), "\n", categorical_prompt_examples)

## --- 2. Categorical + numeric ---

get_sentiment_numeric_system_prompt <- function(labels) {
  labels_enum <- paste0('"', labels, '"', collapse = " | ")
  labels_list <- paste(labels, collapse = ", ")
  
  paste0(
    "You are a professional sentiment analysis engine for literary text. ",
    "Analyze the emotional tone of the provided text and output a single JSON object matching this exact schema:\n",
    "{\n",
    "  \"sentiment\": string,        // exactly one of: ", labels_enum, "\n",
    "  \"numeric_score\": number,    // -1.0 to 1.0, overall emotional valence: -1.0 = extremely negative, 0.0 = neutral/mixed, 1.0 = extremely positive\n",
    "  \"confidence_score\": number, // 0.0-1.0, your certainty that the sentiment label and numeric_score above are the best fit for this text\n",
    "  \"reasoning\": string         // one short sentence explaining both the label and the numeric score\n",
    "}\n",
    "Rules:\n",
    "1. Output ONLY the raw JSON object. No markdown code fences, no backticks, no preamble, no explanation, no trailing text.\n",
    "2. Choose exactly one sentiment label, even when the text expresses mixed emotions - pick the single most dominant one.\n",
    "3. numeric_score reflects overall positive/negative valence, independent of which specific label was chosen — a Fear label can still have a numeric_score anywhere on the negative side depending on intensity (e.g. mild unease vs. abject terror), and Neutral should generally sit close to 0.0 but is not required to be exactly 0.0.\n",
    "4. If the text is ambiguous or blended, still choose the most probable label and a numeric_score reflecting the blend, but assign confidence_score below 0.5.\n",
    "5. If the input is empty, whitespace-only, or nonsensical (not natural language), pick whichever label in this set represents a neutral or no-signal state, set numeric_score to 0.0, and confidence_score to 0.0.\n",
    "6. Base your judgment on the emotional content of the text itself, not on real-world factual accuracy or your own opinion of the content.\n",
    "7. Do not add, remove, or rename JSON keys. Do not wrap the object in an array.\n",
    "Allowed sentiment categories: ", labels_list
  )
}

categorical_numeric_prompt_examples <- r"(
Examples:
Input: "The old house creaked, and she felt the walls closing in around her."
Output: {"sentiment": "Fear", "numeric_score": -0.7, "confidence_score": 0.82, "reasoning": "The creaking house and closing walls create a strong sense of dread and entrapment."}
Input: "asdkj 12341 !!!"
Output: {"sentiment": "Neutral", "numeric_score": 0.0, "confidence_score": 0.0, "reasoning": "No interpretable emotional content."}
)"

## Fixed default (9-category, with worked examples) — derived, not duplicated.
sentiment_numeric_system_prompt <- paste0(get_sentiment_numeric_system_prompt(sentiment_levels), "\n", categorical_numeric_prompt_examples)

## --- 3. Numeric only — no labels dependency, so no generic/fixed split needed ---

numeric_only_system_prompt <- r"(
You are a sentiment intensity scoring engine. Given a short piece of text, output a single JSON object matching this exact schema:
{
  "numeric_score": number,    // -1.0 to 1.0, overall emotional valence: -1.0 = extremely negative, 0.0 = neutral/mixed, 1.0 = extremely positive
  "confidence_score": number, // 0.0-1.0, your certainty in this valence estimate
  "reasoning": string         // one short sentence explaining the score
}
Rules:
1. Output ONLY the raw JSON object. No markdown code fences, no backticks, no preamble, no explanation, no trailing text.
2. Do NOT classify the text into any named emotion category — estimate only its overall positive/negative valence and intensity.
3. If the text is ambiguous or blended, output a numeric_score reflecting the net balance, and assign confidence_score below 0.5.
4. If the input is empty, whitespace-only, or nonsensical (not natural language), output {"numeric_score": 0.0, "confidence_score": 0.0, "reasoning": "No interpretable emotional content."}.
5. Base your judgment on the emotional content of the text itself, not on real-world factual accuracy or your own opinion of the content.
6. Do not add, remove, or rename JSON keys. Do not wrap the object in an array.
Examples:
Input: "The old house creaked, and she felt the walls closing in around her."
Output: {"numeric_score": -0.7, "confidence_score": 0.82, "reasoning": "The creaking house and closing walls create a strong sense of dread and entrapment."}
Input: "asdkj 12341 !!!"
Output: {"numeric_score": 0.0, "confidence_score": 0.0, "reasoning": "No interpretable emotional content."}
)"

## --- 4. Judge prompt ---

get_judge_system_prompt <- function(labels) {
  labels_list <- paste(labels, collapse = ", ")
  
  paste0(
    "You are an expert emotion-annotation reviewer. You will be given a short piece of text and a sentiment label that another method assigned to it. Decide whether that label accurately describes the text's dominant emotion.\n\n",
    "Output ONLY a single JSON object matching this exact schema:\n",
    "{\n",
    "  \"agree\": boolean,\n",
    "  \"corrected_sentiment\": string,  // one of the categories below if agree is false; otherwise repeat the original label\n",
    "  \"reasoning\": string  // one short sentence explaining the judgment\n",
    "}\n\n",
    "Allowed sentiment categories: ", labels_list, "\n\n",
    "Rules:\n",
    "1. If the assigned label is a reasonable fit for the text's dominant emotion, set agree=true and corrected_sentiment to the same label.\n",
    "2. If the label is a poor fit, set agree=false and corrected_sentiment to whichever category you believe fits best.\n",
    "3. Base your judgment only on the emotional content of the text itself, not on assumptions about surrounding context you cannot see.\n",
    "4. Output ONLY the raw JSON object. No markdown code fences, no backticks, no preamble, no explanation outside the reasoning field."
  )
}

## Fixed default (9-category) — derived, not hand-duplicated.
judge_system_prompt <- get_judge_system_prompt(sentiment_levels)


## ================= SHARED PARSING HELPER =================
extract_json_response <- function(content_blocks) {
  text_blocks <- Filter(function(b) b$type == "text", content_blocks)
  if (length(text_blocks) == 0) return(NULL)
  
  content_text <- text_blocks[[1]]$text
  clean_json <- gsub("(?s).*(\\{.*\\}).*", "\\1", content_text, perl = TRUE) |> trimws()
  
  jsonlite::fromJSON(clean_json)
}

## ================= FILENAME TAGGING HELPERS =================
labels_tag_for <- function(labels) {
  sorted_labels <- sort(labels)
  hash_input <- paste(sorted_labels, collapse = "|")
  hash_val <- sum(utf8ToInt(hash_input))
  hash_str <- as.character(as.hexmode(hash_val))
  
  paste0(length(labels), "cat-", hash_str)
}




## ================= NUMERIC VALIDATION HELPER =================
## Guards against malformed numeric values slipping through as
## plausible-looking data — e.g. a broken scientific-notation literal like
## `0.75e-1612345678912345` parses as numeric and underflows to ~0, which is
## indistinguishable from a genuine "the model said zero" answer unless
## caught here. Used by every scoring path (local, Claude sync, Claude
## batch) so a parsing glitch is never silently reported as a real result.
##
## Returns NA_real_ (not 0) for anything invalid or implausibly extreme —
## NA preserves "we don't actually know" rather than manufacturing a
## fake-but-plausible value, and callers should log when this fires.
safe_numeric <- function(x, min_val, max_val, underflow_floor = 1e-6) {
  if (is.null(x) || !is.numeric(x) || is.nan(x) || is.infinite(x)) {
    return(NA_real_)
  }
  # Catches near-zero underflow from a malformed exponent, without
  # rejecting genuine, deliberately-small-but-real values below the floor —
  # 1e-6 is far smaller than any meaningful confidence/valence distinction
  # this schema actually needs.
  if (abs(x) > 0 && abs(x) < underflow_floor) {
    return(NA_real_)
  }
  pmin(pmax(x, min_val), max_val)
}

exists("safe_numeric")
safe_numeric(0.75e-1612345678912345, 0, 1)   # should return NA, not 0
safe_numeric(0.1, 0, 1)                        # should return 0.1, unaffected











