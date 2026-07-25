library(dplyr)   # used implicitly if any shared object involves data manipulation
library(rlang)    # %||% used inside extract_json_response()

## ================= PROMPTS =================

sentiment_system_prompt <- r"(
You are a professional sentiment analysis engine for literary text. Analyze the emotional tone of the provided text and output a single JSON object matching this exact schema:
{
  "sentiment": string,   // exactly one of: "Trust" | "Fear" | "Sadness" | "Anger" | "Surprise" | "Disgust" | "Joy" | "Anticipation" | "Neutral"
  "confidence_score": number  // 0.0-1.0, your certainty that this is the single best-fitting label (not the intensity of the emotion)
}
Rules:
1. Output ONLY the raw JSON object. No markdown code fences, no backticks, no preamble, no explanation, no trailing text.
2. Choose exactly one sentiment label, even when the text expresses mixed emotions - pick the single most dominant one.
3. If the text is ambiguous or blended, still choose the most probable label, but assign confidence_score below 0.5.
4. If the input is empty, whitespace-only, or nonsensical (not natural language), output {"sentiment": "Neutral", "confidence_score": 0.0}.
5. Base your judgment on the emotional content of the text itself, not on real-world factual accuracy or your own opinion of the content.
6. Do not add, remove, or rename JSON keys. Do not wrap the object in an array.
Examples:
Input: "The old house creaked, and she felt the walls closing in around her."
Output: {"sentiment": "Fear", "confidence_score": 0.82}
Input: "asdkj 12341 !!!"
Output: {"sentiment": "Neutral", "confidence_score": 0.0}
)"

judge_system_prompt <- r"(
You are an expert emotion-annotation reviewer. You will be given a short piece of text and a sentiment label that another method assigned to it. Decide whether that label accurately describes the text's dominant emotion.

Output ONLY a single JSON object matching this exact schema:
{
  "agree": boolean,
  "corrected_sentiment": string,  // one of the 9 categories below if agree is false; otherwise repeat the original label
  "reasoning": string  // one short sentence explaining the judgment
}

Allowed sentiment categories: Trust, Fear, Sadness, Anger, Surprise, Disgust, Joy, Anticipation, Neutral

Rules:
1. If the assigned label is a reasonable fit for the text's dominant emotion, set agree=true and corrected_sentiment to the same label.
2. If the label is a poor fit, set agree=false and corrected_sentiment to whichever category you believe fits best.
3. Base your judgment only on the emotional content of the text itself, not on assumptions about surrounding context you cannot see.
4. Output ONLY the raw JSON object. No markdown code fences, no backticks, no preamble, no explanation outside the reasoning field.
)"

## ================= LABEL SET =================
## Single source of truth — referenced by JSON schemas, validation checks,
## and chart level-ordering so nothing can silently disagree across scripts.

valid_sentiment_labels <- c("Trust", "Fear", "Sadness", "Anger", "Surprise", 
                            "Disgust", "Joy", "Anticipation", "Neutral")

## Chart y-axis ordering — Neutral deliberately placed LAST in this vector,
## since ggplot's discrete y-axis renders the last level at the TOP.
## Keeping Neutral visually separated from the 8 real emotions is a
## deliberate, series-wide design choice — do not reorder without
## updating every chart that references this.
sentiment_levels <- c("Joy", "Trust", "Anticipation", "Surprise",
                      "Anger", "Disgust", "Fear", "Sadness", "Neutral")

## Chart fill colors — Neutral in muted gray (not part of the emotion
## spectrum), the 8 real emotions in distinct saturated hues.
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

## ================= SHARED PARSING HELPER =================
## Extracts and parses the JSON object from a Claude `content` block list.
## Schema-agnostic — used identically by score_claude_synch() (sentiment
## schema), parse_batch_result_line() (sentiment schema), and
## score_judge_claude() (judge schema).

extract_json_response <- function(content_blocks) {
  text_blocks <- Filter(function(b) b$type == "text", content_blocks)
  if (length(text_blocks) == 0) return(NULL)
  
  content_text <- text_blocks[[1]]$text
  clean_json <- gsub("(?s).*(\\{.*\\}).*", "\\1", content_text, perl = TRUE) |> trimws()
  
  jsonlite::fromJSON(clean_json)
}


