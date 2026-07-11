library(rulexicon)      # Access to RuSentiLex lexicon 
library(dplyr)          # Data manipulation 
library(tidyr)          # Reshaping: pivot_wider function
library(ggplot2)        # Data visualization
library(forcats)
library(textdata)       # Downloads and caches AFINN / Bing / NRC lexicons locally
library(stringr)        # String ops: str_replace_all for contraction expansion
library(tibble)         # tribble() for readable row-by-row data entry; clean printing
library(scales)         # Axis formatters: label_percent(), label_number()
library(googlesheets4)  # Reads lyrics dataframe directly from Google Sheets
library(tidytext)       # Tokenisation pipeline: unnest_tokens + lexicon joins
library(syuzhet)        # Line-level scoring without tokenisation; better for lyrics
library(ggrepel)
library(gt)
library(reticulate)
library(purrr)
library(httr2)   
library(jsonlite)
library(ollamar)
library(patchwork)


# DATA PREPARATION
# Changes, 2Pac syuzhet pipeline

## Read lyrics of song into R 
## Authenticate Google account that contains lyrics file
gs4_deauth()

## Read individual sheets of lyrics
lyrics_id <- "13dM_5vWQGdAeRI-thE6nbzlZIbvbZ8dw9eCu3MsP9Kk"


# Changes, 2Pac
## 1.1 Lexicon based sentiments
lyrics_raw_2pac <- read_sheet(lyrics_id, "Changes 2Pac")


## Syuzhet scoring for 2Pac, Changes 
lyrics_scored_2pac <- lyrics_raw_2pac |>
  mutate(syuzhet_score = get_sentiment(text, method = "syuzhet"))
lyrics_scored_2pac

# Normalise to –1 / +1
# syuzhet scores are unbounded, so we divide by the observed maximum
# to get a scale anchored between –1 and +1
max_abs <- max(abs(lyrics_scored_2pac$syuzhet_score))
max_abs

lyrics_scored_2pac <- lyrics_scored_2pac |> 
  mutate(normalized_score = syuzhet_score / max_abs)
lyrics_scored_2pac


section_order_2pac <- c("Verse 1", "Chorus 1", "Verse 2", "Chorus 2", "Interlude", "Verse 3", "Chorus 3")

## Most negative and most positive scores
extremes <- 
  lyrics_scored_2pac |> 
  group_by(section) |> 
  summarize(
    section_min = min(normalized_score),
    section_max = max(normalized_score))

extremes_plot <- extremes|> 
  filter(section_min != 0) |> 
  mutate(min_color = ifelse(section_min < 0, "#bd1515", "#2a9d8f"))


plot_2pac_section_scores <- lyrics_scored_2pac |>
  mutate(section = factor(section, levels = section_order_2pac)) |>
  ggplot(aes(x = section, y = normalized_score)) +
  geom_boxplot(fill = "#e0f2f9",
               color = "black",
               outlier.color = "red",
               median.linewidth = 0,
               alpha = 0.5) +
  geom_jitter(aes(colour = normalized_score), width = 0.2, show.legend = FALSE) +
  scale_color_gradient2(
    low      = "#bd1515",
    mid      = "grey90",
    high     = "#2a9d8f",
    midpoint = 0
  ) +
  geom_label_repel(
    data = extremes |> filter(section_max != 0),
    aes(x = section,
        y = section_max,
        label = round(section_max, 2)),
    colour   = "#2a9d8f",
    fontface = "bold",
    size     = 3.5,
    fill     = "white",
    box.padding = 0.5,
    point.padding = 0.5,
    segment.color = "grey50"
  ) +
  geom_label_repel(
    data = extremes_plot,
    aes(x = section, 
        y = section_min,
        label = round(section_min, 2)),
    color = extremes_plot$min_color,
    fontface = "bold",
    size     = 3.5,
    fill     = "white",
    box.padding = 0.5,
    point.padding = 0.5,
    segment.color = "grey50"
  ) +
  scale_y_continuous(
    limits = c(-1.2, 1.2),
    oob = squish,
    breaks = c(-1.0, -0.5, 0, 0.5, 1.0),
    labels = c("-1.0", "-0.5", "0", "0.5", "1") # Changed "Neutral" to "0"
  ) +
  labs(
    title    = "'Changes, 2Pac'",
    subtitle = "Cross sectional emotional trajectory (syuzhet)",
    x        = "Section",
    y        = "Sentiment Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray30", linewidth = 0.1),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_2pac_section_scores

ggsave(
  filename = "plots/01-plot_2pac_section_scores.png",
  plot = plot_2pac_section_scores,
  width = 15,
  height = 10,
  dpi = 300
)


lyrics_scored_2pac |> 
  filter(section == "Chorus 3") |> 
  mutate(normalized_score = percent(normalized_score, accuracy = 0.2)) |> 
  select(line, text, normalized_score) |> 
  gt() |> 
  cols_label(
    line = "Line Number",
    text = "Line",
    normalized_score = "Score"
  ) |>
  tab_header(
    title = "Scored Lines of Chorus 3",
    subtitle = "Only two lines scored: 96 an 98 (Normalized syuzhet scores)"
  ) |> 
  # Header styling
  tab_style(
    style = list(cell_fill(color = "#2a9d8f"), cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) |> 
  # Base Body Styling (all rows)
  tab_style(
    style = cell_fill(color = "#cbe8f5"),
    locations = cells_body()
  ) |> 
  # Highlighting the scored lines
  tab_style(
    style = cell_fill(color = "#f4a261"),
    locations = cells_body(rows = normalized_score != "0.0%")
  ) |> 
  cols_align(align = "center", columns = everything()) |> 
  gtsave("tables/01-table_chorus_3_scored_lines.png")



## 1.2 Hugging Face
Sys.getenv("HF_TOKEN")

use_condaenv("r-reticulate", required = TRUE)

# Import Python infrastructure modules
os <- import("os")
hf_hub <- import("huggingface_hub")
transformers <- import("transformers")

hf_token <- Sys.getenv("HF_TOKEN")
if (hf_token == "") {
  stop("Error: HG_TOKEN not found! Please check your .Renviron file.")
}

client <- hf_hub$InferenceClient(
  provider = "hg-inference",
  api_key = hf_token
)

# Modify your pipeline call slightly if needed:
classifier <- transformers$pipeline(
  task = "text-classification",
  model = "cardiffnlp/twitter-roberta-base-emotion",
  client = client,
  top_k = 1L # Forces the model to only return the most confident emotion
)


lyrics_2pac_scored_roberta <- lapply(seq_along(lyrics_raw_2pac$text), function(i) {
  line_text <- lyrics_raw_2pac$text[i]
  
  print(paste("Processing line:", line_text))
  tryCatch({
    # Run classification
    res <- classifier(line_text)
    
    # Access directly: res[[1]] is the result, [[1]] is the first (and only) dict
    emotion_label <- res[[1]][[1]]$label
    confidence_score <- res[[1]][[1]]$score
    
    tibble(
      emotion_label = str_to_title(emotion_label),
      confidence_score = as.numeric(confidence_score)
    )
  }, error = function(e) {
    # Return a dummy row if a line fails
    tibble(emotion_label = "Neutral", confidence_score = 0)
  })
}) |> bind_rows()

lyrics_2pac_scored_roberta_final <- lyrics_raw_2pac |> 
  bind_cols(lyrics_2pac_scored_roberta)


# Calculate emotion frequency per section
plot_lyrics_2pac_scored_roberta <- lyrics_2pac_scored_roberta_final |>
  mutate(section = factor(section, levels = section_order_2pac)) |> 
  group_by(section, emotion_label) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  mutate(share = count / sum(count)) |> 
  ggplot(aes(x = section, y = share, fill = emotion_label)) +
  geom_bar(stat = "identity", position = "fill", show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      emotion_label, 
      "\nShare: ", round(share * 100, 0), "%",
      "\nConfidence: ", round(avg_conf, 2), ""
    )), 
    position = position_fill(vjust = 0.5), 
    color = "white", 
    size = 3,
    lineheight = 0.8 
  ) +
  labs(title = "Emotional Trajectory (Twitter-roBERTa)",
       x = "Section",
       y = "Proportion of Emotions") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray30", linewidth = 0.1),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_lyrics_2pac_scored_roberta


ggsave(
  filename = "plots/02-plot_lyrics_2pac_scored_roberta.png",
  plot = plot_lyrics_2pac_scored_roberta,
  width = 15,
  height = 10,
  dpi = 300
)


## 1.3 Local LLMs

### llama3.2:latest
score_local_llm <- function(text_line, model = "llama3.2:latest") {
  # Define the core logic within a tryCatch to handle failures internally
  result <- tryCatch({
    sys_prompt <- "You are a professional sentiment analysis engine.
    Your task is to output a single JSON object for the provided text.
    Follow these constraints:
      1. Output format: Strictly JSON. Do not include markdown code blocks, backticks, or explanatory text.
      2. Keys required: 'emotion_label' (string) and 'confidence_score' (numeric between 0.0 and 1.0).
      3. Allowed labels: 'Joy', 'Optimism', 'Anger', 'Sadness', or 'Neutral'.
      4. Behavior: Do not provide conversational responses, justifications, or meta-commentary. If a line is ambiguous, choose the most probable label and assign a low confidence score. If the input is empty or nonsensical, return 'Neutral' with a confidence of 0.0."
    
    res <- generate(
      model = model,
      prompt = paste(sys_prompt, text_line),
      output = "text"
    )
    
    # Extract JSON string
    json_str <- gsub(".*(\\{.*\\}).*", "\\1", res)
    
    # Parse
    fromJSON(json_str)
    
  }, error = function(e) {
    # If the model fails or returns non-JSON, return this default list
    list(emotion_label = "Neutral", confidence_score = 0.0)
  })
  
  # Ensure the returned object is always a list for unnest_wider to work
  return(as.list(result))
}


results_2pac_llama3.2_latest <- lyrics_raw_2pac |>
  mutate(sentiment_data = map(text, ~score_local_llm(.x))) |>
  unnest_wider(sentiment_data)


plot_2pac_lyrics_llama3.2_latest <- results_2pac_llama3.2_latest |> 
 mutate(section = factor(section, levels = section_order_2pac)) |> 
  group_by(section, emotion_label) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  mutate(share = count / sum(count)) |> 
  ggplot(aes(x = section, y = share, fill = emotion_label)) +
  geom_bar(stat = "identity", position = "fill", show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      emotion_label, 
      "\nShare: ", round(share * 100, 0), "%",
      "\nConfidence: ", round(avg_conf, 2), ""
    )), 
    position = position_fill(vjust = 0.5), 
    color = "white", 
    size = 3,
    lineheight = 0.8 
  ) +
  labs(title = "Emotional Trajectory (llama3.2:latest)",
       x = "Section",
       y = "Proportion of Emotions") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray30", linewidth = 0.1),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_2pac_lyrics_llama3.2_latest

ggsave(
  filename = "plots/03-plot_2pac_lyrics_llama3.2_latest.png",
  plot = plot_2pac_lyrics_llama3.2_latest,
  width = 15,
  height = 10,
  dpi = 300
)


### phi4-mini:latest
phi4_mini_latest <- "phi4-mini:latest"

results_2pac_phi4_mini_latest <- lyrics_raw_2pac |>
  mutate(batch_id = ceiling(row_number() / 10)) |> 
  group_split(batch_id) |> 
  map_df(~.x |> 
           mutate(sentiment_data = map(text, ~score_local_llm(.x, model = phi4_mini_latest))) |>
           unnest_wider(sentiment_data)) |> 
  select(-batch_id)

results_2pac_phi4_mini_latest
  

plot_lyrics_2pac_phi4_mini_latest <- results_2pac_phi4_mini_latest |> 
  mutate(section = factor(section, levels = section_order_2pac)) |> 
  group_by(section, emotion_label) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  mutate(share = count / sum(count)) |> 
  ggplot(aes(x = section, y = share, fill = emotion_label)) +
  geom_bar(stat = "identity", position = "fill", show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      emotion_label, 
      "\nShare: ", round(share * 100, 0), "%",
      "\nConfidence: ", round(avg_conf, 2), ""
    )), 
    position = position_fill(vjust = 0.5), 
    color = "white", 
    size = 3,
    lineheight = 0.8 
  ) +
  labs(title = "Emotional Trajectory (phi4-mini:latest)",
       x = "Section",
       y = "Proportion of Emotions") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray30", linewidth = 0.1),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_lyrics_2pac_phi4_mini_latest


ggsave(
  filename = "plots/04-plot_lyrics_2pac_phi4_mini_latest.png",
  plot = plot_lyrics_2pac_phi4_mini_latest,
  width = 15,
  height = 10,
  dpi = 300
)


## 1.4 Claude API

Sys.getenv("CLAUDE_API_KEY")

### Get the list of Claude Models available
get_clean_model_list <- function() {
  req <- request("https://api.anthropic.com/v1/models") |> 
    req_headers(
      "x-api-key" = Sys.getenv("CLAUDE_API_KEY"),
      "anthropic-version" = "2023-06-01"
    )
  
  resp <- req_perform(req)
  data <- resp_body_json(resp)$data
  
  # Flatten and convert to a clean tibble
  model_df <- map_dfr(data, ~tibble(
    id = .x$id,
    display_name = .x$display_name,
    created_at = .x$created_at
  ))
  
  return(model_df)
}


clean_models <- get_clean_model_list()
clean_models


## Build the main scoring function
score_claude_llm <- possibly(function(text_input) {
  
  # Construct the API request
  req <- request("https://api.anthropic.com/v1/messages") |> 
    req_headers(
      "x-api-key" = Sys.getenv("CLAUDE_API_KEY"),
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ) |> 
    req_body_json(list(
      model = "claude-sonnet-5",
      max_tokens = 100,
      system =
      "You are a professional sentiment analysis engine.
       Your task is to output a single JSON object for the provided text.
       Follow these constraints:
         1. Output format: Strictly JSON.
            Do not include markdown code blocks, backticks, or explanatory text.
         2. Keys required: 'emotion_label' (string) and 'confidence_score'
            (numeric between 0.0 and 1.0).
         3. Allowed labels: 'Joy', 'Optimism', 'Anger', 'Sadness', or 'Neutral'.
         4. Behavior: Do not provide conversational responses, justifications, or meta-commentary.
            If a line is ambiguous, choose the most probable label and assign a low confidence score. 
           If the input is empty or nonsensical, return 'Neutral' with a confidence of 0.0.",
      messages = list(list(role = "user", content = text_input))
      ))
  
  # Execute the request
  resp <- req_perform(req)
  
  # Parse the response
  resp_body <- resp_body_json(resp)
  
  content_text <- resp_body$content[[1]]$text
  
  # Remove potential markdown code blocks
  clean_json <- gsub("```json|```", "", content_text)
  clean_json <- trimws(clean_json)
  
  # Extract JSON from the string
  result <- fromJSON(clean_json)
  
  return(result)

}, otherwise = list(emotion_label = NA, confidence_score = NA))
  

## Create a wrapper function that builds a data frame from responses
analyze_lyrics_claude <- function(lyrics_df) {
  lyrics_df |>
    mutate(sentiment_data = map(text, ~score_claude_llm(.x))) |> 
    unnest_wider(sentiment_data)
}

lyrics_2pac_claude_sonnet_5 <- lyrics_raw_2pac |>
  analyze_lyrics_claude()

lyrics_2pac_claude_sonnet_5


plot_lyrics_2pac_claude_sonnet_5 <- lyrics_2pac_claude_sonnet_5 |> 
  mutate(section = factor(section, levels = section_order_2pac)) |> 
  group_by(section, emotion_label) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  mutate(share = count / sum(count)) |> 
  ggplot(aes(x = section, y = share, fill = emotion_label)) +
  geom_bar(stat = "identity", position = "fill", show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      emotion_label, 
      "\nShare: ", round(share * 100, 0), "%",
      "\nConfidence: ", round(avg_conf, 2), ""
    )), 
    position = position_fill(vjust = 0.5), 
    color = "white", 
    size = 3,
    lineheight = 0.8 
  ) +
  labs(title = "Emotional Trajectory (Claude Sonnet 5)",
       x = "Section",
       y = "Proportion of Emotions") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray30", linewidth = 0.1),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )


ggsave(
  filename = "plots/05-plot_lyrics_2pac_claude-sonnet_5.png",
  plot = plot_lyrics_2pac_claude_sonnet_5,
  width = 15,
  height = 10,
  dpi = 300
)



# Changes, Kino
## 2.1 Lexicon based sentiments

## Access to RuSentiLex lexicon
rusentilex_data <- hash_rusentilex_2017
head(rusentilex_data)

lyrics_raw_kino <- read_sheet(lyrics_id, "Changes Kino")

lyrics_tokens_kino <- lyrics_raw_kino |> 
  unnest_tokens(token, text)

# Clean the lexicon: resolve duplicates by picking the strongest sentiment
sentiment_map <- c("positive" = 1, "negative" = -1, "neutral" = 0)

lexicon_clean <- rusentilex_data |> 
  filter(source == "opinion") |> 
  mutate(val = sentiment_map[sentiment]) |> 
  group_by(token) |> 
  slice_max(abs(val), with_ties = FALSE) |> 
  ungroup() |> 
  select(token, val)


# Define Valence Shifters
valence_shifters <- tibble(
  token = c("не", "нет", "ни", "без", "никогда", "очень", "сильно", "слишком", "весьма"),
  shift = c(rep(-1, 5), rep(1.5, 4))
)


lyrics_scored_kino <- lyrics_tokens_kino |> 
  ungroup() |> 
  rename(lemma = 3) |> 
  left_join(lexicon_clean, by = c("lemma" = "token")) |> 
  group_by(section) |> 
  mutate(
    prev_token = lag(lemma),
    shifter = if_else(prev_token %in% valence_shifters$token, prev_token, NA_character_)
  ) |> 
  left_join(valence_shifters, by = c("shifter" = "token")) |> 
  mutate(
    final_score = case_when(
      !is.na(shift) ~ val * shift,
      !is.na(val)   ~ val,
      TRUE          ~ 0
    )
  ) |> 
  ungroup()


section_sentiment_kino <- lyrics_scored_kino |>
  group_by(section) |>
  summarize(
    total_sentiment = sum(final_score, na.rm = TRUE),
    lexical_density = mean(final_score != 0, na.rm = TRUE)
  )
section_sentiment_kino


lyrics_scored_kino |> 
  filter(final_score > 0)

section_sentiment_kino$total_sentiment


section_sentiment_kino |>
  mutate(total_sentiment = percent(total_sentiment, accuracy = 0.1)) |> 
  mutate(lexical_density = percent(lexical_density, accuracy = 0.1)) |> 
  gt() |> 
  cols_label(
    section = "Section",
    total_sentiment = "Sentiment Score",
    lexical_density = "Emotional Frequency"
  ) |>
  tab_header(
    title = "Перемен, Кино sentiment scores: RuSentiLex lexicon",
    subtitle = "Only one verse is scored but is also false positive"
  ) |> 
  # Header styling
  tab_style(
    style = list(cell_fill(color = "#2a9d8f"), cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) |> 
  # Base Body Styling (all rows)
  tab_style(
    style = cell_fill(color = "#cbe8f5"),
    locations = cells_body()
  ) |> 
  # Highlighting the scored lines
  tab_style(
    style = cell_fill(color = "#f4a261"),
    locations = cells_body(rows = total_sentiment != "0.0%")
  ) |> 
  cols_align(align = "center", columns = everything()) |> 
  gtsave("tables/02-table_changes_kino_sentiment_scores_rusentilex.png")



## 2.2 Hugging Face
Sys.getenv("HF_TOKEN")

use_condaenv("r-reticulate", required = TRUE)

# Import Python infrastructure modules
os <- import("os")
hf_hub <- import("huggingface_hub")
transformers <- import("transformers")

hf_token <- Sys.getenv("HF_TOKEN")
if (hf_token == "") {
  stop("Error: HG_TOKEN not found! Please check your .Renviron file.")
}

client <- hf_hub$InferenceClient(
  provider = "hg-inference",
  api_key = hf_token
)

# Modify your pipeline call slightly if needed:
classifier <- transformers$pipeline(
  task = "text-classification",
  model = "Aniemore/rubert-base-emotion-russian-cedr-m7",
  client = client,
  top_k = 1L # Forces the model to only return the most confident emotion
)


lyrics_kino_scored_cedr_m7 <- lapply(lyrics_raw_kino$text, function(line) {
  print(paste("Processing line:", line))
  tryCatch({
    py_res <- classifier(line)
    print(py_res)
    # Extracting directly from the pipeline output format
    flat_vector <- unlist(py_to_r(py_res))
    
    tibble(
      emotion_label = str_to_title(py_res[[1]][[1]]$label),
      confidence_score = as.numeric(py_res[[1]][[1]]$score)
      )
    }, error = function(e) {
  # Return a dummy row if a line fails
  tibble(emotion_label = "Neutral", confidence_score = 0)
      })
  }) |> bind_rows()

lyrics_kino_scored_cedr_m7


lyrics_kino_scored_cedr_m7_final <- lyrics_raw_kino |> 
  bind_cols(lyrics_kino_scored_cedr_m7)

lyrics_kino_scored_cedr_m7


lyrics_kino_scored_cedr_m7_final$section


section_order_kino <- c("Куплет 1", "Припев 1", 
                   "Куплет 2", "Припев 2", 
                   "Куплет 3", "Припев 3", "Припев 4")


# Calculate emotion frequency per section
plot_kino_emotional_labels_conf_cedr_m7 <- lyrics_kino_scored_cedr_m7_final |>
  ggplot(aes(x = line, y = confidence_score)) +
  # Add a background fill to represent the 'Uncertainty Zone' (below 0.5)
  annotate("rect",
           xmin = -Inf, 
           xmax = Inf, 
           ymin = 0,
           ymax = 0.5,
           fill = "#ffcccc",
           alpha = 0.3) +
  geom_line(color = "#2c3e50",
            linewidth = 0.8) +
  geom_point(color = "#e74c3c", size = 2) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
    ) +
  scale_x_continuous(
    limits = c(1, 38),
    breaks = seq(1, 38, 1),
    expand = c(0, 0.3)
    ) +
  labs(
    title = "The 'Optimism' Trap: Model Confidence Pulse",
    subtitle = "Model incorrectly classifies every line as 'Optimism' with consistently low confidence",
    x = "Line Number",
    y = "Confidence Score"
  ) + 
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray30", linewidth = 0.1),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )


plot_kino_emotional_labels_conf_cedr_m7

ggsave(
  filename = "plots/06-plot_kino_emotional_labels_conf_cedr_m7.png",
  plot = plot_kino_emotional_labels_conf_cedr_m7,
  width = 15,
  height = 10,
  dpi = 300
)


## 2.3 Local LLMs

results_kino_llama3_2_latest <- lyrics_raw_kino |>
  mutate(sentiment_data = map(text, ~score_local_llm(.x))) |>
  unnest_wider(sentiment_data)

results_kino_llama3_2_latest |> View()

plot_kino_lyrics_llama3_2_latest <- results_kino_llama3_2_latest |> 
  mutate(section = factor(section, levels = section_order_kino)) |> 
  group_by(section, emotion_label) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  mutate(share = count / sum(count)) |> 
  ggplot(aes(x = section, y = share, fill = emotion_label)) +
  geom_bar(stat = "identity", position = "fill", show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      emotion_label, 
      "\nShare: ", round(share * 100, 0), "%",
      "\nConfidence: ", round(avg_conf, 2), ""
    )), 
    position = position_fill(vjust = 0.5), 
    color = "white", 
    size = 3,
    lineheight = 0.8 
  ) +
  labs(title = "Emotional Trajectory: Перемен, Кино",
       subtitle = "Model: llama3.2:latest",
       x = "Section",
       y = "Proportion of Emotions"
       ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray30", linewidth = 0.1),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_kino_lyrics_llama3_2_latest

ggsave(
  filename = "plots/07-plot_kino_lyrics_llama3_2_latest.png",
  plot = plot_kino_lyrics_llama3_2_latest,
  width = 15,
  height = 10,
  dpi = 300
)


### phi4-mini:latest
results_kino_phi4_mini_latest <- lyrics_raw_kino |>
  mutate(batch_id = ceiling(row_number() / 10)) |> 
  group_split(batch_id) |> 
  map_df(~.x |> 
           mutate(sentiment_data = map(text, ~score_local_llm(.x, model = phi4_mini_latest))) |>
           unnest_wider(sentiment_data)) |> 
  select(-batch_id)

results_kino_phi4_mini_latest


plot_lyrics_kino_phi4_mini_latest <- results_kino_phi4_mini_latest |> 
  mutate(section = factor(section, levels = section_order_kino)) |> 
  group_by(section, emotion_label) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  mutate(share = count / sum(count)) |> 
  ggplot(aes(x = section, y = share, fill = emotion_label)) +
  geom_bar(stat = "identity", position = "fill", show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      emotion_label, 
      "\nShare: ", round(share * 100, 0), "%",
      "\nConfidence: ", round(avg_conf, 2), ""
    )), 
    position = position_fill(vjust = 0.5), 
    color = "white", 
    size = 3,
    lineheight = 0.8 
  ) +
  labs(title = "Emotional Trajectory: Перемен, Кино",
       subtitle = "Model: phi4-mini:latest",
       x = "Section",
       y = "Proportion of Emotions"
       ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray30", linewidth = 0.1),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_lyrics_kino_phi4_mini_latest

ggsave(
  filename = "plots/08-plot_lyrics_kino_phi4_mini_latest.png",
  plot = plot_lyrics_kino_phi4_mini_latest,
  width = 15,
  height = 10,
  dpi = 300
)

# 2.4 Claude Sonnet 5 Model
lyrics_kino_claude_sonnet_5 <- lyrics_raw_kino |>
  analyze_lyrics_claude()

lyrics_kino_claude_sonnet_5


plot_lyrics_kino_claude_sonnet_5 <- lyrics_kino_claude_sonnet_5 |> 
  mutate(section = factor(section, levels = section_order_kino)) |> 
  group_by(section, emotion_label) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  mutate(share = count / sum(count)) |> 
  ggplot(aes(x = section, y = share, fill = emotion_label)) +
  geom_bar(stat = "identity", position = "fill", show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      emotion_label, 
      "\nShare: ", round(share * 100, 0), "%",
      "\nConfidence: ", round(avg_conf, 2), ""
    )), 
    position = position_fill(vjust = 0.5), 
    color = "white", 
    size = 3,
    lineheight = 0.8 
  ) +
  labs(title = "Emotional Trajectory: Перемен, Кино",
       subtitle = "Model: Claude Sonnet",
       x = "Section",
       y = "Proportion of Emotions"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray30", linewidth = 0.1),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_lyrics_kino_claude_sonnet_5

ggsave(
  filename = "plots/09-plot_lyrics_kino_claude_sonnet_5.png",
  plot = plot_lyrics_kino_claude_sonnet_5,
  width = 15,
  height = 10,
  dpi = 300
)




