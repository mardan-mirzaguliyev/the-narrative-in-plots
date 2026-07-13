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
library(writexl)


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
lyrics_scored_2pac_syuzhet <- lyrics_raw_2pac |>
  mutate(syuzhet_score = get_sentiment(text, method = "syuzhet"))
lyrics_scored_2pac_syuzhet 

# Normalise to –1 / +1
# syuzhet scores are unbounded, so we divide by the observed maximum
# to get a scale anchored between –1 and +1
max_abs <- max(abs(lyrics_scored_2pac_syuzhet$syuzhet_score))
max_abs

lyrics_scored_2pac_syuzhet <- lyrics_scored_2pac_syuzhet |> 
  mutate(normalized_score = syuzhet_score / max_abs)
lyrics_scored_2pac_syuzhet


section_order_2pac <- c("Verse 1", "Chorus 1", "Verse 2", "Chorus 2", "Interlude", "Verse 3", "Chorus 3")


## Most negative and most positive scores
extremes <- 
  lyrics_scored_2pac_syuzhet |> 
  group_by(section) |> 
  summarize(
    section_min = min(normalized_score),
    section_max = max(normalized_score))

## Build a coloring logic for the points
extremes_plot <- extremes|> 
  filter(section_min != 0) |> 
  mutate(min_color = ifelse(section_min < 0, "#bd1515", "#2a9d8f"))


plot_2pac_scores_syuzhet <- lyrics_scored_2pac_syuzhet |>
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
    title    = "Sentiment distributions",
    subtitle = "Lexicon method: syuzhet package",
    x        = NULL,
    y        = NULL,
    caption  = "Data: Changes, 2Pac lyrics"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_2pac_scores_syuzhet


ggsave(
  filename = "plots/01-plot_2pac_scores_syuzhet.png",
  plot = plot_2pac_scores_syuzhet,
  width = 15,
  height = 10,
  dpi = 300
)


lyrics_scored_2pac_syuzhet |> 
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
  gtsave("tables/01-table_chorus_3_scored_lines_syuzhet.png")


## 1.2 Local LLMs - Used for both lyrics

### llama3.2:latest
results_2pac_llama3_2_latest <- lyrics_raw_2pac |>
  mutate(sentiment_data = map(text, ~score_local_llm(.x))) |>
  unnest_wider(sentiment_data)


plot_2pac_llama3_2_latest <- results_2pac_llama3_2_latest |> 
  mutate(section = factor(section, levels = section_order_2pac)) |> 
  group_by(section, emotion_label) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  group_by(section) |>
  mutate(share = count / sum(count)) |> 
  ungroup() |> 
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
  labs(title = "Emotional Distribution",
       subtitle = "Model: llama3.2:latest",
       x = NULL,
       y = NULL,
       caption = "Data: Changes, 2Pac lyrics") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_2pac_llama3_2_latest


ggsave(
  filename = "plots/02-plot_2pac_llama3_2_latest.png",
  plot = plot_2pac_llama3_2_latest,
  width = 15,
  height = 10,
  dpi = 300
)


results_2pac_phi4_mini_latest <- lyrics_raw_2pac |>
  mutate(batch_id = ceiling(row_number() / 10)) |> 
  group_split(batch_id) |> 
  map_df(~.x |> 
           mutate(sentiment_data = map(text, ~score_local_llm(.x, model = phi4_mini_latest))) |>
           unnest_wider(sentiment_data)) |> 
  select(-batch_id)

results_2pac_phi4_mini_latest


plot_2pac_phi4_mini_latest <- results_2pac_phi4_mini_latest |> 
  mutate(section = factor(section, levels = section_order_2pac)) |> 
  group_by(section, emotion_label) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  group_by(section) |>
  mutate(share = count / sum(count)) |> 
  ungroup() |> 
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
  labs(title = "Emotional Distribution",
       subtitle = "Model: phi4-mini:latest",
       x = NULL,
       y = NULL,
       caption = "Data: Changes, 2Pac lyrics") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_2pac_phi4_mini_latest


ggsave(
  filename = "plots/03-plot_2pac_phi4_mini_latest.png",
  plot = plot_2pac_phi4_mini_latest,
  width = 15,
  height = 10,
  dpi = 300
)


## 1.3 Hugging Face
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


# Pipeline call
classifier <- transformers$pipeline(
  task = "text-classification",
  model = "cardiffnlp/twitter-roberta-base-emotion",
  client = client,
  top_k = 1L, # Forces the model to only return the most confident emotion
)


results_2pac_roberta <- lapply(seq_along(lyrics_raw_2pac$text), function(i) {
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


results_2pac_final_roberta <- lyrics_raw_2pac |> 
  bind_cols(results_2pac_roberta)

results_2pac_final_roberta


# Calculate emotion frequency per section
plot_2pac_scored_roberta <- results_2pac_final_roberta |>
  mutate(section = factor(section, levels = section_order_2pac)) |> 
  group_by(section, emotion_label) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  group_by(section) |>
  mutate(share = count / sum(count)) |> 
  ungroup() |> 
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
  labs(title = "Emotional Distribution",
       subtitle = "Model: Twitter-roBERTa",
       x = NULL,
       y = NULL,
       caption = "Data: Changes, 2Pac lyrics") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_2pac_scored_roberta


ggsave(
  filename = "plots/04-plot_2pac_scored_roberta.png",
  plot = plot_2pac_scored_roberta,
  width = 15,
  height = 10,
  dpi = 300
)


## 1.4 Claude API
results_2pac_claude_sonnet_5 <- lyrics_raw_2pac |>
  analyze_lyrics_claude()

results_2pac_claude_sonnet_5


plot_2pac_claude_sonnet_5 <- results_2pac_claude_sonnet_5 |> 
  mutate(section = factor(section, levels = section_order_2pac)) |> 
  group_by(section, emotion_label) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  group_by(section) |>
  mutate(share = count / sum(count)) |> 
  ungroup() |> 
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
  labs(title = "Emotional Distribution",
       subtitle = "Model: Claude Sonnet 5",
       x = NULL,
       y = NULL,
       caption = "Data: Changes, 2Pac lyrics") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_2pac_claude_sonnet_5


ggsave(
  filename = "plots/05-plot_2pac_claude_sonnet_5.png",
  plot = plot_2pac_claude_sonnet_5,
  width = 15,
  height = 10,
  dpi = 300
)



## Scored objects
lyrics_scored_2pac_syuzhet |>
  select(-syuzhet_score) |> 
  bind_cols(
    results_2pac_llama3_2_latest |> 
      select(llama3_label = emotion_label, llama3_conf = confidence_score),
    results_2pac_phi4_mini_latest |> 
      select(phi4_label = emotion_label, phi4_conf = confidence_score),
    results_2pac_final_roberta |> 
      select(roberta_label = emotion_label, roberta_conf = confidence_score),
    results_2pac_claude_sonnet_5 |> 
      select(claude_label = emotion_label, claude_conf = confidence_score)
  ) |>
  mutate(normalized_score = percent(normalized_score, accuracy = 0.1)) |> 
  gt() |> 
  cols_label(
    section = "Section",
    line = "Line Number",
    text = "Line Text",
    normalized_score = "Syzhet Score",
    llama3_label = "Llama 3 Label",
    llama3_conf = "Llama 3 conf",
    phi4_label = "Phi 4",
    phi4_conf = "Ph 4 Conf",
    roberta_label = "Twitter RoBERTa",
    roberta_conf = "Twitter RoBERTa conf",
    claude_label = "Sonnet 5 Label",
    claude_conf = "Sonnet 5 conf"
  ) |>
  tab_header(
    title = "Line by Line ",
    subtitle = "Labels differ from model to model"
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
  gtsave("tables/02-scores_2pac_merged.png")



# Changes, 2Pac - Combined

lyrics_scored_2pac_syuzhet |>
  select(-syuzhet_score) |> 
  bind_cols(
    results_2pac_llama3_2_latest |> 
      select(llama3_label = emotion_label, llama3_conf = confidence_score),
    results_2pac_phi4_mini_latest |> 
      select(phi4_label = emotion_label, phi4_conf = confidence_score),
    results_2pac_final_roberta |> 
      select(roberta_label = emotion_label, roberta_conf = confidence_score),
    results_2pac_claude_sonnet_5 |> 
      select(claude_label = emotion_label, claude_conf = confidence_score)
  ) |>
  mutate(normalized_score = percent(normalized_score, accuracy = 0.1)) |> 
  write_xlsx("tables/changes_2pac.xlsx")


