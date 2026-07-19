library(rulexicon)
library(googlesheets4)
library(tidytext)
library(tidyverse)
library(gt)



# Changes, Kino

## 2.1 Lexicon based sentiments
gs4_auth()

lyrics_id <- "13dM_5vWQGdAeRI-thE6nbzlZIbvbZ8dw9eCu3MsP9Kk"
changes_kino_raw <- read_sheet(lyrics_id, "Changes Kino")


## Access to RuSentiLex lexicon
rusentilex_data <- hash_rusentilex_2017
head(rusentilex_data)


changes_kino_rusentilex <- changes_kino_raw |> 
  unnest_tokens(token, text) |> 
  left_join(rusentilex_data, by = "token") |> 
  mutate(is_scored = !is.na(sentiment))

changes_kino_rusentilex |>
  filter(is_scored) |>
  select(section, line, token, sentiment) |>
  arrange(section, line) |>
  gt() |>
  tab_header(
    title = "Перемен, Кино sentiment scores: RuSentiLex lexicon",
    subtitle = "Tokens matched, with lexicon sentiment label"
  ) |>
  cols_label(
    section   = "Section",
    line      = "Line",
    token     = "Token",
    sentiment = "Sentiment"
  ) |>
  tab_style(
    style = list(cell_fill(color = "#2a9d8f"), cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels(everything())
  ) |>
  tab_style(
    style = cell_fill(color = "#2a9d8f"),
    locations = cells_body(columns = sentiment, rows = sentiment == "positive")
  ) |>
  tab_style(
    style = cell_fill(color = "#bd1515"),
    locations = cells_body(columns = sentiment, rows = sentiment == "negative")
  ) |>
  tab_style(
    style = cell_fill(color = "#cbe8f5"),
    locations = cells_body(columns = sentiment, rows = !sentiment %in% c("positive", "negative"))
  ) |>
  tab_options(
    table.font.size = px(15),
    heading.title.font.size = px(22),
    column_labels.font.weight = "bold",
    data_row.padding = px(10)
  ) |> 
  gtsave("tables/02-table_changes_kino_sentiment_scores_rusentilex.png")



## 2.2 Local LLMs - Used for both lyrics

### llama3.2:latest

section_order_kino <- c("Куплет 1", "Припев 1",
                        "Куплет 2", "Припев 2", 
                        "Куплет 3", "Припев 3",
                        "Припев 4")


changes_kino_llama3_2_latest <- changes_kino_raw |> 
  get_or_run_local(text, output_name = "changes_kino_local", model = "llama3.2:latest")

changes_kino_llama3_2_latest



plot_changes_kino_llama3_2_latest <- changes_kino_llama3_2_latest |> 
  mutate(section = factor(section, levels = section_order_kino)) |> 
  group_by(section, sentiment) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  group_by(section) |>
  mutate(share = count / sum(count)) |> 
  ungroup() |> 
  ggplot(aes(x = section, y = share, fill = sentiment)) +
  geom_bar(stat = "identity", position = "fill", show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      sentiment, 
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
       caption = "Data: Перемен, Кино lyrics (1989)") +
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

plot_changes_kino_llama3_2_latest


ggsave(
  filename = "plots/06-plot_changes_kino_llama3_2_latest.png",
  plot = plot_changes_kino_llama3_2_latest,
  width = 15,
  height = 10,
  dpi = 300
)


### phi4-mini:latest
phi_4_mini_latest = "phi4-mini:latest"


changes_kino_phi4_mini_latest <- changes_kino_raw |>
  get_or_run_local(text, output_name = "changes_kino_local", model = phi_4_mini_latest)

changes_kino_phi4_mini_latest


plot_changes_kino_phi4_mini_latest <- changes_kino_phi4_mini_latest |> 
  mutate(section = factor(section, levels = section_order_kino)) |> 
  group_by(section, sentiment) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  group_by(section) |>
  mutate(share = count / sum(count)) |> 
  ungroup() |> 
  ggplot(aes(x = section, y = share, fill = sentiment)) +
  geom_bar(stat = "identity", position = "fill", show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      sentiment, 
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
       caption = "Data: Перемен, Кино lyrics (1989)") +
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

plot_changes_kino_phi4_mini_latest


ggsave(
  filename = "plots/07-plot_changes_kino_phi4_mini_latest.png",
  plot = plot_changes_kino_phi4_mini_latest,
  width = 15,
  height = 10,
  dpi = 300
)


## 2.3 Hugging Face
transformers <- import("transformers")

classifier <- transformers$pipeline(
  "text-classification",
  model = "Aniemore/rubert-base-emotion-russian-cedr-m7",
  top_k = NULL
)

## ---- flatten one result (one line's worth of emotion scores) --------------
flatten_emotion_result <- function(result_item) {
  map_dfr(result_item, ~ tibble(label = .x$label, score = .x$score))
}

## ---- batch runner with caching, mirroring get_or_run_hf() -----------------
get_or_run_hf_local <- function(data, text_col, output_name,
                                model = "Aniemore/rubert-base-emotion-russian-cedr-m7") {
  cache_path <- paste0(output_name, "_", gsub("/", "-", model), "_local.rds")
  
  if (file.exists(cache_path)) {
    return(readRDS(cache_path))
  }
  
  texts <- data[[text_col]]
  raw_results <- classifier(texts, top_k = NULL)
  
  emotion_scores <- map2_dfr(
    raw_results, seq_along(raw_results),
    ~ flatten_emotion_result(.x) |> mutate(row_id = .y)
  )
  
  out <- data |>
    mutate(row_id = row_number()) |>
    left_join(emotion_scores, by = "row_id") |>
    select(-row_id)
  
  saveRDS(out, cache_path)
  out
}

## ---- run it on the actual song ---------------------------------------------
changes_kino_cedr <- get_or_run_hf_local(
  changes_kino_raw, text_col = "text", output_name = "changes_kino"
)


changes_kino_cedr <- changes_kino_cedr |> 
  rename(sentiment = label,
         confidence_score = score)

changes_kino_cedr <- changes_kino_cedr |>
  group_by(section, line) |>
  slice_max(confidence_score, n = 1, with_ties = FALSE) |>
  ungroup()

changes_kino_cedr


plot_changes_kino_cedr <- changes_kino_cedr |>
  mutate(section = factor(section, levels = section_order_kino)) |> 
  group_by(section, sentiment) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  group_by(section) |>
  mutate(share = count / sum(count)) |> 
  ungroup() |> 
  ggplot(aes(x = section, y = share, fill = sentiment)) +
  geom_bar(stat = "identity", position = "fill", show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      sentiment, 
      "\nShare: ", round(share * 100, 0), "%",
      "\nConfidence: ", round(avg_conf, 2), ""
    )), 
    position = position_fill(vjust = 0.5), 
    color = "white", 
    size = 3,
    lineheight = 0.8 
  ) +
  labs(title = "Emotional Distribution",
       subtitle = "Model: russian-cedr-m7",
       x = NULL,
       y = NULL,
       caption = "Data: Перемен, Кино lyrics (1989)") +
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

plot_changes_kino_cedr


ggsave(
  filename = "plots/08-plot_changes_kino_cedr.png",
  plot = plot_changes_kino_cedr,
  width = 15,
  height = 10,
  dpi = 300
)



## 2.4 Claude API
changes_kino_sonnet_5 <- changes_kino_raw |>
  get_or_run_claude_batch(text, output_name = "changes_kino", model = "claude-sonnet-5")


plot_changes_kino_sonnet_5 <- changes_kino_sonnet_5 |> 
  mutate(section = factor(section, levels = section_order_kino)) |> 
  group_by(section, sentiment) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  group_by(section) |>
  mutate(share = count / sum(count)) |> 
  ungroup() |> 
  ggplot(aes(x = section, y = share, fill = sentiment)) +
  geom_bar(stat = "identity", position = "fill", show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      sentiment, 
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
       caption = "Data: Перемен, Кино lyrics (1989)") +
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

plot_changes_kino_sonnet_5


ggsave(
  filename = "plots/09-plot_changes_kino_sonnet_5.png",
  plot = plot_changes_kino_sonnet_5,
  width = 15,
  height = 10,
  dpi = 300
)


