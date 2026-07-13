# Changes, Kino
## 2.1 Lexicon based sentiments

gs4_deauth()

lyrics_id <- "13dM_5vWQGdAeRI-thE6nbzlZIbvbZ8dw9eCu3MsP9Kk"
lyrics_raw_kino <- read_sheet(lyrics_id, "Changes Kino")


lyrics_tokens_kino <- lyrics_raw_kino |> 
  unnest_tokens(token, text)


## Access to RuSentiLex lexicon
rusentilex_data <- hash_rusentilex_2017
head(rusentilex_data)


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


lyrics_scored_kino_rulexicon <- lyrics_tokens_kino |> 
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


section_sentiment_kino_rulexicon <- lyrics_scored_kino_rulexicon |>
  group_by(section) |>
  summarize(
    total_sentiment = sum(final_score, na.rm = TRUE),
    lexical_density = mean(final_score != 0, na.rm = TRUE)
  )
section_sentiment_kino_rulexicon


lyrics_scored_kino_rulexicon |> 
  filter(final_score > 0)


section_sentiment_kino_rulexicon |>
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
    subtitle = "Only one verse is scored, but is false positive"
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


## 2.2 Local LLMs

### llama3.2:latest
results_kino_llama3_2_latest <- lyrics_raw_kino |>
  mutate(sentiment_data = map(text, ~score_local_llm(.x))) |>
  unnest_wider(sentiment_data)

results_kino_llama3_2_latest


section_order_kino <- c("Куплет 1",
                        "Припев 1", 
                        "Куплет 2", 
                        "Припев 2", 
                        "Куплет 3", 
                        "Припев 3",
                        "Припев 4")


plot_kino_llama3_2_latest <- results_kino_llama3_2_latest |> 
  mutate(section = factor(section, levels = section_order_kino)) |> 
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
       caption = "Data: Перемен, Кино lyrics") +
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

plot_kino_llama3_2_latest


ggsave(
  filename = "plots/06-plot_kino_llama3_2_latest.png",
  plot = plot_kino_llama3_2_latest,
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


plot_kino_phi4_mini_latest <- results_kino_phi4_mini_latest |> 
  mutate(section = factor(section, levels = section_order_kino)) |> 
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
       caption = "Data: Перемен, Кино lyrics") +
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

plot_kino_phi4_mini_latest


ggsave(
  filename = "plots/07-plot_kino_phi4_mini_latest.png",
  plot = plot_kino_phi4_mini_latest,
  width = 15,
  height = 10,
  dpi = 300
)


## 2.3 Hugging Face
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


# Calculate emotion frequency per section
plot_kino_emotional_labels_conf_cedr_m7 <- lyrics_kino_scored_cedr_m7_final |> 
  mutate(section = factor(section, levels = section_order_kino)) |> 
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
       subtitle = "Model: Aniemore/rubert-base-emotion-russian-cedr-m7",
       x = NULL,
       y = NULL,
       caption = "Data: Перемен, Кино lyrics") +
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

plot_kino_emotional_labels_conf_cedr_m7


ggsave(
  filename = "plots/08-plot_kino_emotional_labels_conf_cedr_m7.png",
  plot = plot_kino_emotional_labels_conf_cedr_m7,
  width = 15,
  height = 10,
  dpi = 300
)


# 2.4 Claude Sonnet 5 Model
kino_claude_sonnet_5 <- lyrics_raw_kino |>
  analyze_lyrics_claude()

kino_claude_sonnet_5


plot_kino_claude_sonnet_5 <- kino_claude_sonnet_5 |> 
  mutate(section = factor(section, levels = section_order_kino)) |> 
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
       caption = "Data: Перемен, Кино lyrics") +
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

plot_kino_claude_sonnet_5


ggsave(
  filename = "plots/09-plot_kino_claude_sonnet_5.png",
  plot = plot_kino_claude_sonnet_5,
  width = 15,
  height = 10,
  dpi = 300
)



# Changes, Kino - Combined

results_kino_llama3_2_latest |> 
  rename(llama3_label = emotion_label, llama3_conf = confidence_score) |> 
  bind_cols(
    results_kino_phi4_mini_latest |> 
      select(phi4_label = emotion_label, phi4_conf = confidence_score),
    lyrics_kino_scored_cedr_m7_final |> 
      select(roberta_label = emotion_label, roberta_conf = confidence_score),
    kino_claude_sonnet_5 |> 
      select(claude_label = emotion_label, claude_conf = confidence_score)
  ) |> 
  write_xlsx("tables/changes_kino.xlsx")

