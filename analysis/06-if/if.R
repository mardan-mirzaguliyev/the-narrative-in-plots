library(tidyverse)
library(googlesheets4)
library(tidytext)
library(scales)


gs4_deauth()


if_id <- "1ojm1BP4nNaV5R2b3-6ryl-R7dia-xVk52UVdz17bH9M"


if_raw <- read_sheet(if_id, "If")
if_raw


if_tokens <- if_raw |> 
  unnest_tokens(word, Text)

# NRC
nrc <- get_sentiments("nrc")

# Check composite key uniqueness
if_raw |> count(Verse, Line) |> filter(n > 1)

if_nrc <- if_tokens |> 
  left_join(nrc, by = "word", relationship = "many-to-many") |> 
  filter(!is.na(sentiment)) |> 
  filter(!sentiment %in% c("positive", "negative")) |> 
  count(Verse, Line, sentiment) |> 
  slice_max(n, by = c(Verse, Line), with_ties = FALSE) |> 
  select(Verse, Line, sentiment) |> 
  right_join(
    if_raw |> select(Verse, Line, Text),
    by = c("Verse", "Line")
  ) |> 
  mutate(Sentiment = str_to_title(replace_na(sentiment, "neutral"))) |> 
  arrange(Verse, Line) |>
  select(Verse, Line, Text, Sentiment)


if_nrc


sentiment_colors <- c("Neutral" = "#adb5bd",
                      "Trust" = "#2a9d8f", "Anticipation" = "#e9c46a", 
                      "Fear" = "#264653", "Joy" = "#f4a261", 
                      "Anger" = "#e76f51", "Sadness" = "#6d6875", 
                      "Surprise" = "#84a59d", "Disgust" = "#bc6c25")


plot_if_sections_nrc <- if_nrc |> 
  group_by(Verse, Sentiment) |> 
  summarize(n = n(), .groups = "drop") |> 
  group_by(Verse) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  select(-n) |>
  mutate(Sentiment = factor(Sentiment,
                            levels = c("Joy", "Trust", "Anticipation", "Surprise",
                                       "Anger", "Disgust", "Fear", "Sadness", "Neutral"))) |> 
  ggplot(aes(x = Verse, y = Sentiment, fill = prop)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = percent(prop, accuracy = 0.1)),
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  scale_x_continuous(breaks = seq(1, 4, by = 1), limits = c(0.5, 4.5)) +
  scale_fill_gradient2(
    low = "#f7f7f7", mid = "#a8d8d2", high = "#2a9d8f",
    midpoint = 0.08, labels = percent
  ) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "Emotion Profile Across Verses",
    subtitle = "NRC lexicon: proportion of sentences by dominant emotion",
    caption = "Data: If, Rudyard Kipling (1910)",
    x = NULL, y = NULL, fill = "Proportion"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 12, hjust = 0.5),
    axis.text         = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position = "none"
  )

plot_if_sections_nrc


ggsave(
  filename = "plots/01-plot_if_sections_nrc.png",
  plot = plot_if_sections_nrc,
  width = 15,
  height = 10,
  dpi = 300
)


plot_if_lines_nrc <- if_nrc |> 
  mutate(Sentiment = factor(Sentiment,
                            levels = c("Joy", "Trust", "Anticipation", "Surprise",
                                       "Anger", "Disgust", "Fear", "Sadness", "Neutral"))) |> 
  ggplot(aes(x = Line, y = Sentiment, fill = Sentiment)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_x_continuous(breaks = seq(1, 32, by = 1), limits = c(0.5, 32.5)) +
  scale_fill_manual(values = sentiment_colors) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "Emotion Profile Across Lines",
    subtitle = "NRC lexicon: Sentence Level Sentiments",
    caption = "Data: If, Rudyard Kipling (1910)",
    x = NULL, y = NULL, fill = "Proportion"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 12, hjust = 0.5),
    axis.text         = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position = "none"
  )


plot_if_lines_nrc


ggsave(
  filename = "plots/02-plot_if_lines_nrc.png",
  plot = plot_if_lines_nrc,
  width = 15,
  height = 10,
  dpi = 300
)



# Local model - gemma4:latest
if (!dir.exists("data")) dir.create("data")
gemma4_latest <- "gemma4:latest"


if_gemma4_latest <- if_raw |> 
  get_or_run_local(Text, output_name = "data/if", model = gemma4_latest)

if_gemma4_latest <- if_gemma4_latest |> 
  mutate(Sentiment = str_to_title(replace_na(sentiment, "neutral"))) |> 
  select(-sentiment) |> 
  relocate(Sentiment, .before = confidence_score)

if_gemma4_latest


plot_if_sections_gemma4_latest <- if_gemma4_latest |>
  group_by(Verse, Sentiment) |> 
  summarize(n = n(), .groups = "drop") |> 
  group_by(Verse) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  select(-n) |>
  mutate(Sentiment = factor(Sentiment,
                            levels = c("Joy", "Trust", "Anticipation", "Surprise",
                                       "Anger", "Disgust", "Fear", "Sadness", "Neutral"))) |> 
  ggplot(aes(x = Verse, y = Sentiment, fill = prop)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = percent(prop, accuracy = 0.1)),
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  scale_x_continuous(breaks = seq(1, 4, by = 1), limits = c(0.5, 4.5)) +
  scale_fill_gradient2(
    low = "#f7f7f7", mid = "#a8d8d2", high = "#2a9d8f",
    midpoint = 0.08, labels = percent
  ) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "Emotion Profile Across Verses",
    subtitle = "Gemma4: proportion of sentences by dominant emotion",
    caption = "Data: If, Rudyard Kipling (1910)",
    x = NULL, y = NULL, fill = "Proportion"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 12, hjust = 0.5),
    axis.text         = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position = "none"
  )

plot_if_sections_gemma4_latest


ggsave(
  filename = "plots/03-plot_if_sections_gemma4_latest.png",
  plot = plot_if_sections_gemma4_latest,
  width = 15,
  height = 10,
  dpi = 300
)


plot_if_lines_gemma4_latest <- if_gemma4_latest |> 
  mutate(Sentiment = factor(Sentiment,
                            levels = c("Joy", "Trust", "Anticipation", "Surprise",
                                       "Anger", "Disgust", "Fear", "Sadness", "Neutral"))) |> 
  ggplot(aes(x = Line, y = Sentiment, fill = Sentiment)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_x_continuous(breaks = seq(1, 32, by = 1), limits = c(0.5, 32.5)) +
  scale_fill_manual(values = sentiment_colors) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "Emotion Profile Across Lines",
    subtitle = "Gemma4: Sentence Level Sentiments",
    caption = "Data: If, Rudyard Kipling (1910)",
    x = NULL, y = NULL, fill = "Proportion"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 12, hjust = 0.5),
    axis.text         = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position = "none"
  )


plot_if_lines_gemma4_latest


ggsave(
  filename = "plots/04-plot_if_lines_gemma4_latest.png",
  plot = plot_if_lines_gemma4_latest,
  width = 15,
  height = 10,
  dpi = 300
)


