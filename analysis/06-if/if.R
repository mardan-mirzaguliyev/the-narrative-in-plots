library(tidyverse)
library(googlesheets4)
library(tidytext)
library(scales)
library(reticulate)
library(writexl)
library(janitor)
library(ggalluvial)
library(ggrepel)
library(gt)


gs4_deauth()

if_id <- "1ojm1BP4nNaV5R2b3-6ryl-R7dia-xVk52UVdz17bH9M"

if_raw <- read_sheet(if_id, "If")
if_raw


if_tokens <- if_raw |> 
  unnest_tokens(word, Text)

# NRC
nrc <- get_sentiments("nrc")


sentiment_colors <- c("Neutral" = "#adb5bd",
                      "Trust" = "#2a9d8f", "Anticipation" = "#e9c46a", 
                      "Fear" = "#264653", "Joy" = "#f4a261", 
                      "Anger" = "#e76f51", "Sadness" = "#6d6875", 
                      "Surprise" = "#84a59d", "Disgust" = "#bc6c25")


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


if_nrc <- if_nrc |> 
  clean_names() |> 
  rename(nrc_sentiment = sentiment) |> 
  select(verse, line, text, nrc_sentiment)


plot_if_verses_nrc <- if_nrc |> 
  group_by(verse, nrc_sentiment) |> 
  summarize(n = n(), .groups = "drop") |> 
  group_by(verse) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  select(-n) |>
  mutate(nrc_sentiment = factor(nrc_sentiment,
                                levels = c("Joy", "Trust", "Anticipation", "Surprise",
                                           "Anger", "Disgust", "Fear", "Sadness", "Neutral"))) |> 
  ggplot(aes(x = verse, y = nrc_sentiment, fill = prop)) +
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
    title = "Emotion Profile Across Verses. Anger is dominant.",
    subtitle = "NRC lexicon sentiments",
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

plot_if_verses_nrc


ggsave(
  filename = "plots/01-plot_if_verses_nrc.png",
  plot = plot_if_verses_nrc,
  width = 15,
  height = 10,
  dpi = 300
)


plot_if_lines_nrc <- if_nrc |> 
  mutate(nrc_sentiment = factor(nrc_sentiment,
                                levels = c("Joy", "Trust", "Anticipation", "Surprise",
                                           "Anger", "Disgust", "Fear", "Sadness", "Neutral"))) |> 
  ggplot(aes(x = line, y = nrc_sentiment, fill = nrc_sentiment)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_x_continuous(breaks = seq(1, 32, by = 1), limits = c(0.5, 32.5)) +
  scale_fill_manual(values = sentiment_colors) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "Emotion Profile Across Lines. Anger is dominant.",
    subtitle = "NRC lexicon sentiments",
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

if_gemma4_latest |> filter(is.na(confidence_score))


if_gemma4_latest <- if_gemma4_latest |> 
  mutate(Sentiment = str_to_title(replace_na(sentiment, "neutral"))) |> 
  select(-sentiment) |> 
  relocate(Sentiment, .before = confidence_score)


if_gemma4_latest <- if_gemma4_latest |> 
  clean_names() |> 
  rename(gemma4_sentiment = sentiment,
         gemma4_conf = confidence_score,
         gemma4_reasoning = reasoning)

if_gemma4_latest


plot_if_verses_gemma4_latest <- if_gemma4_latest |>
  group_by(verse, gemma4_sentiment) |> 
  summarize(n = n(), .groups = "drop") |> 
  group_by(verse) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  select(-n) |>
  mutate(gemma4_sentiment = factor(gemma4_sentiment,
                                   levels = c("Joy", "Trust", "Anticipation", "Surprise",
                                              "Anger", "Disgust", "Fear", "Sadness", "Neutral"))) |> 
  ggplot(aes(x = verse, y = gemma4_sentiment, fill = prop)) +
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
    title = "Emotion Profile Across Verses. Anticipation is dominant.",
    subtitle = "Gemma 4 sentiments",
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

plot_if_verses_gemma4_latest


ggsave(
  filename = "plots/03-plot_if_verses_gemma4_latest.png",
  plot = plot_if_verses_gemma4_latest,
  width = 15,
  height = 10,
  dpi = 300
)


plot_if_lines_gemma4_latest <- if_gemma4_latest |> 
  mutate(gemma4_sentiment = factor(gemma4_sentiment,
                                   levels = c("Joy", "Trust", "Anticipation", "Surprise",
                                              "Anger", "Disgust", "Fear", "Sadness", "Neutral"))) |> 
  ggplot(aes(x = line, y = gemma4_sentiment, fill = gemma4_sentiment)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_x_continuous(breaks = seq(1, 32, by = 1), limits = c(0.5, 32.5)) +
  scale_fill_manual(values = sentiment_colors) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "Emotion Profile Across Lines. Anticipation is dominant.",
    subtitle = "Gemma 4 sentiments",
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


# Claude Sonnet 5 

if_sonnet_5 <- if_raw |> 
  get_or_run_claude_batch(Text, Line, output_name = "data/if")

if_sonnet_5 |> filter(is.na(confidence_score))   # or whatever the confidence column is called before the coercion happened

if_sonnet_5 <- if_sonnet_5 |> 
  mutate(Sentiment = str_to_title(replace_na(sentiment, "neutral"))) |> 
  select(-sentiment) |> 
  relocate(Sentiment, .before = confidence_score)

if_sonnet_5 <- if_sonnet_5 |> 
  clean_names() |> 
  rename(sonnet5_sentiment = sentiment,
         sonnet5_conf = confidence_score,
         sonnet5_reasoning = reasoning)

if_sonnet_5


plot_if_verses_sonnet_5 <- if_sonnet_5 |>
  group_by(verse, sonnet5_sentiment) |> 
  summarize(n = n(), .groups = "drop") |> 
  group_by(verse) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  select(-n) |>
  mutate(sonnet5_sentiment = factor(sonnet5_sentiment,
                                    levels = c("Joy", "Trust", "Anticipation", "Surprise",
                                               "Anger", "Disgust", "Fear", "Sadness", "Neutral"))) |> 
  ggplot(aes(x = verse, y = sonnet5_sentiment, fill = prop)) +
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
    title = "Emotion Profile Across Verses. Anticipation is dominant.",
    subtitle = "Claude Sonnet 5 sentiments",
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

plot_if_verses_sonnet_5


ggsave(
  filename = "plots/05-plot_if_verses_sonnet_5.png",
  plot = plot_if_verses_sonnet_5,
  width = 15,
  height = 10,
  dpi = 300
)


plot_if_lines_sonnet_5 <- if_sonnet_5 |> 
  mutate(sonnet5_sentiment = factor(sonnet5_sentiment,
                                    levels = c("Joy", "Trust", "Anticipation", "Surprise",
                                               "Anger", "Disgust", "Fear", "Sadness", "Neutral"))) |> 
  ggplot(aes(x = line, y = sonnet5_sentiment, fill = sonnet5_sentiment)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_x_continuous(breaks = seq(1, 32, by = 1), limits = c(0.5, 32.5)) +
  scale_fill_manual(values = sentiment_colors) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "Emotion Profile Across Lines. Anticipation is dominant.",
    subtitle = "Claude Sonnet 5 sentiments",
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

plot_if_lines_sonnet_5



ggsave(
  filename = "plots/06-plot_if_lines_sonnet_5.png",
  plot = plot_if_lines_sonnet_5,
  width = 15,
  height = 10,
  dpi = 300
)



# Comparable columns names to be merged
if_comparison <- if_nrc |> 
  select(verse, line, text, nrc_sentiment) |> 
  left_join(
    if_gemma4_latest |> select(verse, line, gemma4_sentiment, gemma4_conf, gemma4_reasoning),
    by = c("verse", "line")
  ) |> 
  left_join(
    if_sonnet_5 |> select(verse, line, sonnet5_sentiment, sonnet5_conf, sonnet5_reasoning),
    by = c("verse", "line")
  ) |> 
  arrange(verse, line)

if_comparison


# Compute per-line agreement level

if_agreement <- if_comparison |> 
  rowwise() |> 
  mutate(
    n_distinct_labels = n_distinct(c(nrc_sentiment, gemma4_sentiment, sonnet5_sentiment)),
    agreement_level = case_when(
      n_distinct_labels == 1 ~ "All 3 agree",
      n_distinct_labels == 2 ~ "2 of 3 agree",
      n_distinct_labels == 3 ~ "All 3 disagree"
    )
  ) |> 
  ungroup() |> 
  mutate(agreement_level = factor(agreement_level, 
                                  levels = c("All 3 agree", "2 of 3 agree", "All 3 disagree")))

## ================= Agreement strip plot =================

agreement_colors <- c(
  "All 3 agree"  = "#2a9d8f",
  "2 of 3 agree" = "#e9c46a",
  "All 3 disagree" = "#e76f51"
)

plot_if_agreement_strip <- if_agreement |> 
  ggplot(aes(x = line, y = 1, fill = agreement_level)) +
  geom_tile(color = "white", linewidth = 0.5, height = 0.9) +
  geom_vline(
    data = tibble(x = c(8.5, 16.5, 24.5)),  # verse boundaries — adjust if not 8 lines/verse
    aes(xintercept = x),
    color = "#264653", linewidth = 0.6, linetype = "dashed"
  ) +
  scale_x_continuous(breaks = seq(1, 32, by = 1), limits = c(0.5, 32.5), expand = c(0, 0)) +
  scale_fill_manual(values = agreement_colors, name = NULL) +
  labs(
    title = "Where Do NRC, Gemma 4, and Claude Sonnet 5 Agree?",
    subtitle = "Line-by-line consensus across three sentiment methods: dashed lines mark verse breaks",
    caption = "Data: If, Rudyard Kipling (1910)",
    x = "Line",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(size = 11, hjust = 0.5),
    axis.text.y        = element_blank(),
    axis.text.x        = element_text(size = 10, color = "black"),
    axis.title.x       = element_text(size = 11),
    panel.grid          = element_blank(),
    plot.background    = element_rect(fill = "#cbe8f5", color = NA),
    panel.background   = element_rect(fill = "#cbe8f5", color = NA),
    legend.position    = "bottom",
    legend.text         = element_text(size = 11)
  )

plot_if_agreement_strip


ggsave(
  filename = "plots/07-plot_if_agreement_strip.png",
  plot = plot_if_agreement_strip,
  width = 15,
  height = 4,
  dpi = 300
)


## ggalluvial expects one row per line, with one column per "axis" (method)
## — this is exactly the shape if_comparison is already in, so no reshaping needed.

plot_if_alluvial <- if_comparison |> 
  arrange(line) |> 
  mutate(
    nrc_sentiment     = factor(nrc_sentiment, levels = sentiment_levels),
    gemma4_sentiment  = factor(gemma4_sentiment, levels = sentiment_levels),
    sonnet5_sentiment = factor(sonnet5_sentiment, levels = sentiment_levels)
  ) |> 
  ggplot(aes(axis1 = nrc_sentiment, axis2 = gemma4_sentiment, axis3 = sonnet5_sentiment)) +
  geom_alluvium(aes(fill = nrc_sentiment), width = 0.25, alpha = 0.85, color = "white", linewidth = 0.2,
                discern = FALSE) +
  geom_stratum(width = 0.25, fill = "white", color = "#264653", linewidth = 0.6, discern = FALSE) +
  geom_text_repel(
    stat = "stratum", aes(label = after_stat(stratum)),
    size = 3.5, fontface = "bold", discern = FALSE,
    direction = "y", nudge_x = -0.2, segment.size = 0.3, segment.color = "#264653",
    min.segment.length = 0, box.padding = 0.3, max.overlaps = Inf,
    max.iter = 20000, force = 2
  ) +
  geom_text_repel(
    stat = "alluvium", aes(label = line),
    size = 2.2, color = "black",
    direction = "y", nudge_x = 0.2, segment.size = 0.15, segment.color = "grey50",
    min.segment.length = 0, box.padding = 0.15, max.overlaps = Inf,
    max.iter = 20000, force = 1.5
  ) +
  scale_x_discrete(limits = c("NRC", "Gemma 4", "Claude Sonnet 5"), expand = c(0.2, 0.2)) +
  scale_fill_manual(values = sentiment_colors) +
  coord_cartesian(clip = "off") +   # critical — stops repel-nudged labels from being cut at panel edges
  labs(
    title = "How Sentiment Labels Shift Across Methods",
    subtitle = "Each line's path from NRC to Gemma 4 to Claude Sonnet 5, colored by NRC's original label",
    caption = "Data: If, Rudyard Kipling (1910)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text.x       = element_text(size = 13, face = "bold", color = "black"),
    axis.text.y       = element_blank(),
    panel.grid         = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position   = "none",
    plot.margin        = margin(t = 20, r = 60, b = 20, l = 60)   # extra room for labels pushed outward
  )

plot_if_alluvial


ggsave(
  filename = "plots/08-plot_if_alluvial.png",
  plot = plot_if_alluvial,
  width = 14,
  height = 16,
  dpi = 300
)



# Agreement / Disagreement between Gemma 5 and Sonnet 5


if_agreement_gemma4_sonnet5 <- if_comparison |> 
  select(-nrc_sentiment) |> 
  rowwise() |> 
  mutate(
    n_distinct_labels = n_distinct(c(gemma4_sentiment, sonnet5_sentiment)),
    agreement_level = case_when(
      n_distinct_labels == 1 ~ "Agree",
      n_distinct_labels == 2 ~ "Disagree"
    )
  ) |> 
  ungroup() |> 
  mutate(agreement_level = factor(agreement_level, 
                                  levels = c("Agree", "Disagree")))

if_agreement_gemma4_sonnet5



## ================= Agreement strip plot - Gemma 4, Sonnet 5 =================

agreement_colors_gemma4_sonnet_5 <- c(
  "Agree"  = "#2a9d8f",
  "Disagree" = "#e76f51"
)

unique(if_agreement_gemma4_sonnet5$agreement_level)

plot_if_agreement_strip_gemma4_sonnet5 <- if_agreement_gemma4_sonnet5 |> 
  ggplot(aes(x = line, y = 1, fill = agreement_level)) +
  geom_tile(color = "white", linewidth = 0.5, height = 0.9) +
  geom_vline(
    data = tibble(x = c(8.5, 16.5, 24.5)),  # verse boundaries — adjust if not 8 lines/verse
    aes(xintercept = x),
    color = "#264653", linewidth = 0.6, linetype = "dashed"
  ) +
  scale_x_continuous(breaks = seq(1, 32, by = 1), limits = c(0.5, 32.5), expand = c(0, 0)) +
  scale_fill_manual(values = agreement_colors_gemma4_sonnet_5, name = NULL) +
  labs(
    title = "Where Do Gemma 4, and Claude Sonnet 5 Agree?",
    subtitle = "Line-by-line consensus across two models: dashed lines mark verse breaks",
    caption = "Data: If, Rudyard Kipling (1910)",
    x = "Line",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(size = 11, hjust = 0.5),
    axis.text.y        = element_blank(),
    axis.text.x        = element_text(size = 10, color = "black"),
    axis.title.x       = element_text(size = 11),
    panel.grid          = element_blank(),
    plot.background    = element_rect(fill = "#cbe8f5", color = NA),
    panel.background   = element_rect(fill = "#cbe8f5", color = NA),
    legend.position    = "bottom",
    legend.text         = element_text(size = 11)
  )
  

plot_if_agreement_strip_gemma4_sonnet5


ggsave(
  filename = "plots/09-plot_if_agreement_strip_gemma4_sonnet5.png",
  plot = plot_if_agreement_strip_gemma4_sonnet5,
  width = 15,
  height = 4,
  dpi = 300
)



plot_if_alluvial_gemma4_sonnet5 <- if_comparison |> 
  arrange(line) |> 
  mutate(
    gemma4_sentiment  = factor(gemma4_sentiment, levels = sentiment_levels),
    sonnet5_sentiment = factor(sonnet5_sentiment, levels = sentiment_levels),
    agreement = if_else(gemma4_sentiment == sonnet5_sentiment, "Agree", "Disagree")
  ) |> 
  ggplot(aes(axis1 = gemma4_sentiment, axis2 = sonnet5_sentiment)) +
  geom_alluvium(aes(fill = agreement), width = 0.25, alpha = 0.85, color = "white", linewidth = 0.2,
                discern = FALSE) +
  geom_stratum(width = 0.25, fill = "white", color = "#264653", linewidth = 0.6, discern = FALSE) +
  geom_text_repel(
    stat = "stratum", aes(label = after_stat(stratum)),
    size = 3.5, fontface = "bold", discern = FALSE,
    direction = "y", nudge_x = -0.2, segment.size = 0.3, segment.color = "#264653",
    min.segment.length = 0, box.padding = 0.3, max.overlaps = Inf,
    max.iter = 20000, force = 2
  ) +
  geom_text_repel(
    stat = "alluvium", aes(label = line),
    size = 2.2, color = "black",
    direction = "y", nudge_x = 0.2, segment.size = 0.15, segment.color = "grey50",
    min.segment.length = 0, box.padding = 0.15, max.overlaps = Inf,
    max.iter = 20000, force = 1.5
  ) +
  scale_x_discrete(limits = c("Gemma 4", "Claude Sonnet 5"), expand = c(0.2, 0.2)) +
  scale_fill_manual(values = agreement_colors_gemma4_sonnet_5, name = NULL) +
  coord_cartesian(clip = "off") +
  labs(
    title = "How Sentiment Labels Shift Across Two Models?",
    subtitle = "Each line's path from Gemma 4 to Claude Sonnet 5, colored by agreement",
    caption = "Data: If, Rudyard Kipling (1910)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text.x       = element_text(size = 13, face = "bold", color = "black"),
    axis.text.y       = element_blank(),
    panel.grid         = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position   = "bottom",
    legend.text        = element_text(size = 12),
    plot.margin         = margin(t = 20, r = 60, b = 20, l = 60)
  )

plot_if_alluvial_gemma4_sonnet5


ggsave(
  filename = "plots/10-plot_if_alluvial_gemma4_sonnet5.png",
  plot = plot_if_alluvial_gemma4_sonnet5,
  width = 12,
  height = 9,
  dpi = 300
)



# Tables

# Save results as Excel file to upload to Datawrapper
write_xlsx(if_gemma4_latest, "tables/if_gemma4_latest.xlsx")
write_xlsx(if_sonnet_5, "tables/if_sonnet_5.xlsx")


if_agreement |> 
  filter(agreement_level == "2 of 3 agree") |> 
  mutate(
    pattern = case_when(
      nrc_sentiment == gemma4_sentiment  ~ "NRC = Gemma4 (Sonnet5 differs)",
      nrc_sentiment == sonnet5_sentiment ~ "NRC = Sonnet5 (Gemma4 differs)",
      gemma4_sentiment == sonnet5_sentiment ~ "Gemma4 = Sonnet5 (NRC differs)"
    )
  ) |> 
  count(pattern) |> 
  arrange(desc(n))


if_agreement |> 
  filter(agreement_level == "2 of 3 agree", gemma4_sentiment == sonnet5_sentiment) |> 
  select(line, text, nrc_sentiment, gemma4_sentiment, sonnet5_sentiment) |> 
  count(nrc_sentiment) |> 
  arrange(desc(n))


if_2_llm_agree <- if_agreement |> 
  filter(agreement_level == "2 of 3 agree", gemma4_sentiment == sonnet5_sentiment) |> 
  select(line, text, nrc_sentiment, gemma4_sentiment, sonnet5_sentiment, gemma4_reasoning, sonnet5_reasoning)


if_2_llm_agree |> 
  gt() |> 
  cols_label(
    line = "Line",
    text = "Text",
    nrc_sentiment = "NRC",
    gemma4_sentiment = "Gemma 4",
    gemma4_reasoning = "Gemma 4 reasoning",
    sonnet5_sentiment = "Claude Sonnet 5",
    sonnet5_reasoning = "Claude Sonnet 5 reasoning"
  ) |> 
  tab_header(
    title = "Where Gemma 4 and Claude Sonnet 5 Agree Against NRC",
    subtitle = "Lines where both models converged on a label the NRC lexicon didn't assign"
  ) |> 
  tab_caption("Data: If, Rudyard Kipling (1910)") |> 
  # Header styling
  tab_style(
    style = list(cell_fill(color = "#2a9d8f"), cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) |> 
  # Base body styling
  tab_style(
    style = cell_fill(color = "#cbe8f5"),
    locations = cells_body()
  ) |> 
  cols_align(align = "center", columns = c(line, gemma4_sentiment, sonnet5_sentiment)) |> 
  cols_align(align = "left", columns = text) |> 
  cols_width(
    line ~ px(60),
    text ~ px(400),
    gemma4_sentiment ~ px(150),
    sonnet5_sentiment ~ px(150)
  ) |> 
  gtsave("tables/01-if_2_llm_agree.png")



plutchik_order <- c("Joy", "Trust", "Fear", "Surprise", "Sadness", "Disgust", "Anger", "Anticipation")

wheel_distance <- function(a, b) {
  if (a == "Neutral" | b == "Neutral") return(NA_integer_)
  idx_a <- match(a, plutchik_order) - 1
  idx_b <- match(b, plutchik_order) - 1
  d <- abs(idx_a - idx_b)
  pmin(d, 8 - d)
}


gemma4_sonnet5_disagreements <- if_comparison |> 
  filter(gemma4_sentiment != sonnet5_sentiment) |> 
  count(gemma4_sentiment, sonnet5_sentiment) |> 
  arrange(desc(n)) |> 
  rowwise() |> 
  mutate(
    distance = wheel_distance(gemma4_sentiment, sonnet5_sentiment),
    pair_type = case_when(
      is.na(distance) ~ "Involves Neutral",
      distance == 1    ~ "Adjacent",
      distance == 2    ~ "Related",
      distance == 3    ~ "Distant",
      distance == 4    ~ "Opposite"
    )
  ) |> 
  ungroup() |> 
  select(-distance)


gemma4_sonnet5_disagreements |> 
  gt() |> 
  cols_label(
    gemma4_sentiment = "Gemma 4",
    sonnet5_sentiment = "Claude Sonnet 5",
    n = "Number of Lines",
    pair_type = "Relationship"
  ) |> 
  tab_header(
    title = "Where Gemma 4 and Claude Sonnet 5 Disagree",
    subtitle = "19 of 32 lines: few are truly opposed readings"
  ) |> 
  tab_caption("Data: If, Rudyard Kipling (1910)") |> 
  tab_style(
    style = list(cell_fill(color = "#2a9d8f"), cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = cell_fill(color = "#cbe8f5"),
    locations = cells_body()
  ) |> 
  tab_style(
    style = cell_fill(color = "#e76f51"),
    locations = cells_body(columns = pair_type, rows = pair_type %in% c("Distant", "Opposite"))
  ) |> 
  tab_style(
    style = cell_fill(color = "#adb5bd"),
    locations = cells_body(columns = pair_type, rows = pair_type == "Involves Neutral")
  ) |> 
  cols_align(align = "center", columns = everything()) |> 
  cols_width(
    gemma4_sentiment ~ px(140),
    sonnet5_sentiment ~ px(140),
    n ~ px(70),
    pair_type ~ px(130)
  ) |> 
  gtsave("tables/02-gemma4_sonnet5_disagreements.png")


gemma4_sonnet5_disagreed_lines <- if_comparison |> 
  filter(gemma4_sentiment != sonnet5_sentiment)

gemma4_sonnet5_disagreed_lines |> 
  filter(gemma4_sentiment == "Anger" & sonnet5_sentiment == "Trust") |> 
  dplyr::pull(sonnet5_reasoning)

gemma4_sonnet5_disagreed_lines |> 
  filter(gemma4_sentiment == "Sadness" & sonnet5_sentiment == "Trust") |> 
  dplyr::pull(sonnet5_reasoning)


# Checks
if_comparison |> 
  filter(line == 28)

# If all men count with you, but none too much;

if_nrc

nrc |> 
  filter(word == "if")

nrc |> 
  filter(word == "all")

nrc |> 
  filter(word == "men")

nrc |> 
  filter(word == "count")

nrc |> 
  filter(word == "with")

nrc |> 
  filter(word == "you")


nrc |> 
  filter(word == "but")


nrc |> 
  filter(word == "none")


nrc |> 
  filter(word == "too")

nrc |> 
  filter(word == "much")


if_gemma4_latest |> 
  filter(line == 28) |> 
  dplyr::pull(gemma4_reasoning)

if_sonnet_5 |> 
  filter(line == 28) |> 
  dplyr::pull(sonnet5_reasoning)


if_agreement |> 
  filter(agreement_level == "2 of 3 agree", gemma4_sentiment == sonnet5_sentiment, nrc_sentiment == "Anger") |> 
  select(line, text, nrc_sentiment, gemma4_sentiment, sonnet5_sentiment)

if_agreement |> 
  filter(agreement_level == "2 of 3 agree", gemma4_sentiment == sonnet5_sentiment, nrc_sentiment == "Neutral") |> 
  select(line, text, nrc_sentiment, gemma4_sentiment, sonnet5_sentiment)







# Reasoning and Judging - For future analysis

## NRC judged by Gemma4

if_nrc_judged_by_gemma4_latest <- if_nrc |> 
  get_or_run_judge_local(Text, Sentiment, output_name = "data/if_nrc", model = gemma4_latest)

if_nrc_judged_by_gemma4_latest






