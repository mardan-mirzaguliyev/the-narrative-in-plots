library(googlesheets4)
library(tidyverse)
library(ggrepel)
library(textrecipes)
library(glmnet)
library(gt)
library(here)
library(dotenv)


load_dot_env(file = here(".env"))

lyrics_id <- Sys.getenv("ADIEMUS_ID")


gs4_auth()

lyrics_raw <- read_sheet(lyrics_id, sheet = "Adiemus") 
lyrics_raw

# Claude Sonnet 5
lyrics_original_sonnet_5 <- lyrics_raw |>
  get_or_run_claude_synch(text_col = text,
                          output_name = "data/output/adiemus",
                          model = "claude-sonnet-5",
                          labels = default_sentiment_labels)

unique(lyrics_original_sonnet_5$sentiment)
unique(lyrics_original_sonnet_5$reasoning)


# Gemini 3.6 Flash Results turned into tibble

# Mapping the Gemini audio analysis into a structured timeline

lyrics_mp3_gemini <- tribble(
  ~stage, ~timestamp, ~section, ~emotion, ~valence,
  1,  "00:00", "Intro",                  "Pastoral Solitude",       -0.3,
  2,  "00:15", "A-Theme Part I",         "Voice Awakens",            0.1,
  3,  "00:33", "A-Theme Part II",        "Rhythmic Lock",            0.4,
  4,  "00:52", "B-Theme",                "Solar Burst",              0.9,
  5,  "01:19", "Interlude",              "Receding Tide",           -0.2,
  6,  "01:56", "B-Theme Return",         "Re-energized Affirmation", 0.95,
  7,  "02:24", "A-Theme Restatement",    "Intimate Compression",     0.2,
  8,  "02:41", "Pre-Climax Surge",       "Coiled Tension",           0.6,
  9,  "02:57", "Climactic Apex",         "Monumental Density",       1.0,
  10, "03:12", "Outro Transition",       "Ancestral Chant",          0.5,
  11, "03:40", "Final Dissolve",         "Residual Reverberation",  -0.1
)

lyrics_mp3_gemini


plot_lyrics_mp3_gemini <- lyrics_mp3_gemini |> 
  ggplot(aes(x = stage, y = valence, group = 1)) +
  # The main arc line
  geom_line(color = "gray80", linewidth = 1) +
  # Color points based on tension (positive = high energy, negative = calm)
  geom_point(aes(color = valence > 0), size = 4, show.legend = FALSE) +
  
  # Zero-baseline for visual reference
  annotate("segment", x = 1, xend = 11, y = 0, yend = 0, 
           linetype = "dashed", color = "gray50", linewidth = 0.6) +
  scale_color_manual(values = c("TRUE" = "#f2a900",  
                                "FALSE" = "#4ba3e3")) + 
  # Custom x-axis using the section names instead of numbers
  scale_x_continuous(breaks = 1:11, labels = lyrics_mp3_gemini$section) +
  # Add the emotion labels above the points
  geom_label(aes(label = emotion),
             vjust = -0.5,
             fill = "#1e1e1e",
             color = "white",
             linewidth = 0.2,
             size = 3.5,
             fontface = "bold") +
  # Expand limits slightly so labels don't cut off
  coord_cartesian(ylim = c(-0.5, 1.3)) +
  labs(
    title = "Flow of acoustic tension, choral density, and rhythm",
    subtitle = "Gemini 3.6 Flash jugments after 'listening' to the song mp3",
    caption = "Data: Adiemus, Karl Jenkins (1995)",
    x = NULL,
    y = "Acoustic Tension & Euphoria"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 10, hjust = 0.5),
    axis.text         = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    panel.grid         = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position   = "none",
    plot.margin        = margin(t = 20, r = 40, b = 20, l = 40)
  )

plot_lyrics_mp3_gemini


ggsave(
  filename = "plots/01-plot_lyrics_mp3_gemini.png",
  plot = plot_lyrics_mp3_gemini,
  width = 15,
  height = 10,
  dpi = 300
)


# Read the translated lyrics into R
lyrics_translated_unlabeled <- read_csv("data/output/adiemus_translated_full.csv")
lyrics_translated_unlabeled

lyrics_translated_labeled <- lyrics_translated_unlabeled |> 
  get_or_run_claude_synch(text_col = translated_text,
    output_name = "data/output/adiemus-translation",
    model = "claude-sonnet-5",
    labels = default_sentiment_labels)

lyrics_translated_labeled

# Final data objects
glimpse(lyrics_original_sonnet_5)
glimpse(lyrics_mp3_gemini)
glimpse(lyrics_translated_labeled)


# Build a comparison table
adiemus_comparison <- lyrics_original_sonnet_5 |> 
  select(line, text, original_sentiment = sentiment, original_confidence = confidence_score) |> 
  left_join(
    lyrics_translated_labeled |> 
      select(line, translated_text, translated_sentiment = sentiment, translated_confidence = confidence_score, reasoning),
    by = "line"
  )

adiemus_comparison

adiemus_comparison |> count(translated_sentiment) |> arrange(desc(n))
sum(adiemus_comparison |> count(translated_sentiment) |> pull(n))   # should equal 50

# The same line but different translation - 0 in this case
inconsistent_translations <- adiemus_comparison |> 
  group_by(text) |> 
  filter(n_distinct(translated_text) > 1) |> 
  ungroup() |> 
  arrange(text, line)

inconsistent_translations |> 
  select(line, text, translated_text, translated_sentiment)

# The same line, the same translation but different sentiment label
inconsistent_sentiments <- adiemus_comparison |> 
  group_by(translated_text) |> 
  filter(n_distinct(translated_sentiment) > 1) |> 
  ungroup() |> 
  arrange(translated_text, line)

inconsistent_sentiments|> 
  select(line, text, translated_text, translated_sentiment, translated_confidence, reasoning)


# How many distinct repeated phrases have sentiment instability?
inconsistent_sentiments |> distinct(translated_text) |> nrow()

# How many total lines are affected?
nrow(inconsistent_sentiments)


# Visualize two Sonnet 5 results
distribution_data <- adiemus_comparison |> 
  count(translated_sentiment, name = "n") |> 
  mutate(
    pct = n / sum(n),
    sentiment = factor(translated_sentiment, levels = default_sentiment_labels)
  ) |> 
  filter(n > 0) |>   # drop categories with zero lines — nothing to show for them
  arrange(desc(n))


plot_adiemus_distribution <- distribution_data |> 
  ggplot(aes(x = fct_reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(n, " (", scales::percent(pct, accuracy = 1), ")")), 
             hjust = -0.15, size = 4, fontface = "bold", color = "#264653") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = default_sentiment_colors, guide = "none") +
  scale_y_continuous(limits = c(0, max(distribution_data$n) * 1.25), expand = c(0, 0)) +
  labs(
    title = "After 'translation', the Song Leans Hard Toward Anticipation",
    subtitle = "Sonnet 5's sentiment judgement on all 50 lines",
    caption = "Data: Adiemus (AI generated 'translation'), Karl Jenkins (1995)",
    x = NULL, 
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(face = "bold", size = 13),
    axis.text.x = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.margin = margin(t = 10, r = 60, b = 10, l = 10),
    plot.background = element_rect(fill = "#cbe8f5", color = NA),
    panel.background = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_adiemus_distribution


ggsave(
  filename = "plots/02-plot_adiemus_distribution.png",
  plot = plot_adiemus_distribution,
  width = 15,
  height = 10,
  dpi = 300
)


disputed_phrases <- adiemus_comparison |> 
  filter(translated_text == "Sun and fire" | str_detect(translated_text, "Oh the sky burns")) |> 
  select(line, text, translated_text, translated_sentiment, translated_confidence, reasoning) |> 
  arrange(translated_text, line)

adiemus_comparison |> 
  filter(str_detect(translated_text, "burn")) |> 
  distinct(translated_text)


disputed_phrases |> 
  gt(groupname_col = "translated_text") |> 
  cols_label(
    line = "Line",
    text = "Original",
    translated_sentiment = "Sentiment",
    translated_confidence = "Confidence",
    reasoning = "Reasoning"
  ) |> 
  fmt_number(columns = translated_confidence, decimals = 2) |> 
  tab_header(
    title = "The Same Phrase, Scored Differently Each Time",
    subtitle = "Two repeated lines where identical translations produced disagreeing sentiment labels"
  ) |> 
  tab_caption("Data: Adiemus, Karl Jenkins (1995)") |> 
  tab_style(
    style = list(cell_fill(color = "#2a9d8f"), cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = list(cell_fill(color = "#264653"), cell_text(color = "white", weight = "bold")),
    locations = cells_row_groups()
  ) |> 
  tab_style(
    style = cell_fill(color = "#cbe8f5"),
    locations = cells_body()
  ) |> 
  tab_style(
    style = list(cell_fill(color = "#f4a261"), cell_text(weight = "bold")),
    locations = cells_body(columns = translated_sentiment)
  ) |> 
  cols_align(align = "left", columns = c(text, reasoning)) |> 
  cols_align(align = "center", columns = c(line, translated_sentiment, translated_confidence)) |> 
  cols_width(
    line ~ px(50),
    text ~ px(160),
    translated_sentiment ~ px(100),
    translated_confidence ~ px(110),
    reasoning ~ px(340)
  ) |> 
  gtsave(filename = "tables/01-different-sentiments.png")


