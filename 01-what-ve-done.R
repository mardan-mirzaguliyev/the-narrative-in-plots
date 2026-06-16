# Load necessary packages

library(dplyr)
library(tidyr)
library(ggplot2)
library(textdata)
library(stringr)
library(tibble)
library(scales)
library(googlesheets4)
library(tidytext)


# Authenticate Google Sheets file that contains lyrics
gs4_deauth()

# Read Google Sheets file with lyrics
lyrics_id <- "13dM_5vWQGdAeRI-thE6nbzlZIbvbZ8dw9eCu3MsP9Kk"
lyrics_raw <- read_sheet(lyrics_id)

# 1. Tokenize your sanitized dataset
lyrics_tokens <- lyrics_raw |> 
  unnest_tokens(word, text) |> 
  # Remove function words
  anti_join(stop_words, by = "word")


# Download AFINN
# AFINN scores words –5 (very negative) to +5 (very positive)
# Valence = sum(scores) / (n_scored_words * 5), giving –1 to +1
afinn <- get_sentiments("afinn")

lyrics_scored <- lyrics_tokens |>
  inner_join(afinn, by = "word")

## Emotional Valence
valence_sumamry <- lyrics_scored |> 
  summarize(
    n_words = n(),
    raw_sum = sum(value),
    max_possible = n() * 5,
    emotional_valence = raw_sum / max_possible
  )

cat("\n-- Overall Emotional Valence -----\n")
print(valence_sumamry)

# Valence per section
valence_section <- lyrics_scored |> 
  group_by(section) |> 
  summarize(
    n_words = n(),
    raw_sum = sum(value),
    valence = raw_sum / (n() * 5)
  ) |> 
  mutate(section = factor(section,
    levels = c("verse1", "chorus",
                "verse2", "bridge")))

cat("\n-- Valence by Section -----\n")
print(valence_section)

# ── 5. EMOTION BREAKDOWN via NRC ─────────────────────────────
# NRC tags words with 8 emotions + positive/negative binary

nrc <- get_sentiments("nrc")

emotion_counts <- lyrics_tokens |> 
  inner_join(nrc, by = "word", 
            relationship = "many-to-many") |> 
  filter(!sentiment %in% c("positive", "negative")) |> 
  count(sentiment, sort = TRUE) |> 
  mutate(pct = n / sum(n) * 100)

cat("\n-- Emotion Breakdown (NRC) -----\n")
print(emotion_counts)






## 1.1 Count total words 

lyrics %>%
  unnest_tokens(output = word, input = text) |> 
  nrow()


lyrics_with_sentiment_scores <- lyrics |>
  unnest_tokens(output = word, input = text) |>
  inner_join(get_sentiments("bing"), by = "word") |> 
  count(line, sentiment) |> 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |> 
  (\(df) {
    if (!"positive" %in% names(df)) df$positive <- 0
    if (!"negative" %in% names(df)) df$negative <- 0
    mutate(df, sentiment_score = positive - negative)
  })() 


lyrics_with_sentiment_scores |> 
  ggplot(aes(x = line, y = value)) +
  geom_col(aes(fill = value), show.legend = FALSE) +
  scale_fill_gradient2(
    low      = "#bd1515",
    mid      = "grey90",
    high     = "#2a9d8f",
    midpoint = 0
  ) +
  geom_text(aes(label = value), position = position_stack(vjust = 0.5)) +
  scale_x_continuous(breaks = seq(1, max(lyrics_with_sentiment_scores$line), by = 1)) +
  labs(
    title = "Sentiment Score per Line — \"What I've Done\"",
    x     = "Line",
    y     = "Sentiment score (positive − negative)"
  ) +
  labs(
    title = "'What I've Done' ",
    subtitle = "Line-by-line emotional trajectory showing narrative shifts",
    x = "Song Timeline (Line Number)",
    y = "Net Sentiment Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.title.x = element_text(color = "black", size = 11),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray30", linewidth = 0.1),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#cbe8f5", color = NA),
    panel.background = element_rect(fill = "#cbe8f5", color = NA)
  )

















