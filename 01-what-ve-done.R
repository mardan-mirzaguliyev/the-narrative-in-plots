# Load necessary packages

library(dplyr)          # Data manipulation 
library(tidyr)          # Reshaping: pivot_wider function
library(ggplot2)        # Data visualization
library(textdata)       # Downloads and caches AFINN / Bing / NRC lexicons locally
library(stringr)        # String ops: str_replace_all for contraction expansion
library(tibble)         # tribble() for readable row-by-row data entry; clean printing
library(scales)         # Axis formatters: label_percent(), label_number()
library(googlesheets4)  # Reads lyrics dataframe directly from Google Sheets
library(tidytext)       # Tokenisation pipeline: unnest_tokens + lexicon joins
library(syuzhet)        # Line-level scoring without tokenisation; better for lyrics


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
valence_summary <- lyrics_scored |> 
  summarize(
    n_words = n(),
    raw_sum = sum(value),
    max_possible = n() * 5,
    valence = raw_sum / max_possible
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


# ── 6. EMOTIONAL ARC (valence per line)
arc <- lyrics_tokens |> 
  inner_join(afinn, by = "word") |> 
  group_by(line, section) |> 
  summarize(line_valence = sum(value) / (n() * 5),
            .groups = "drop")

cat("\n-- Emotional Arc (line-by-line valence) -----\n")
print(arc)

# ── 7. LYRIC DENSITY ─────────────────────────────────────────
# Density = meaningful words per line (after stopword removal)
density <- lyrics_raw |> 
  mutate(total_words = str_count(text, "\\S+")) |> 
  left_join(
    lyrics_tokens |> count(line, name = "content_words"),
    by = "line"
  ) |> 
  mutate(
    content_words = replace_na(content_words, 0),
    density_ratio = content_words / total_words
  )

overall_density <- mean(density$density_ratio, na.rm = TRUE)
cat(sprintf("\n-- Lyric Density -----\n"))
cat(sprintf("Average content-word ratio: %.2f\n", overall_density))
cat(sprintf("Classification: %s\n", 
            ifelse(overall_density < 0.40, "Sparse", ifelse(overall_density < 0.60, "Moderate", "Dense" ))
))

# ── 8. SENTIMENT SIGNAL TABLE ────────────────────────────────
# Key phrases with their AFINN word scores

signal_words <- lyrics_scored |> 
  arrange(value) |> 
  select(section, word, afinn_score = value) |>
  mutate(polarity = ifelse(afinn_score < 0, "negative", "neutral"))

cat("\n-- Sentiment Signal Words -----\n")
print(signal_words)


# ── 9. PLOTS
# 9a. Overall emotion breakdown bar chart
p1 <- ggplot(emotion_counts, aes(x = reorder(sentiment, pct),
                                 y = pct, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Emotion Breakdown - \"What I've Done\"",
    subtitle = "NRC lexicon, stopwords removed",
    x = NULL,
    y = "Sjare of emotional words (%)"
  ) +
  theme_minimal(base_size =  13) +
  theme(plot.title = element_text(face = "bold"))

print(p1)
  

# 9b. Emotional arc line chart
p2 <- ggplot(arc, aes(x = line, y = line_valence,
                      color = section, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    verse1 = "#534AB7", chorus = "#1D9E75",
    verse2 = "#378ADD", bridge = "#BA7517"
  )) +
  scale_y_continuous(limits = c(-1, 1),
                     labels = label_number(accuracy = 0.1)) +
  labs(
    title = "Emotional Arc - Line-by-line Valence",
    subtitle = "AFINN scores normalized to -1 / +1",
    x = "Line Number",
    y = "Valence",
    color = "Section") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
    
print(p2)


# 9c. Section-level valence bar chart
p3 <- ggplot(valence_section,
             aes(x = section, y = valence, fill = valence > 0)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_fill_manual(values = c("TRUE" = "#1D9E75", "FALSE" = "#534AB7")) +
  scale_y_continuous(limits = c(-1, 1),
                     labels = label_number(accuracy = 0.1)) +
  labs(
    title = "Emotional Valence by Section",
    subtitle = "AFINN scores normalised to –1 / +1",
    x = "Song section",
    y = "Valence"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p3)
  


# ── 10. SUMMARY TABLE ────────────────────────────────────────
cat("\n══ Full Summary ═══════════════════════════════════════\n")
cat(sprintf("Overall valence        : %+.3f\n", valence_summary$valence))
cat(sprintf("Dominant emotion       : %s\n",   emotion_counts$sentiment[1]))
cat(sprintf("Lyric density ratio    : %.2f (%s)\n",
            overall_density,
            ifelse(overall_density < 0.40, "Sparse",
                   ifelse(overall_density < 0.60, "Moderate", "Dense"))))
cat(sprintf("Total scored words     : %d\n",   valence_summary$n_words))
cat(sprintf("Redemptive arc         : %s\n",
            ifelse(tail(arc$line_valence, 1) > valence_summary$valence,
                   "Upward (redemptive)", "Flat / downward")))
cat("═══════════════════════════════════════════════════════\n")



valence_sumamry



  
  
  
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












lyrics <- lyrics |>
  mutate(syuzhet_score = get_sentiment(text, method = "syuzhet"))

lyrics |>
  ggplot(aes(x = line, y = syuzhet_score)) +
  geom_col(aes(fill = syuzhet_score), show.legend = FALSE) +
  scale_fill_gradient2(
    low      = "#bd1515",
    mid      = "grey90",
    high     = "#2a9d8f",
    midpoint = 0
  ) +
  geom_text(
    aes(label = round(syuzhet_score, 2)),
    position = position_stack(vjust = 0.5),
    colour   = "white",
    fontface = "bold",
    size     = 4
  ) +
  scale_x_continuous(breaks = seq(1, max(lyrics$line), by = 1)) +
  labs(
    title    = "'What I've Done'",
    subtitle = "Line-by-line emotional trajectory (syuzhet)",
    x        = "Song Timeline (Line Number)",
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




