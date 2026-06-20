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


# Authenticate Google account that containes lyrics file
gs4_deauth()

# Read Google Sheets file with lyrics
# For recreation copy and paste the lyrics into your own Google Sheets file. 
# with these columns: section line text
lyrics_id <- "13dM_5vWQGdAeRI-thE6nbzlZIbvbZ8dw9eCu3MsP9Kk"
lyrics_raw <- read_sheet(lyrics_id)

# 1. Tokenize your sanitized dataset

# Total word count with stop words
lyrics_raw |> 
  unnest_tokens(word, text) |> 
  nrow()

lyrics_tokens <- lyrics_raw |> 
  unnest_tokens(word, text)


# Download AFINN
# AFINN scores words –5 (very negative) to +5 (very positive)
# Valence = sum(scores) / (n_scored_words * 5), giving –1 to +1
afinn <- get_sentiments("afinn")


lyrics_scored <- lyrics_tokens |>
  inner_join(afinn, by = "word")

## Emotional Valence - Tidytext
valence_summary <- lyrics_scored |> 
  summarize(
    n_words = n(),
    raw_sum = sum(value),
    max_possible = n() * 5,
    valence = raw_sum / max_possible
  )

cat("\n-- Overall Emotional Valence -----\n")
print(valence_summary)


## Emotional Valence - Syuzhet
lyrics_scored <- lyrics_raw |>
  mutate(syuzhet_score = get_sentiment(text, method = "syuzhet"))

max_abs <- max(abs(lyrics_scored$syuzhet_score))

overall_valence_syuzhet <- mean(lyrics_scored$syuzhet_score / max_abs)

cat(sprintf("AFINN valence  : %+.3f\n", 0.025))
cat(sprintf("syuzhet valence: %+.3f\n", overall_valence_syuzhet))


## Valence per line
# EMOTIONAL ARC (valence per line)
arc <- lyrics_scored |>
  group_by(line, section) |> 
  summarize(line_valence = sum(value) / (n() * 5),
            .groups = "drop")

cat("\n-- Emotional Arc (line-by-line valence) -----\n")
print(arc)


min_line_valence <- min(arc$line_valence)
max_line_valence <- max(arc$line_valence)

p1 <- ggplot(arc, aes(x = line, y = line_valence,
                      color = section, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    verse1 = "#534AB7", chorus = "#1D9E75",
    verse2 = "#378ADD", bridge = "#BA7517"
  )) +
  geom_label(
    aes(label = round(line_valence, 2)),
    vjust    = -0.6,      # pushes label above the point
    colour   = "black",
    fontface = "bold",
    size     = 3.5,
    fill     = "white",   # label background
    label.size = 0.2      # border thickness around label box
  ) +
  scale_y_continuous(limits = c(min_line_valence, max_line_valence),
                     labels = label_number(accuracy = 0.1)) +
  scale_x_continuous(breaks = seq(1, max(lyrics_scored$line), by = 1)) +
  labs(
    title = "Emotional Arc - Line-by-line Valence",
    subtitle = "AFINN scores normalized to -1 / +1",
    x = "Line Number",
    y = "Valence",
    color = "Section") +
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

print(p1)


# Valence per section
## weights by word count: lines with more scored words have more influence
# Best for: "What is the overall sentiment density of this section?"

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


# Average of arc - line weighted
valence_section_from_arc <- arc |> 
  group_by(section) |> 
  summarize(valence = mean(line_valence))


valence_section_from_arc


p2 <- ggplot(valence_section_from_arc, aes(x = section, y = valence,
                        color = section, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    verse1 = "#534AB7", chorus = "#1D9E75",
    verse2 = "#378ADD", bridge = "#BA7517"
  )) +
  geom_label(
    aes(label = round(valence, 2)),
    vjust    = -0.6,      # pushes label above the point
    colour   = "black",
    fontface = "bold",
    size     = 3.5,
    fill     = "white",   # label background
    label.size = 0.2      # border thickness around label box
  ) +
  labs(
    title = "Emotional Arc - Line-by-line Valence",
    subtitle = "AFINN scores normalized to -1 / +1",
    x = "Line Number",
    y = "Valence",
    color = "Section") +
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

print(p2)



# ── . EMOTION BREAKDOWN via NRC ─────────────────────────────
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


p3 <- ggplot(emotion_counts, aes(x = reorder(sentiment, pct),
                                 y = pct, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  geom_label(
    aes(label = paste0(round(pct, 2), "%")),
    vjust    = -0.6,      # pushes label above the point
    colour   = "black",
    fontface = "bold",
    size     = 3.5,
    fill     = "white",   # label background
    label.size = 0.2      # border thickness around label box
  ) +
  labs(
    title = "Emotion Breakdown - \"What I've Done\"",
    subtitle = "NRC lexicon, stopwords removed",
    x = NULL,
    y = "Share of emotional words (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )


print(p3)




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



## Syuzhet Calculations and Plot

## Get syuzhet lexicon

lyrics_scored_syuzhet <- lyrics_raw |>
  mutate(syuzhet_score = get_sentiment(text, method = "syuzhet"))

lyrics_scored_syuzhet

# Normalise to –1 / +1
# syuzhet scores are unbounded, so we divide by the observed maximum
# to get a scale anchored between –1 and +1
max_abs <- max(abs(lyrics_scored_syuzhet$syuzhet_score))

lyrics_scored_syuzhet <- lyrics_scored_syuzhet |>
  mutate(normalised_score = syuzhet_score / max_abs)
lyrics_scored_syuzhet

# Overall valence = mean of all normalised line scores
overall_valence <- mean(lyrics_scored_syuzhet$normalised_score)

cat(sprintf("Overall emotional valence: %+.3f\n", overall_valence))



lyrics_scored_syuzhet |>
  ggplot(aes(x = line, y = syuzhet_score)) +
  geom_col(aes(fill = syuzhet_score), show.legend = FALSE) +
  scale_fill_gradient2(
    low      = "#bd1515",
    mid      = "grey90",
    high     = "#2a9d8f",
    midpoint = 0
  ) +
  geom_label(
    aes(label = round(syuzhet_score, 2)),
    vjust    = -0.6,      # pushes label above the point
    colour   = "black",
    fontface = "bold",
    size     = 3.5,
    fill     = "white",   # label background
    label.size = 0.2      # border thickness around label box
  ) +
  scale_x_continuous(breaks = seq(1, max(lyrics_scored_syuzhet$line), by = 1)) +
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



