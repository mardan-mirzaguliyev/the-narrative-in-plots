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


# Read Google Sheets file into R 
## Authenticate Google account that contains lyrics file
gs4_deauth()

## Read Google Sheets file with lyrics
## For recreation copy and paste the lyrics into your own Google Sheets file. 
## with these columns: section line text
lyrics_id <- "13dM_5vWQGdAeRI-thE6nbzlZIbvbZ8dw9eCu3MsP9Kk"
lyrics_raw <- read_sheet(lyrics_id)


# Tokenize the lyrics
lyrics_tokens <- lyrics_raw |> 
  unnest_tokens(word, text)

## Total word count - before comparing against AFINN
word_count <- lyrics_tokens |> nrow()
word_count

## Download AFINN lexicon
# AFINN scores words –5 (very negative) to +5 (very positive)
# Valence = sum(scores) / (n_scored_words * 5), giving –1 to +1
afinn <- get_sentiments("afinn")
afinn

## Compare words against afinn 
lyrics_scored <- lyrics_tokens |>
  left_join(afinn, by = "word")


# tidytext calculations and plots 

## Emotional Valence - tidytext
valence_summary <- lyrics_scored |> 
  mutate(value = replace_na(value, 0)) |> 
  summarize(
    n_words = n(),
    raw_sum = sum(value),
    max_possible = n() * 5,
    valence = raw_sum / max_possible
  )


## Valence per line - Emotional Arc (valence per line)
arc <- lyrics_scored |>
  group_by(line, section) |> 
  summarize(
    n_scored = sum(!is.na(value)),
    raw_sum = sum(value, na.rm = TRUE),
    # n_scored * 5 the theoretical maximum possible score for that many words)
    # percentage of the maximum possible intensity — always between −1 and +1, 
    # regardless of how many words matched or how long the line is.
    line_valence = if_else(n_scored > 0, raw_sum / (n_scored * 5), 0), # avoid divide-by-zero
            .groups = "drop") |> 
  arrange(line)
arc

## Emotional Arc (valence per line) Plot
section_labels <- arc |>
  mutate(
    block = cumsum(section != lag(section, default = first(section)))
  ) |>
  group_by(section, block) |>
  summarize(
    line_no      = mean(line),                  # true midpoint within this block
    line_valence = max(line_valence) + 0.15,
    .groups      = "drop"
  )

y_min <- min(arc$line_valence, na.rm = TRUE) - 0.3
y_max <- max(section_labels$line_valence, na.rm = TRUE) + 0.3

p1 <- ggplot(arc, aes(x = line, y = line_valence,
                      color = section, linetype = section, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_point(size = 3, show.legend = FALSE) +
  scale_color_manual(values = c(
    "verse1" = "#0072B2",
    "chorus" = "#D55E00",
    "verse2" = "#009E73",
    "bridge" = "#CC79A7"
   )) +
  geom_label(
    aes(label = round(line_valence, 2)),
    vjust    = -0.6,      # pushes label above the point
    colour   = "black",
    fontface = "bold",
    size     = 5,
    fill     = "white",   # label background
    linewidth = 0.2,      # border thickness around label box
    show.legend = FALSE
  ) +
  geom_label(
    data = section_labels,
    aes(x = line_no, y = line_valence, label = section, color = section),
    fontface = "bold",
    size = 5,
    fill = "white",
    show.legend = FALSE
  ) +
  scale_y_continuous(limits = c(y_min, y_max),
                     labels = label_number(accuracy = 0.1)) +
  scale_x_continuous(breaks = seq(1, max(lyrics_scored$line), by = 1)) +
  labs(
    title = "Emotional Arc - Line-by-line Valence",
    subtitle = "AFINN scores normalized to -1 / +1",
    x = "Line Number",
    y = "Valence"
    ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 12, hjust = 0.5),
    axis.text         = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray30", linewidth = 0.1),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position = "none"
  )

print(p1)

ggsave(
  filename = "01-emotional-arc.png",
  plot = p1,
  width = 15,
  height = 10,
  dpi = 300
)



# Overall valence
valence_overall <- lyrics_scored |> 
  filter(!is.na(value)) |>          # keep only AFINN-matched words
  summarize(
    n_words = n(),
    raw_sum = sum(value),
    max_possible = n() * 5,
    valence = raw_sum / max_possible
  )

print(valence_overall)

# Valence per section

## Method 1: Weights by word count: lines with more scored words 
             ## have more influence
             # Best for: "What is the overall sentiment density of this section?"

valence_section <- lyrics_scored |>
  filter(!is.na(value)) |>          # keep only AFINN-matched words
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


## Method 2 (SELECTED): Arc-derived valence scores based on valence per lines
             ## Average of arc - line weighted
valence_section_from_arc <- arc |> 
  group_by(section) |> 
  summarize(valence = mean(line_valence)) |>
  mutate(section = factor(section,
                          levels = c("verse1", "chorus",
                                     "verse2", "bridge")))


section_labels_for_p2 <- valence_section_from_arc |>
  group_by(section) |>
  summarize(               
    valence = max(valence) + 0.15, # small offset above the point
    .groups      = "drop"
  )


p2 <- ggplot(valence_section_from_arc, aes(x = section, y = valence,
                        color = section, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_point(size = 3, show.legend = FALSE) +
  scale_color_manual(values = c(
    "verse1" = "#0072B2",
    "chorus" = "#D55E00",
    "verse2" = "#009E73",
    "bridge" = "#CC79A7"
  )) +
  geom_label(
    data = section_labels_for_p2,
    aes(label = round(valence, 2)),
    vjust    = -0.7,      # pushes label above the point
    colour   = "black",
    fontface = "bold",
    size     = 4,
    fill     = "white",   # label background
    linewidth = 0.2,      # border thickness around label box
    show.legend = FALSE
  ) +
  geom_label(
    data = section_labels_for_p2,
    aes(x = section, y = valence, label = section, color = section),
    fontface = "bold",
    size = 5,
    fill = "white",
    show.legend = FALSE
  ) +
  labs(
    title = "Valence Per Section",
    subtitle = "AFINN scores normalized to -1 / +1",
    x = "Section",
    y = "Valence",
    color = "Section") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(size = 14, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray30", linewidth = 0.1),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

print(p2)

ggsave(
  filename = "02-valence-per-section.png",
  plot = p2,
  width = 15,
  height = 10,
  dpi = 300
)



# Emotion Breakdown via NRC
## NRC tags words with 8 emotions + positive/negative binary

nrc <- get_sentiments("nrc")

emotion_counts <- lyrics_tokens |> 
  inner_join(nrc, by = "word", 
            relationship = "many-to-many") |> 
  filter(!sentiment %in% c("positive", "negative")) |> 
  count(sentiment, sort = TRUE) |> 
  mutate(pct = n / sum(n) * 100)


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
    linewidth = 0.2      # border thickness around label box
  ) +
  labs(
    title = "Emotion Breakdown",
    subtitle = "NRC lexicon, stopwords removed",
    x = NULL,
    y = "Share of emotions (%)"
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

ggsave(
  filename = "03-emotional-breakdown.png",
  plot = p3,
  width = 15,
  height = 10,
  dpi = 300
)


# Lyric Density
## Density = meaningful words per line (after stopword removal)
lyrics_tokens_content <- lyrics_raw |> 
  unnest_tokens(word, text) |> 
  anti_join(stop_words, by = "word") # removes the, a, of, in, etc.

density <- lyrics_raw |> 
  mutate(total_words = str_count(text)) |> 
  left_join(
    lyrics_tokens_content |> 
      count(line, name = "content_words"),
    by = "line"
  ) |> 
  mutate(content_words = replace_na(content_words, 0),
         density_ratio = round(content_words / total_words, 2))

overall_density <- mean(density$density_ratio)


# Sentiment Signal Table
# Key phrases with their AFINN word scores
signal_words <- lyrics_scored |> 
  filter(!is.na(value)) |> 
  arrange(value) |> 
  select(section, word, afinn_score = value) |> 
  mutate(
    polarity = case_when(
      afinn_score > 0 ~ "positive",
      afinn_score < 0 ~ "negative",
      TRUE            ~ "neutral" # exactly 0 — rare in AFINN but possible
    )
  )



# Syuzhet Calculations and Plots
## Get syuzhet lexicon

lyrics_scored_syuzhet <- lyrics_raw |>
  mutate(syuzhet_score = get_sentiment(text, method = "syuzhet"))
lyrics_scored_syuzhet

# Normalise to –1 / +1
# syuzhet scores are unbounded, so we divide by the observed maximum
# to get a scale anchored between –1 and +1
max_abs <- max(abs(lyrics_scored_syuzhet$syuzhet_score))
max_abs

lyrics_scored_syuzhet <- lyrics_scored_syuzhet |>
  mutate(normalised_score = syuzhet_score / max_abs)
lyrics_scored_syuzhet

# Overall valence = mean of all normalised line scores
overall_valence_syuzhet <- mean(lyrics_scored_syuzhet$normalised_score)

cat(sprintf("Overall emotional valence: %+.3f\n", overall_valence_syuzhet))



p4 <- lyrics_scored_syuzhet |>
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
    linewidth = 0.2      # border thickness around label box
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

print(p4)



ggsave(
  filename = "04-syuzhet-calculations.png",
  plot = p4,
  width = 15,
  height = 10,
  dpi = 300
)





