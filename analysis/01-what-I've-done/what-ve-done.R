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
library(here)
library(dotenv)


load_dot_env(file = here(".env"))


# Read Google Sheets file into R 
## Authenticate Google account that contains lyrics file
gs4_deauth()

## Read Google Sheets file with lyrics
## For recreation copy and paste the lyrics into your own Google Sheets file. 
## with these columns: section line text
lyrics_id <- Sys.getenv("WHAT_I_VE_DONE_LYRICS_ID")
lyrics_raw <- read_sheet(lyrics_id)


# Tokenize the lyrics
lyrics_tokens <- lyrics_raw |> 
  mutate(line_id = row_number()) |> # Create a unique ID for each line
  unnest_tokens(output = word, input = text, to_lower = TRUE, drop = FALSE)


# Calculate density per line using the preserved text and line_id
line_density <- lyrics_tokens |> 
  group_by(line_id, text) |> 
  summarise(
    total_words = n(),
    content_words = sum(!word %in% stop_words$word),
    .groups = "drop"
  ) |> 
  mutate(
    density_ratio = round(content_words / total_words, 2)
  )

# Overall average density
overall_density <- mean(line_density$density_ratio, na.rm = TRUE)
overall_density 

## Total word count 
word_count <- lyrics_tokens |> nrow()
word_count

## Download AFINN lexicon
# AFINN scores words –5 (very negative) to +5 (very positive)
afinn <- get_sentiments("afinn")
afinn

# Score words with afinn
lyrics_scored <- lyrics_tokens |>
  left_join(afinn, by = "word")


# Overall valence
valence_overall_afinn <- lyrics_scored |> 
  filter(!is.na(value)) |>          
  summarize(
    n_words = n(),
    raw_sum = sum(value),
    max_possible = n() * 5,
    valence = raw_sum / max_possible
  )

valence_overall_afinn

## Line level Emotional Arc (valence per line)
arc_afinn <- lyrics_scored |>
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
arc_afinn


## Line level plot
section_labels_line <- arc |>
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
y_max <- max(section_labels_line$line_valence, na.rm = TRUE) + 0.3

plot_line_level_afinn <- ggplot(arc, aes(x = line, 
                      y = line_valence,
                      color = section,
                      group = 1)) +
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
    data = section_labels_line,
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
    title = "Emotional Arc: Line-by-line Valence",
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

plot_line_level_afinn

ggsave(
  filename = "plots/01-plot_line_level_afinn.png",
  plot = plot_line_level_afinn,
  width = 15,
  height = 10,
  dpi = 300
)


# Section level Emotional Arc (valence per section)

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
valence_section


## Method 2 (SELECTED): Arc-derived valence scores based on valence per lines
             ## Average of arc - line weighted
valence_section_from_arc <- arc |> 
  group_by(section) |> 
  summarize(valence = mean(line_valence)) |>
  mutate(section = factor(section,
                          levels = c("verse1", "chorus",
                                     "verse2", "bridge")))


section_labels <- valence_section_from_arc |>
  group_by(section) |>
  summarize(               
    valence = max(valence) + 0.15, # small offset above the point
    .groups      = "drop"
  )

plot_section_level_afinn <- ggplot(valence_section_from_arc, aes(x = section, y = valence,
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
    data = section_labels,
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
    data = section_labels,
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

plot_section_level_afinn

ggsave(
  filename = "plots/02-plot_section_level_afinn.png",
  plot = plot_section_level_afinn,
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


plot_emotion_breakdown_nrc <- ggplot(emotion_counts, aes(x = reorder(sentiment, pct),
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

plot_emotion_breakdown_nrc


ggsave(
  filename = "plots/03-plot_emotion_breakdown_nrc.png",
  plot =plot_emotion_breakdown_nrc,
  width = 15,
  height = 10,
  dpi = 300
)

