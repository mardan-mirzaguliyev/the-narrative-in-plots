library(rulexicon)      # Access to RuSentiLex lexicon 
library(dplyr)          # Data manipulation 
library(tidyr)          # Reshaping: pivot_wider function
library(ggplot2)        # Data visualization
library(forcats)
library(textdata)       # Downloads and caches AFINN / Bing / NRC lexicons locally
library(stringr)        # String ops: str_replace_all for contraction expansion
library(tibble)         # tribble() for readable row-by-row data entry; clean printing
library(scales)         # Axis formatters: label_percent(), label_number()
library(googlesheets4)  # Reads lyrics dataframe directly from Google Sheets
library(tidytext)       # Tokenisation pipeline: unnest_tokens + lexicon joins
library(syuzhet)        # Line-level scoring without tokenisation; better for lyrics
library(ggrepel)
library(gt)

# DATA PREPARATION
# Changes, 2Pac syuzhet pipeline

## Read lyrics of song into R 
## Authenticate Google account that contains lyrics file
gs4_deauth()

## Read individual sheets of lyrics
lyrics_id <- "13dM_5vWQGdAeRI-thE6nbzlZIbvbZ8dw9eCu3MsP9Kk"


# Changes 2Pac
lyrics_raw_2pac <- read_sheet(lyrics_id, "Changes 2Pac")


## Syuzhet scoring for 2Pac, Changes 
lyrics_scored_2pac <- lyrics_raw_2pac |>
  mutate(syuzhet_score = get_sentiment(text, method = "syuzhet"))
lyrics_scored_2pac


# Normalise to –1 / +1
# syuzhet scores are unbounded, so we divide by the observed maximum
# to get a scale anchored between –1 and +1
max_abs <- max(abs(lyrics_scored_2pac$syuzhet_score))
max_abs

lyrics_scored_2pac <- lyrics_scored_2pac |> 
  mutate(normalized_score = syuzhet_score / max_abs)
lyrics_scored_2pac


section_order <- c("Verse 1", "Chorus 1", "Verse 2", "Chorus 2", "Interlude", "Verse 3", "Chorus 3")

## Most negative and most positive scores
extremes <- 
  lyrics_scored_2pac |> 
  group_by(section) |> 
  summarize(
    section_min = min(normalized_score),
    section_max = max(normalized_score))

plot_2pac_section_scores <- lyrics_scored_2pac |>
  mutate(section = factor(section, levels = section_order)) |>
  ggplot(aes(x = section, y = normalized_score)) +
  geom_boxplot(fill = "#e0f2f9",
               color = "black",
               outlier.color = "red",
               median.linewidth = 0,
               alpha = 0.5) +
  geom_jitter(aes(colour = normalized_score), width = 0.2, show.legend = FALSE) +
  scale_color_gradient2(
    low      = "#bd1515",
    mid      = "grey90",
    high     = "#2a9d8f",
    midpoint = 0
  ) +
  geom_label_repel(
    data = extremes |> filter(section_max != 0),
    aes(x = section,
        y = section_max,
        label = round(section_max, 2)),
    colour   = "#2a9d8f",
    fontface = "bold",
    size     = 3.5,
    fill     = "white",
    box.padding = 0.5,
    point.padding = 0.5,
    segment.color = "grey50"
  ) +
  geom_label_repel(
    data = extremes |> filter(section_min != 0),
    aes(x = section, 
        y = section_min,
        label = round(section_min, 2)),
    colour = ifelse(extremes |> 
                      filter(section_min != 0) |> 
                      pull(section_min) < 0, "#bd1515", "#2a9d8f"),
    fontface = "bold",
    size     = 3.5,
    fill     = "white",
    box.padding = 0.5,
    point.padding = 0.5,
    segment.color = "grey50"
  ) +
  scale_y_continuous(
    limits = c(-1.2, 1.2),
    oob = scales::squish,
    breaks = c(-1.0, -0.5, 0, 0.5, 1.0),
    labels = c("-1.0", "-0.5", "0", "0.5", "1") # Changed "Neutral" to "0"
  ) +
  labs(
    title    = "'Changes, 2Pac'",
    subtitle = "Cross sectional emotional trajectory (syuzhet)",
    x        = "Section",
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

plot_2pac_section_scores

ggsave(
  filename = "plots/01-plot_2pac_section_scores.png",
  plot = plot_2pac_section_scores,
  width = 15,
  height = 10,
  dpi = 300
)


lyrics_scored_2pac |> 
  filter(section == "Chorus 3") |> 
  mutate(normalized_score = percent(normalized_score, accuracy = 0.2)) |> 
  select(line, text, normalized_score) |> 
  gt() |> 
  cols_label(
    line = "Line Number",
    text = "Line",
    normalized_score = "Score"
  ) |>
  tab_header(
    title = "Scored Lines of Chorus 3",
    subtitle = "Only two lines scored: 96 an 98 (Normalized syuzhet scores)"
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
    locations = cells_body(rows = normalized_score != "0.0%")
  ) |> 
  cols_align(align = "center", columns = everything()) |> 
  gtsave("tables/01-table_chorus_3_scored_lines.png")



# Changes Kino

## Access to RuSentiLex lexicon
rusentilex_data <- hash_rusentilex_2017
head(rusentilex_data)


lyrics_raw_kino <- read_sheet(lyrics_id, "Changes Kino")

lyrics_tokens_kino <- lyrics_raw_kino |> 
  unnest_tokens(token, text)


# 1. Clean the lexicon: resolve duplicates by picking the strongest sentiment
sentiment_map <- c("positive" = 1, "negative" = -1, "neutral" = 0)

lexicon_clean <- rusentilex_data |>
  filter(source == "opinion") |>
  mutate(val = sentiment_map[sentiment]) |>
  group_by(token) |>
  slice_max(abs(val), with_ties = FALSE) |>
  ungroup() |>
  select(token, val)

# 2. Define Valence Shifters
valence_shifters <- tibble(
  token = c("не", "нет", "ни", "без", "никогда", "очень", "сильно", "слишком", "весьма"),
  shift = c(rep(-1, 5), rep(1.5, 4))
)


# Rename based on position (e.g., column 3) instead of name
lyrics_scored <- lyrics_tokens_kino |>
  ungroup() |>
  rename(lemma = 3) |>  # Replace 3 with the actual column index
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


section_sentiment <- lyrics_scored |>
  group_by(section) |>
  summarize(
    total_sentiment = sum(final_score, na.rm = TRUE),
    lexical_density = mean(final_score != 0, na.rm = TRUE)
  )


