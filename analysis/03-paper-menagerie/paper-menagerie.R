# DATA PREPARATION

# Load necessary packages
library(tidyverse)
library(pdftools)  
library(tibble)    
library(tidytext)  
library(magick)    
library(ggimage)   
library(rsvg)           
library(scales)    
library(gt)        
library(syuzhet)   
library(patchwork) 
library(ggrepel)   
library(httr2)        
library(jsonlite)  


story_path <- "data/paper-menagerie.pdf"
story_raw <- pdf_text(story_path)


## Build a data frame from the raw text

### Collapse all text first
story_full_text <- paste(story_raw, collapse = " ") |> 
  str_squish()
story_full_text

# Split on paragraph numbers (1 through 13)
story_tibble <- tibble(
  text = str_split(story_full_text,
                   "(?=\\b([1-9]|1[0-3])\\s+[A-Z])")[[1]]
  ) |> 
  filter(str_squish(text) != "") |> 
  mutate(
    paragraph = row_number(),
    text = str_squish(text)
  ) |> 
  select(paragraph, text)

## Tokenize the text
story_tokens <- 
  story_tibble |> 
  unnest_tokens(word, text)

## Quick checks
glimpse(story_tibble)
nrow(story_tibble)

glimpse(story_tokens)
nrow(story_tokens)

head(story_tokens, 20)
tail(story_tokens, 20)

## Total word count
word_count <- story_tokens |> 
  count()
word_count

## ANALYSIS

## Download lexicons

afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")


### Compute coverage of each lexicons

### AFINN
table_coverage_afinn <- story_tokens |> 
  mutate(token_id = row_number()) |> 
  left_join(afinn, by = "word") |>
  summarize(
    total_words = n_distinct(token_id),
    matched_words = n_distinct(token_id[!is.na(value)]),
    coverage = percent(matched_words / total_words, accuracy = 0.1))

### Bing
table_coverage_bing <- story_tokens |> 
  mutate(token_id = row_number()) |> 
  left_join(bing, by = "word") |>
  summarize(
    total_words = n_distinct(token_id),
    matched_words = n_distinct(token_id[!is.na(sentiment)]),
    coverage = percent(matched_words / total_words, accuracy = 0.1))


### NRC
table_coverage_nrc <- story_tokens |> 
  mutate(token_id = row_number()) |> 
  left_join(nrc, by = "word", relationship = "many-to-many") |>
  summarize(
    total_words = n_distinct(token_id),
    matched_words = n_distinct(token_id[!is.na(sentiment)]),
    coverage = percent(matched_words / total_words, accuracy = 0.1))


table_coverage <- bind_rows(table_coverage_afinn,
                            table_coverage_bing,
                            table_coverage_nrc)

table_coverage <- table_coverage |> 
  mutate(lexicon = c("AFINN", "Bing", "NRC")) |> 
  relocate(lexicon)
table_coverage


### Coverage table
table_coverage |>
  gt() |> 
  cols_label(
    lexicon = "Lexicon",
    total_words = "Total Words",
    matched_words = "Matched Words",
    coverage = "Coverage"
  ) |>
  tab_header(
    title = "Coverage of three Lexicons",
    subtitle = "Matched words ratio to total words"
  ) |> 
  tab_style(
    style = cell_fill(color = "#2a9d8f"),
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = cell_fill(color = "#cbe8f5"),
    locations = cells_body()
  ) |> 
  cols_align(align = "center", columns = everything()) |> 
  gtsave("tables/01-table_coverage.png")


### Most frequent words - both positive and negative
### With the word 'like'
plot_most_frequent_words_afinn_with_like <- story_tokens |> 
  left_join(afinn, by = "word") |> 
  filter(!is.na(value)) |> 
  mutate(
    sentiment = case_when(
      value > 0 ~ "positive",
      value < 0 ~ "negative",
      value == 0 ~ "neutral"
    )
  ) |> 
  count(word, sentiment, sort = TRUE) |> 
  ungroup() |> 
  group_by(sentiment) |> 
  slice_max(n, n = 10) |> 
  ungroup() |> 
  mutate(word = reorder(word, n)) |> 
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = NULL,
       y = NULL) +
  geom_label(
    aes(label = n),
    vjust    = 0.5,      # pushes label above the point
    colour   = "black",
    fontface = "bold",
    size     = 3.5,
    fill     = "white",   # label background
    linewidth = 0.2      # border thickness around label box
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_most_frequent_words_afinn_with_like


plot_most_frequent_words_bing_with_like <- story_tokens |> 
  left_join(bing, by = "word") |> 
  filter(!is.na(sentiment)) |> 
  count(word, sentiment, sort = TRUE) |> 
  ungroup() |> 
  group_by(sentiment) |> 
  slice_max(n, n = 10) |> 
  ungroup() |> 
  mutate(word = reorder(word, n)) |> 
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = NULL,
       y = NULL) +
  geom_label(
    aes(label = n),
    vjust    = 0.5,      # pushes label above the point
    colour   = "black",
    fontface = "bold",
    size     = 3.5,
    fill     = "white",   # label background
    linewidth = 0.2      # border thickness around label box
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_most_frequent_words_bing_with_like


plot_combined_word_count_with_like <- plot_most_frequent_words_afinn_with_like / plot_most_frequent_words_bing_with_like +
  plot_annotation(
    title    = "Most frequent positive and negative words",
    subtitle = "Two lexicon approaches: AFINN (top) - Bing (down)",
    caption  = "Data: Paper Menagerie by Ken Liu (2011)",
    theme    = theme(
      plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      plot.caption  = element_text(size = 10, hjust = 0.5),
      plot.background = element_rect(fill = "#cbe8f5", color = NA)
    )
  )

plot_combined_word_count_with_like 


ggsave("plots/01-plot_combined_word_count_with_like.png",
       plot = plot_combined_word_count_with_like,
       width  = 12,
       height = 18,    # tall to accommodate 4 stacked plots
       dpi    = 300,
       bg     = "#cbe8f5")



### Most frequent words - both positive and negative
### Without the word 'like'
plot_most_frequent_words_afinn_without_like <- story_tokens |> 
  left_join(afinn, by = "word") |> 
  filter(!is.na(value)) |> 
  filter(word != "like") |> 
  mutate(
    sentiment = case_when(
      value > 0 ~ "positive",
      value < 0 ~ "negative",
      value == 0 ~ "neutral"
    )
  ) |> 
  count(word, sentiment, sort = TRUE) |> 
  ungroup() |> 
  group_by(sentiment) |> 
  slice_max(n, n = 10) |> 
  ungroup() |> 
  mutate(word = reorder(word, n)) |> 
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = NULL,
       y = NULL) +
  geom_label(
    aes(label = n),
    vjust    = 0.5,      # pushes label above the point
    colour   = "black",
    fontface = "bold",
    size     = 3.5,
    fill     = "white",   # label background
    linewidth = 0.2      # border thickness around label box
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_most_frequent_words_afinn_without_like


plot_most_frequent_words_bing_without_like <- story_tokens |> 
  left_join(bing, by = "word") |> 
  filter(!is.na(sentiment)) |> 
  filter(word != "like") |> 
  count(word, sentiment, sort = TRUE) |> 
  ungroup() |> 
  group_by(sentiment) |> 
  slice_max(n, n = 10) |> 
  ungroup() |> 
  mutate(word = reorder(word, n)) |> 
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = NULL,
       y = NULL) +
  geom_label(
    aes(label = n),
    vjust    = 0.5,      # pushes label above the point
    colour   = "black",
    fontface = "bold",
    size     = 3.5,
    fill     = "white",   # label background
    linewidth = 0.2      # border thickness around label box
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_most_frequent_words_bing_without_like


plot_combined_word_count_without_like <- plot_most_frequent_words_afinn_without_like / plot_most_frequent_words_bing_without_like +
  plot_annotation(
    title    = "Most frequent positive and negative words",
    subtitle = "Two lexicon approaches: AFINN (top) - Bing (down) ('like' removed) ",
    caption  = "Data: Paper Menagerie by Ken Liu (2011)",
    theme    = theme(
      plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      plot.caption  = element_text(size = 10, hjust = 0.5),
      plot.background = element_rect(fill = "#cbe8f5", color = NA)
    )
  )

plot_combined_word_count_without_like


ggsave("plots/02-plot_combined_word_count_without_like.png",
       plot   = plot_combined_word_count_without_like,
       width  = 12,
       height = 18,    # tall to accommodate 4 stacked plots
       dpi    = 300,
       bg     = "#cbe8f5")


## Emotional arc - AFINN
story_afinn <- story_tokens |> 
  left_join(afinn, by = "word") |> 
  group_by(paragraph) |> 
  summarize(
    n_scored = sum(!is.na(value)),
    raw_sum = sum(value, na.rm = TRUE),
    mean_valence = mean(value, na.rm = TRUE),
    # Valence = sum(scores) / (n_scored_words * 5), giving –1 to +1
    normalized_score = ifelse(
      n_scored > 0,
      raw_sum / (n_scored * 5),
      0
    ),
    .groups = "drop"
  ) |> 
  arrange(paragraph)

story_afinn


plot_emotional_arc_afinn <- story_afinn |> 
  ggplot(aes(x = paragraph,
                          y = normalized_score,
                          color = normalized_score > 0,
                          group = 1)) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_point(aes(color = normalized_score > 0), size = 3, show.legend = FALSE) +
  annotate(
    "segment",
    x = 1,
    xend = 13,
    y = 0,
    yend = 0,
    linetype = "dashed",
    color = "#a8d8d2",
    linewidth = 0.6
  ) +
  scale_color_manual(values = c("TRUE" = "#2a9d8f",
                                "FALSE" = "#bd1515")) +
  scale_x_continuous(
    breaks = seq(1, 13, by = 1),
    limits = c(1, 14)
  ) +
  geom_label(
    aes(label = round(normalized_score, 2),
        hjust = ifelse(paragraph == 13, 1.5, -0.5)),
    vjust    = -0.2,      # pushes label above the point
    colour   = "black",
    fontface = "bold",
    size     = 4,
    fill     = "white",   # label background
    linewidth = 0.2,      # border thickness around label box
    show.legend = FALSE
  ) +
  labs(
    title = "Paragraph Level Emotional Valence",
    subtitle = "Normalized AFINN scores between -1 / +1",
    caption  = "Data: Paper Menagerie by Ken Liu (2011)",
    x = NULL,
    y = NULL
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

plot_emotional_arc_afinn


ggsave(
  filename = "plots/03-plot_emotional_arc_afinn.png",
  plot = plot_emotional_arc_afinn,
  width = 15,
  height = 10,
  dpi = 300
)


## Emotional proportions - Bing

### Build positive and negative labels
bing_neg <-  story_tokens |>
  left_join(bing, by = "word") |>
  filter(!is.na(sentiment)) |>
  group_by(paragraph, sentiment) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(paragraph) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |> 
  filter(sentiment == "negative")

bing_pos <- story_tokens |>
  left_join(bing, by = "word") |>
  filter(!is.na(sentiment)) |>
  group_by(paragraph, sentiment) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(paragraph) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |> 
  filter(sentiment == "positive")


plot_emotional_proportions_bing <- story_tokens |>
  left_join(bing, by = "word") |>
  filter(!is.na(sentiment)) |>
  group_by(paragraph, sentiment) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(paragraph) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  complete(paragraph, sentiment, fill = list(n = 0, prop = 0)) |> 
  ggplot(aes(x = paragraph, y = prop, fill = sentiment)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_label(
    data = bing_neg,
    aes(x = paragraph,
        y = prop / 2,
        label = percent(prop, accuracy = 0.1)),
    hjust       = 0.5,
    vjust       = 0.5,
    nudge_x     = -0.225,    # nudge left to sit on negative bar
    colour      = "black",
    fontface    = "bold",
    size        = 4,
    fill        = "white",
    linewidth   = 0.2,
    show.legend = FALSE
  ) +
  geom_label(
    data = bing_pos,
    aes(x = paragraph,
        y = prop / 2,
        label = percent(prop, accuracy = 1)),
    hjust = 0.5,
    vjust = 0.5,
    nudge_x = 0.225, # nudge right to sit on positive bar
    colour = "black",
    fontface = "bold",
    size = 4,
    fill = "white",
    linewidth = 0.2,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c(
    "negative" = "#bd1515",   # red for negative
    "positive" = "#2a9d8f"    # teal for positive
  )) +
  scale_x_continuous(
    breaks = seq(1, 13, by = 1),
    limits = c(0.5, 13.5)
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Positive and Negative Proportions of Paragraphs",
    subtitle = "Bing Scores",
    caption  = "Data: Paper Menagerie by Ken Liu (2011)",
    x = NULL,
    y = NULL
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
    legend.position = "bottom"
  )

plot_emotional_proportions_bing


ggsave(
  filename = "plots/04-plot_emotional_proportions_bing.png",
  plot = plot_emotional_proportions_bing,
  width = 15,
  height = 10,
  dpi = 300
)


### Filter out paragraph 13 which has 100% negative sentiment
story_tokens |>
  left_join(bing, by = "word") |>
  filter(!is.na(sentiment)) |>
  group_by(paragraph, sentiment) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(paragraph) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |> 
  filter(paragraph == 13)


story_tokens |>
  left_join(bing, by = "word") |>
  filter(!is.na(sentiment)) |> 
  filter(paragraph == 13)


### NRC emotion profile
plot_nrc_emotional_proportions <- story_tokens |> 
  left_join(nrc, by = "word", relationship = "many-to-many") |> 
  filter(!is.na(sentiment)) |> 
  group_by(paragraph, sentiment) |> 
  summarize(n = n(), .groups = "drop") |> 
  group_by(paragraph) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  select(-n) |> 
  filter(!sentiment %in% c("positive", "negative")) |> 
  mutate(
    sentiment = fct_relevel(sentiment,
                            "joy", "trust", "anticipation", "surprise",
                            "anger", "disgust", "fear", "sadness")) |> 
  ggplot(aes(x = paragraph, y = sentiment, fill = prop)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = percent(prop, accuracy = 0.1)),
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  scale_x_continuous(
    breaks = seq(1, 13, by = 1),
    limits = c(0.5, 13.5)
    ) +
  scale_fill_gradient2(
    low = "#f7f7f7",
    mid = "#a8d8d2",
    high = "#2a9d8f",
    midpoint = 0.08,
    labels = percent
  ) +
  labs(
    title = "Emotion Profile Across Paragraphs",
    subtitle = "NRC lexicon: proportion of matched words per emotion",
    caption  = "Data: Paper Menagerie by Ken Liu (2011)",
    x = NULL,
    y = NULL,
    fill = "Proportion"
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

plot_nrc_emotional_proportions

ggsave(
  filename = "plots/05-plot_nrc_emotional_proportions.png",
  plot = plot_nrc_emotional_proportions,
  width = 15,
  height = 10,
  dpi = 300
)


### Filter out paragraph 11 which has highest anticipation rate - 37.5%
story_tibble |> 
  filter(paragraph == 11)

story_tokens |> 
  filter(paragraph == 11) |> 
  left_join(nrc, by = "word", relationship = "many-to-many") |> 
  filter(!is.na(sentiment)) |> 
  count(sentiment)
  

### syuzhet sentiments
story_scored_syuzhet <- story_tibble |>
  mutate(syuzhet_score = get_sentiment(text, method = "syuzhet"))

# Normalise to –1 / +1
# syuzhet scores are unbounded, so we divide by the observed maximum
# to get a scale anchored between –1 and +1

max_abs <- max(abs(story_scored_syuzhet$syuzhet_score))
max_abs

story_scored_syuzhet <- story_scored_syuzhet |> 
  mutate(normalized_score = syuzhet_score / max_abs)

# Overall valence = mean of all normalised line scores
overall_valence_syuzhet <- mean(story_scored_syuzhet$normalized_score)

cat(sprintf("Overall emotional valence: %+.3f\n", overall_valence_syuzhet))


plot_syuzhet_story_scored <- story_scored_syuzhet |>
  ggplot(aes(x = paragraph, y = normalized_score)) +
  geom_col(aes(fill = normalized_score), show.legend = FALSE) +
  scale_fill_gradient2(
    low      = "#bd1515",
    mid      = "grey90",
    high     = "#2a9d8f",
    midpoint = 0
  ) +
  geom_label(
    aes(label = round(normalized_score, 2),
        vjust = ifelse(normalized_score >= 0, -0.6, 1.6)), # above positive, below negative bars
    colour   = "black",
    fontface = "bold",
    size     = 3.5,
    fill     = "white",   # label background
    linewidth = 0.2      # border thickness around label box
  ) +
  scale_x_continuous(breaks = seq(1, max(story_scored_syuzhet$paragraph), by = 1)) +
  scale_y_continuous(limits = c(
    min(story_scored_syuzhet$normalized_score) - 0.2,
    max(story_scored_syuzhet$normalized_score) + 0.2
  )) +
  labs(
    title    = "Paragraph 6 is the most positive of the story",
    subtitle = "Paragraph-by-paragraph emotional trajectory (syuzhet)",
    caption = "Ken Liu, Paper Menagerie (2011)",
    x = NULL,
    y = NULL
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

plot_syuzhet_story_scored


ggsave(
  filename = "plots/06-plot_syuzhet_story_scored.png",
  plot = plot_syuzhet_story_scored,
  width = 15,
  height = 10,
  dpi = 300
)


### Sentence level division

### Split sentences
story_sentences <- story_tibble |> 
  unnest_sentences(sentence, text) |> 
  mutate(
    sentence = str_remove(sentence, "^\\d+\\s+"),
    sentence = str_to_sentence(sentence),
    sentence_id = row_number()
  ) |> 
  select(sentence_id, paragraph, sentence)


### Tokenize sentences - Not used here but sentence id can be used to group
sentence_tokens <- story_sentences |> 
  unnest_tokens(word, sentence)


# Scoring and then normalizing the scores
# syuzhet scores are unbounded, so we divide by the observed maximum
# Normalise to –1 / +1 to get a scale anchored between –1 and +1
story_scored_syuzhet_sentence <- story_sentences |>
  mutate(syuzhet_score = get_sentiment(sentence, method = "syuzhet"))

max_abs_sentences <- max(abs(story_scored_syuzhet_sentence$syuzhet_score))
max_abs_sentences

story_scored_syuzhet_sentence <- story_scored_syuzhet_sentence |> 
  mutate(normalized_score = syuzhet_score / max_abs_sentences)

### Checks
story_scored_syuzhet_sentence$normalized_score

glimpse(story_scored_syuzhet_sentence)
summary(story_scored_syuzhet_sentence)
story_scored_syuzhet_sentence |> 
  filter(syuzhet_score != 0) |> 
  nrow()

### 5 most positive sentences table
five_most_positive <- story_scored_syuzhet_sentence |> 
  slice_max(normalized_score, n = 5, with_ties = FALSE) |> 
  select(sentence_id, sentence, normalized_score)
five_most_positive

five_most_positive |>
  select(sentence, normalized_score) |> 
  gt() |> 
  cols_label(
    sentence = "Sentence",
    normalized_score = "Sentiment Score"
  ) |>
  tab_header(
    title = "Sentence level sentiments: Five most positive sentences (syuzhet)",
    subtitle = ""
  ) |> 
  tab_style(
    style = cell_fill(color = "#2a9d8f"),
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = cell_fill(color = "#cbe8f5"),
    locations = cells_body()
  ) |> 
  cols_align(align = "center", columns = everything()) |> 
  gtsave("tables/02-table_five-most-positive.png")


### 5 most negative sentences table
five_most_negative <- story_scored_syuzhet_sentence |> 
  slice_min(normalized_score, n = 5, with_ties = FALSE) |> 
  select(sentence_id, sentence, normalized_score)
five_most_negative

five_most_negative |>
  select(sentence, normalized_score) |> 
  gt() |> 
  cols_label(
    sentence = "Sentence",
    normalized_score = "Sentiment Score"
  ) |>
  tab_header(
    title = "Sentence level sentiments: Five most negative sentences (syuzhet)",
    subtitle = "Three negative sentences ties, so, there 6 sentences in negatives"
  ) |> 
  tab_style(
    style = cell_fill(color = "#2a9d8f"),
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = cell_fill(color = "#cbe8f5"),
    locations = cells_body()
  ) |> 
  cols_align(align = "center", columns = everything()) |> 
  gtsave("tables/03-table_five-most-negative.png")


extremes <- bind_rows(five_most_positive, five_most_negative) |> 
  mutate(sentence = str_to_sentence(sentence)) |> 
  mutate(sentence = str_replace_all(sentence, "\\bi\\b", "I"))
extremes

#### EMOTIONAL ARC
plot_syuzhet_sentence <- story_scored_syuzhet_sentence |> 
  ggplot(aes(x = sentence_id, y = normalized_score)) +
  geom_smooth(se = FALSE,
              color = "#2a9d8f",
              linewidth = 1.2,
              method = "loess",
              span = 0.3,
              bg = NA) + # adjust 0.1-0.5 for smoother/rougher arc
  annotate("segment",
           x = 1,
           xend = 431,
           y = 0,
           yend = 0,
           linetype = "dashed",
           color = "grey60",
           linewidth = 0.5) +
  ## All points - small and faint
  geom_jitter(aes(color = normalized_score > 0),
            size = 0.8,
            alpha = 0.2,
            height = 0.008, # tiny vertical jitter only
            width = 0,       # no horizontal jitter — preserves sentence order  
            show.legend = FALSE) +
  # Extreme points larger
  geom_point(data = extremes,
             aes(color = normalized_score > 0),
             size = 3,
             show.legend = FALSE) +
  # Labels for extreme sentences
  geom_label_repel(
    data = extremes,
    aes(label = str_wrap(sentence, width = 30)),
        size = 2.8,
        fontface = "bold",
        fill = "white",
        color = "black",
        linewidth = 0.2,
        box.padding = 0.5,
        max.overlaps = Inf,
        show.legend = FALSE) +
      scale_color_manual(
        values = c("TRUE" = "#2a9d8f",
                   "FALSE" = "#bd1515")) +
      scale_x_continuous(
        breaks = seq(1, 431, by = 43), # roughly every 10% of story
        labels = seq(1, 431, by = 43)
      ) +
      scale_y_continuous(
        limits = c(-1.2, 1.2),
        oob = squish,
        breaks = c(-1.0, -0.5, 0, 0.5, 1.0),
        labels = c("1.0", "-0.5", "Neutral", "0.5", "1.5")
      ) +
      labs(
        title = "Sentence-Level Emotional Arc",
        subtitle = "Syuzhet scores normalized to -1/+1: Labelled sentences are emotional extremes",
        caption = "Ken Liu, Paper Menagerie (2011)",
        x = "Sentence",
        y = "Normalized Sentiment Score",
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
      
plot_syuzhet_sentence


ggsave(
  filename = "plots/07-plot_syuzhet_sentence.png",
  plot = plot_syuzhet_sentence,
  width = 15,
  height = 10,
  dpi = 300
)


### Bonus - LLM Approach
gemma4_latest <- "gemma4:latest"


score_local <- function(sentence_text, model = "llama3.2:latest") {
  req <- request("http://localhost:11434/api/generate") |> 
    req_method("POST") |> # Force POST method
    req_body_json(list(
      model = model,
      prompt = paste0(
        "Analyze the sentiment of the provided sentence. ",
        "Return a 'valence' score strictly between -1.0 (extremely negative) and 1.0 (extremely positive). ",
        "0.0 is perfectly neutral. Provide only valid JSON with the following structure: ",
        '{"valence": <float>, "primary_emotion": "<word>", "reasoning": "<short phrase>"}',
        "\n\nSentence: \"", sentence_text, "\""
      ),
      stream = FALSE
    ))
  
  # Perform the request and store the result in 'resp'
  resp <- req_perform(req)
  
  # Now pass the 'resp' object to extract the body
  txt <- resp_body_json(resp)$response
  
  # Clean potential markdown wrapping
  txt <- gsub("```json|```", "", txt)
  
  return(fromJSON(txt))
}


example_sentences <- story_scored_syuzhet_sentence |> 
  filter(sentence_id %in% extremes$sentence_id) |> 
  select(sentence_id, sentence, normalized_score) |> 
  arrange(desc(normalized_score)) |>
  rename(syuzhet_score = normalized_score)
  

# This will return NA instead of crashing if a single sentence fails
safe_score <- possibly(score_local, otherwise = list(valence = NA, primary_emotion = NA, reasoning = NA))

## llama3.2:latest
# Process only the first few to test, then use map_dfr for the full list
results_llama3_2_latest <- example_sentences |>
  mutate(sentiment_data = map(sentence, ~score_local(.x))) |>
  tidyr::unnest_wider(sentiment_data)
results_llama3_2_latest


## gemma4_latest
results_gemma4_latest <- example_sentences |>
  mutate(sentiment_data = map(sentence, ~score_local(.x, model = gemma4_latest))) |>
  tidyr::unnest_wider(sentiment_data)
results_gemma4_latest


### Build a comparison table with two models and syuzhet
## llama3.2:latest
results_llama3_2_latest |>
  select(sentence, syuzhet_score, valence, primary_emotion, reasoning) |> 
  mutate(syuzhet_score = round(syuzhet_score, 1), 
         valence = round(valence, 1)) |> 
  gt() |> 
  cols_label(
    sentence = "Sentence",
    syuzhet_score = "Syuzhet Score",
    valence = "LLM Score",
    primary_emotion = "Primary Emotion",
    reasoning = "LLM Reasoning"
  ) |>
  tab_header(
    title = "Comparison of Syuzhet Results and Local LLM llama3.2:latest",
    subtitle = "Both syuzhet and LLM scores normalized"
  ) |> 
  tab_style(
    style = cell_fill(color = "#2a9d8f"),
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = cell_fill(color = "#cbe8f5"),
    locations = cells_body()
  ) |> 
  cols_align(align = "center", columns = everything()) |> 
  gtsave("tables/04-table_syuzhet_vs_llama3_2_latest.png")


## gemma4_latest
results_gemma4_latest |>
  select(sentence, syuzhet_score, valence, primary_emotion, reasoning) |> 
  mutate(syuzhet_score = round(syuzhet_score, 1), 
         valence = round(valence, 1)) |> 
  gt() |> 
  cols_label(
    sentence = "Sentence",
    syuzhet_score = "Syuzhet Score",
    valence = "LLM Score",
    primary_emotion = "Primary Emotion",
    reasoning = "LLM Reasoning"
  ) |>
  tab_header(
    title = "Comparison of Syuzhet Results and Local LLM gemma4:latest",
    subtitle = "Both syuzhet and LLM scores normalized"
  ) |> 
  tab_style(
    style = cell_fill(color = "#2a9d8f"),
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = cell_fill(color = "#cbe8f5"),
    locations = cells_body()
  ) |> 
  cols_align(align = "center", columns = everything()) |> 
  gtsave("tables/05-table_syuzhet_vs_gemma4_latest.png")




