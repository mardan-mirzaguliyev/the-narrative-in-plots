# DATA PREPARATION

# Load necessary packages
library(tidyverse) # Core data manipulation and visualization (includes ggplot2, dplyr, tidyr, forcats, stringr)
library(pdftools)  # Extracts text from PDF files page by page
library(tibble)    # Creates structured data frames (tibbles) — actually loaded within tidyverse but explicit here
library(tidytext)  # Tokenization and sentiment lexicons (unnest_tokens, get_sentiments)
library(magick)    # Image processing — circular cropping, masking, and border creation for author portraits
library(ggimage)   # Renders images (Marx and Engels portraits) inside ggplot2 visualizations
library(rsvg)      # SVG rendering engine — required by magick for circle_crop() SVG mask operations     
library(scales)    # Number formatting — percent(), comma() for axis labels and table values
library(gt)        # Markdown table generation and saving via kable() and gtsave
library(syuzhet)   # Line-level scoring without tokenisation; better for sentences or paragraphs
library(patchwork) # Combined plots


# DATA PREPARATION
story_path <- "data/paper-menagerie.pdf"
story_raw <- pdf_text(story_path)


## Build a data frame from the raw text
### Collapse all text first
story_full_text <- paste(story_raw, collapse = " ") |> 
  str_squish()


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
story_tokens |> 
  count()


## ANALYSIS

## Download lexicons

afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")


### Compute coverage of each lexicons

### AFINN
coverage_table_afinn <- story_tokens |> 
  left_join(afinn, by = "word") |>
  summarize(
    total_words = n(),
    matched_words = sum(!is.na(value)),
    coverage = percent(matched_words / total_words, accuracy = 0.1))

### Bing
coverage_table_bing <- story_tokens |> 
  left_join(bing, by = "word") |>
  summarize(
    total_words = n(),
    matched_words = sum(!is.na(sentiment)),
    coverage = percent(matched_words / total_words, accuracy = 0.1))

### NRC
coverage_table_nrc <- story_tokens |> 
  left_join(nrc, by = "word", relationship = "many-to-many") |>
  summarize(
    total_words = n_distinct(word),
    matched_words = n_distinct(word[!is.na(sentiment)]),
    coverage = percent(matched_words / total_words, accuracy = 0.1))


coverage_table <- bind_rows(coverage_table_afinn,
                            coverage_table_bing,
                            coverage_table_nrc)

coverage_table <- coverage_table |> 
  mutate(lexicon =c("AFINN", "Bing", "NRC")) |> 
  relocate(lexicon)
coverage_table


### Coverage table
coverage_table |>
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
  gtsave("tables/coverage_table.png")


### Most frequent words - both positive and negative
### With the word 'like'
p1 <- story_tokens |> 
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
  labs(x = "Contribution to sentiment",
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


p2 <- story_tokens |> 
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
  labs(x = "Contribution to sentiment",
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


p2


combined_word_count_with_like <- p1 / p2 +
  plot_annotation(
    title    = "Most frequent positive and negative words",
    subtitle = "Two lexicon approaches: AFINN (top) - Bing (down)",
    caption  = "Data: Paper Menagerie by Ken Liu (2011) | Analysis: tidytext | Visualization: ggplot2",
    theme    = theme(
      plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      plot.caption  = element_text(size = 10, hjust = 0.5),
      plot.background = element_rect(fill = "#cbe8f5", color = NA)
    )
  )
combined_word_count_with_like 



ggsave("plots/combined_sentiment_with_like.png",
       plot   = combined_word_count_with_like,
       width  = 12,
       height = 18,    # tall to accommodate 4 stacked plots
       dpi    = 300,
       bg     = "#cbe8f5")







### Most frequent words - both positive and negative
### Without the word 'like'
p3 <- story_tokens |> 
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
  labs(x = "Contribution to sentiment",
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

p3


p4 <- story_tokens |> 
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
  labs(x = "Contribution to sentiment",
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

p4


combined_word_count_without_like <- p3 / p4 +
  plot_annotation(
    title    = "Most frequent positive and negative words",
    subtitle = "Two lexicon approaches: AFINN (top) - Bing (down) ('like' removed) ",
    caption  = "Data: Paper Menagerie by Ken Liu (2011) | Analysis: tidytext | Visualization: ggplot2",
    theme    = theme(
      plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      plot.caption  = element_text(size = 10, hjust = 0.5),
      plot.background = element_rect(fill = "#cbe8f5", color = NA)
    )
  )


combined_word_count_without_like

ggsave("plots/combined_sentiment_without_like.png",
       plot   = combined_word_count_without_like,
       width  = 12,
       height = 18,    # tall to accommodate 4 stacked plots
       dpi    = 300,
       bg     = "#cbe8f5")



## Emotional arc - AFINN
emotional_arc_afinn_plot <- story_tokens |> 
  left_join(afinn, by = "word") |> 
  group_by(paragraph) |> 
  summarize(
    n_scored = sum(!is.na(value)),
    raw_sum = sum(value, na.rm = TRUE),
    mean_valence = mean(value, na.rm = TRUE),
    # Valence = sum(scores) / (n_scored_words * 5), giving –1 to +1
    paragraph_valence = ifelse(
      n_scored > 0,
      raw_sum / (n_scored * 5),
      0
    ),
    .groups = "drop"
  ) |> 
  arrange(paragraph) |> 
  ggplot(aes(x = paragraph, 
             y = mean_valence,
             color = mean_valence > 0,
             group = 1)) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_point(aes(color = mean_valence > 0), size = 3, show.legend = FALSE) +
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
    aes(label = round(mean_valence, 2),
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
    title = "Emotional Arc - Paragraph Valence",
    subtitle = "AFINN scores between -5 / +5",
    x = NULL,
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

ggsave(
  filename = "plots/05-emotional-arc-afinn.png",
  plot = emotional_arc_afinn_plot,
  width = 15,
  height = 10,
  dpi = 300
)


## Emotional proportions - Bing
### Bing
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


emotional_proportions_bing_plot <- story_tokens |>
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
    x = NULL,
    y = "Proportion"
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


ggsave(
  filename = "plots/06-emotional_proportions_bing_plot.png",
  plot = emotional_proportions_bing_plot,
  width = 15,
  height = 10,
  dpi = 300
)


### NRC
nrc_emotional_proportions_plot <- story_tokens |> 
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
    semtiment = fct_relevel(sentiment,
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
    subtitle = "NRC lexicon — proportion of matched words per emotion",
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


ggsave(
  filename = "plots/07-nrc_emotional_proportions_plot.png",
  plot = nrc_emotional_proportions_plot,
  width = 15,
  height = 10,
  dpi = 300
)

