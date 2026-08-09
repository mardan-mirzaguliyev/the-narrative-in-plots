library(tidyverse)
library(googlesheets4)
library(gt)


gs4_auth()

lyrics_id <- "1oLgGhdmRUwd8qIetS2wEgRFqxl0fiEJNstoHFfGQXBw"
lyrics_raw <- read_sheet(lyrics_id, sheet = "Impossible Dream")
lyrics_raw


# Hugging Face

# Method 1: Cardiff, hosted classifier

## Cardiff emotional label set
cardiff_labels <- get_model_labels("cardiffnlp/twitter-roberta-base-emotion-multilabel-latest")
cardiff_labels 


lyrics_cardiff <- lyrics_raw |> 
  get_or_run_hf(text, output_name = "data/lyrics", 
                model = "cardiffnlp/twitter-roberta-base-emotion-multilabel-latest")

lyrics_cardiff


# Method 2: Hartmann, local classifier

# Hartmann emotional label set
hartmann_labels <- get_model_labels("j-hartmann/emotion-english-distilroberta-base")
hartmann_labels


setup_hf_local(model = "j-hartmann/emotion-english-distilroberta-base")

lyrics_hartmann <- lyrics_raw |> 
  get_or_run_hf_local(text, output_name = "data/lyrics")

lyrics_hartmann


lyrics_hf_comparison <- lyrics_raw |> 
  left_join(lyrics_cardiff |> select(line, cardiff_sentiment = sentiment, cardiff_conf = confidence_score),
            by = "line") |> 
  left_join(lyrics_hartmann |> select(line, hartmann_sentiment = sentiment, hartmann_conf = confidence_score),
            by = "line")

lyrics_hf_comparison


## Label comparison - Cardiff to Hartmann

plot_data_hf <- lyrics_hf_comparison |> 
  mutate(
    agreement = case_when(
      cardiff_sentiment == hartmann_sentiment ~ "Labels match",
      cardiff_sentiment %in% c("Love", "Optimism", "Pessimism") ~ "Cardiff-only category",
      TRUE ~ "Labels differ"
    ),
    line_label = paste0("Line ", line)
  )

# Sort so the three groups cluster together: matches, then Cardiff-only, then differs
plot_data_hf <- plot_data_hf |> 
  mutate(sort_key = case_when(
    agreement == "Labels match" ~ 1,
    agreement == "Cardiff-only category" ~ 2,
    TRUE ~ 3
  )) |> 
  arrange(sort_key, line)

line_order_hf <- rev(plot_data_hf$line_label)


plot_lyrics_hf_comparison <- plot_data_hf |> 
  ggplot(aes(y = factor(line_label, levels = line_order_hf))) +
  geom_tile(
    aes(x = 1.5, fill = agreement),
    width = 1.6, height = 0.9, alpha = 0.18, show.legend = FALSE
  ) +
  geom_segment(
    aes(x = 1, xend = 2, yend = factor(line_label, levels = line_order_hf),
        color = agreement),
    linewidth = 1.8, alpha = 0.95, lineend = "round", show.legend = FALSE
  ) +
  geom_point(aes(x = 1), color = "#264653", size = 4) +
  geom_text(
    aes(x = 1, label = cardiff_sentiment), 
    color = "#264653", size = 3.2, vjust = -1.4, fontface = "bold"
  ) +
  geom_point(aes(x = 2), color = "#e9c46a", size = 4) +
  geom_text(
    aes(x = 2, label = hartmann_sentiment), 
    color = "#b8860b", size = 3.2, vjust = -1.4, fontface = "bold"
  ) +
  geom_text(
    aes(x = 2.4, 
        label = case_when(
          agreement == "Labels match" ~ "✓ match",
          agreement == "Cardiff-only category" ~ "⊘ no equivalent",
          TRUE ~ "✗ differ"
        ),
        color = agreement),
    size = 3.1, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  
  scale_color_manual(values = c(
    "Labels match"          = "#2a9d8f",
    "Cardiff-only category" = "#e9c46a",
    "Labels differ"         = "#e76f51"
  )) +
  scale_fill_manual(values = c(
    "Labels match"          = "#2a9d8f",
    "Cardiff-only category" = "#e9c46a",
    "Labels differ"         = "#e76f51"
  )) +
  scale_x_continuous(
    breaks = c(1, 2), 
    labels = c("Cardiff", "Hartmann"), 
    limits = c(0.5, 3)
  ) +
  labs(
    title = "Cardiff vs. Hartmann: Same Lines, Different Labels",
    subtitle = paste0(
      sum(plot_data_hf$agreement == "Labels match"), " match; ",
      sum(plot_data_hf$agreement == "Cardiff-only category"), " use a category Hartmann doesn't have; ",
      sum(plot_data_hf$agreement == "Labels differ"), " differ outright"
    ),
    caption = "Data: The Impossible Dream, Man of La Mancha (1965)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 10, hjust = 0.5),
    axis.text         = element_text(size = 11, color = "black"),
    panel.grid         = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position   = "none"
  )

plot_lyrics_hf_comparison


ggsave(
  filename = "plots/01-lyrics_hf_label_comparison.png",
  plot = plot_lyrics_hf_comparison,
  width = 11,
  height = 12,
  dpi = 300
)


# Confidence Score comparison
optimism_lines <- plot_data_hf |> 
  filter(agreement == "Cardiff-only category") |> 
  mutate(
    line_label = paste0("Line ", line, ": ", hartmann_sentiment),
    conf_diff = cardiff_conf - hartmann_conf
  ) |> 
  arrange(line)

line_order_optimism <- rev(unique(optimism_lines$line_label))


plot_optimism_confidence <- optimism_lines |> 
  ggplot(aes(y = factor(line_label, levels = line_order_optimism))) +
  geom_segment(
    aes(x = hartmann_conf, xend = cardiff_conf, yend = factor(line_label, levels = line_order_optimism)),
    color = "#adb5bd", linewidth = 1.5, alpha = 0.7, lineend = "round"
  ) +
  geom_point(aes(x = cardiff_conf), color = "#264653", size = 4) +
  geom_text(
    aes(x = cardiff_conf, label = scales::percent(cardiff_conf, accuracy = 1)), 
    color = "#264653", size = 3, vjust = -1.3, fontface = "bold"
  ) +
  
  geom_point(aes(x = hartmann_conf), color = "#e9c46a", size = 4) +
  geom_text(
    aes(x = hartmann_conf, label = scales::percent(hartmann_conf, accuracy = 1)), 
    color = "#b8860b", size = 3, vjust = -1.3, fontface = "bold"
  ) +
  
  scale_x_continuous(labels = scales::percent, limits = c(0, 1.05), breaks = scales::pretty_breaks(n = 6)) +
  labs(
    title = "How Confident Is Each Model on the 11 \"Optimism-Only\" Lines?",
    subtitle = "Dark dot = Cardiff's confidence in \"Optimism\"; gold dot = Hartmann's confidence in its own (necessarily different) label",
    caption = "Data: The Impossible Dream, Man of La Mancha (1965)",
    x = "Confidence Score",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 10, hjust = 0.5),
    axis.text         = element_text(size = 11, color = "black"),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_optimism_confidence


ggsave(
  filename = "plots/02-lyrics_optimism_confidence.png",
  plot = plot_optimism_confidence,
  width = 11,
  height = 9,
  dpi = 300
)



optimism_lines_summary <- optimism_lines |> 
  summarize(mean_cardiff_conf = mean(cardiff_conf), 
            mean_hartmann_conf = mean(hartmann_conf),
            mean_diff = mean(conf_diff))

optimism_lines_summary |> 
  gt() |> 
  cols_label(
    mean_cardiff_conf = "Cardiff (Optimism)",
    mean_hartmann_conf = "Hartmann (substitute label)",
    mean_diff = "Difference"
  ) |> 
  fmt_percent(columns = everything(), decimals = 1) |> 
  tab_header(
    title = "Confidence Gap on Cardiff's \"Optimism\"-Only Lines",
    subtitle = "Average model confidence across the 11 lines where Cardiff used a category Hartmann doesn't have"
  ) |> 
  tab_caption("Data: The Impossible Dream, Man of La Mancha (1965)") |> 
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
  # Flag the difference column, since that's the headline number
  tab_style(
    style = list(cell_fill(color = "#f4a261"), cell_text(weight = "bold")),
    locations = cells_body(columns = mean_diff)
  ) |> 
  cols_align(align = "center", columns = everything()) |> 
  gtsave("tables/02-cardiff-optimism-only-conf-score-comparison-hartmann.png")


# Claude Sonnet 

## Cardiff-constrained, no Neutral option available
lyrics_sonnet5_cardiff_labels <- lyrics_raw |> 
  get_or_run_claude_synch(text, output_name = "data/lyrics",
                   labels = cardiff_labels)

lyrics_sonnet5_cardiff_labels


lyrics_cardiff_sonnet5_comparison <- lyrics_raw |> 
  left_join(lyrics_cardiff |> select(line, cardiff_sentiment = sentiment, cardiff_conf = confidence_score),
            by = "line") |> 
  left_join(lyrics_sonnet5_cardiff_labels |> 
              select(line, 
                     sonnet5_sentiment = sentiment,
                     sonnet5_conf = confidence_score,
                     sonnet5_reasoning = reasoning),
            by = "line")

lyrics_cardiff_sonnet5_comparison


## Label comparison - Cardiff to Sonnet 5

plot_data_cardiff_sonnet5_comparison <- lyrics_cardiff_sonnet5_comparison |> 
  mutate(
    agreement = case_when(
      cardiff_sentiment == sonnet5_sentiment ~ "Labels match",
      TRUE ~ "Labels differ"
    ),
    line_label = paste0("Line ", line)
  )

# Sort so the three groups cluster together: matches, then Cardiff-only, then differs

plot_data_cardiff_sonnet5_comparison <- lyrics_cardiff_sonnet5_comparison |> 
  mutate(
    agreement = if_else(cardiff_sentiment == sonnet5_sentiment, "Labels match", "Labels differ"),
    line_label = paste0("Line ", line)
  ) |> 
  arrange(desc(agreement == "Labels match"), line)


line_order_cardiff_sonnet5 <- rev(plot_data_cardiff_sonnet5_comparison$line_label)


plot_lyrics_cardiff_sonnet5_comparison <- plot_data_cardiff_sonnet5_comparison |> 
  ggplot(aes(y = factor(line_label, levels = line_order_cardiff_sonnet5))) +
  
  geom_tile(
    aes(x = 1.5, fill = agreement),
    width = 1.6, height = 0.9, alpha = 0.18, show.legend = FALSE
  ) +
  geom_segment(
    aes(x = 1, xend = 2, yend = factor(line_label, levels = line_order_cardiff_sonnet5),
        color = agreement),
    linewidth = 1.8, alpha = 0.95, lineend = "round", show.legend = FALSE
  ) +
  geom_point(aes(x = 1), color = "#264653", size = 4) +
  geom_text(
    aes(x = 1, label = cardiff_sentiment), 
    color = "#264653", size = 3.2, vjust = -1.4, fontface = "bold"
  ) +
  geom_point(aes(x = 2), color = "#e9c46a", size = 4) +
  geom_text(
    aes(x = 2, label = sonnet5_sentiment), 
    color = "#b8860b", size = 3.2, vjust = -1.4, fontface = "bold"
  ) +
  geom_text(
    aes(x = 2.4, 
        label = if_else(agreement == "Labels match", "✓ match", "✗ differ"),
        color = agreement),
    size = 3.1, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  
  scale_color_manual(values = c(
    "Labels match"  = "#2a9d8f",
    "Labels differ" = "#e76f51"
  )) +
  scale_fill_manual(values = c(
    "Labels match"  = "#2a9d8f",
    "Labels differ" = "#e76f51"
  )) +
  scale_x_continuous(
    breaks = c(1, 2), 
    labels = c("Cardiff", "Sonnet 5"), 
    limits = c(0.5, 3)
  ) +
  labs(
    title = "Cardiff vs. Claude Sonnet 5: Same Lines, Same Label Set",
    subtitle = paste0(
      sum(plot_data_cardiff_sonnet5_comparison$agreement == "Labels match"), " match, ",
      sum(plot_data_cardiff_sonnet5_comparison$agreement == "Labels differ"), " differ: both constrained to Cardiff's 11 categories"
    ),
    caption = "Data: The Impossible Dream, Man of La Mancha (1965)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 10, hjust = 0.5),
    axis.text         = element_text(size = 11, color = "black"),
    panel.grid         = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position   = "none"
  )

plot_lyrics_cardiff_sonnet5_comparison


ggsave(
  filename = "plots/03-plot_lyrics_cardiff_sonnet5_comparison.png",
  plot = plot_lyrics_cardiff_sonnet5_comparison,
  width = 14,
  height = 14,
  dpi = 300
)


lyrics_cardiff_sonnet5_comparison |> 
  filter(line == 14) |> 
  dplyr::pull(sonnet5_reasoning)


# Hartmann-constrained 
lyrics_sonnet5_hartmann_labels <- lyrics_raw |> 
  get_or_run_claude_synch(text, output_name = "data/lyrics",
                          labels = hartmann_labels)

lyrics_sonnet5_hartmann_labels


lyrics_hartmann_sonnet5_comparison <- lyrics_raw |> 
  left_join(lyrics_hartmann |> select(line, hartmann_sentiment = sentiment, hartmann_conf = confidence_score),
            by = "line") |> 
  left_join(lyrics_sonnet5_hartmann_labels |> 
              select(line, 
                     sonnet5_sentiment = sentiment,
                     sonnet5_conf = confidence_score,
                     sonnet5_reasoning = reasoning),
            by = "line")

lyrics_hartmann_sonnet5_comparison


## Label comparison - Hartmann to Sonnet 5
plot_data_hartmann_sonnet5_comparison <- lyrics_hartmann_sonnet5_comparison |> 
  mutate(
    agreement = case_when(
      hartmann_sentiment == sonnet5_sentiment ~ "Labels match",
      TRUE ~ "Labels differ"
    ),
    line_label = paste0("Line ", line)
  )

# Sort so the three groups cluster together: matches, then Hartmann-only, then differs

plot_data_hartmann_sonnet5_comparison <- lyrics_hartmann_sonnet5_comparison |> 
  mutate(
    agreement = if_else(hartmann_sentiment == sonnet5_sentiment, "Labels match", "Labels differ"),
    line_label = paste0("Line ", line)
  ) |> 
  arrange(desc(agreement == "Labels match"), line)

line_order_hartmann_sonnet5 <- rev(plot_data_hartmann_sonnet5_comparison$line_label)


plot_lyrics_hartmann_sonnet5_comparison <- plot_data_hartmann_sonnet5_comparison |> 
  ggplot(aes(y = factor(line_label, levels = line_order_hartmann_sonnet5))) +
  geom_tile(
    aes(x = 1.5, fill = agreement),
    width = 1.6, height = 0.9, alpha = 0.18, show.legend = FALSE
  ) +
  geom_segment(
    aes(x = 1, xend = 2, yend = factor(line_label, levels = line_order_hartmann_sonnet5),
        color = agreement),
    linewidth = 1.8, alpha = 0.95, lineend = "round", show.legend = FALSE
  ) +
  geom_point(aes(x = 1), color = "#264653", size = 4) +
  geom_text(
    aes(x = 1, label = hartmann_sentiment), 
    color = "#264653", size = 3.2, vjust = -1.4, fontface = "bold"
  ) +
  geom_point(aes(x = 2), color = "#e9c46a", size = 4) +
  geom_text(
    aes(x = 2, label = sonnet5_sentiment), 
    color = "#b8860b", size = 3.1, vjust = -1.4, fontface = "bold"
  ) +
  geom_text(
    aes(x = 2.4, 
        label = if_else(agreement == "Labels match", "✓ match", "✗ differ"),
        color = agreement),
    size = 3.0, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  
  scale_color_manual(values = c(
    "Labels match"  = "#2a9d8f",
    "Labels differ" = "#e76f51"
  )) +
  scale_fill_manual(values = c(
    "Labels match"  = "#2a9d8f",
    "Labels differ" = "#e76f51"
  )) +
  scale_x_continuous(
    breaks = c(1, 2), 
    labels = c("Hartmann", "Sonnet 5"), 
    limits = c(0.5, 3)
  ) +
  labs(
    title = "Hartmann vs. Claude Sonnet 5: Same Lines, Same Label Set",
    subtitle = paste0(
      sum(plot_data_hartmann_sonnet5_comparison$agreement == "Labels match"), " match, ",
      sum(plot_data_hartmann_sonnet5_comparison$agreement == "Labels differ"), " differ: both constrained to Hartmann's 7 categories"
    ),
    caption = "Data: The Impossible Dream, Man of La Mancha (1965)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 10, hjust = 0.5),
    axis.text         = element_text(size = 11, color = "black"),
    panel.grid         = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position   = "none"
  )

plot_lyrics_hartmann_sonnet5_comparison


ggsave(
  filename = "plots/04-plot_lyrics_hartmann_sonnet5_comparison.png",
  plot = plot_lyrics_hartmann_sonnet5_comparison,
  width = 14,
  height = 14,
  dpi = 300
)


hartmann_labels_with_trust <- c(hartmann_labels, "Trust")
hartmann_labels_with_trust


lyrics_sonnet5_hartmann_labels_with_trust <- lyrics_raw |> 
  get_or_run_claude_synch(text, output_name = "data/lyrics",
                          labels = hartmann_labels_with_trust)



sonnet5_hartman_trust_comparison <- lyrics_raw |> 
  left_join(lyrics_sonnet5_hartmann_labels |> 
              select(line,
                     before_trust_sent = sentiment,
                     before_trust_conf = confidence_score,
                     before_trust_reasoning = reasoning),
            by = "line") |> 
  left_join(lyrics_sonnet5_hartmann_labels_with_trust |> 
              select(line,
                     after_trust_sent = sentiment,
                     after_trust_conf = confidence_score,
                     after_trust_reasoning = reasoning),
            by = "line") |> 
  mutate(
    label_changed = before_trust_sent != after_trust_sent,
    conf_diff = after_trust_conf - before_trust_conf
  )

sonnet5_hartman_trust_comparison

## Reasoning for different label set: Hartmann labels without Trust and Hartmann labels with Trust added
sonnet5_hartman_trust_comparison |> 
  filter(line == 14) |> 
  select(before_trust_reasoning)

sonnet5_hartman_trust_comparison |> 
  filter(line == 14) |> 
  select(after_trust_reasoning)


sonnet5_hartman_trust_comparison |> 
  summarize(n_changed = sum(label_changed), 
            pct_changed = mean(label_changed),
            mean_conf_diff = mean(conf_diff))


## Label comparison — Sonnet 5 without Trust vs. Sonnet 5 with Trust
plot_data_sonnet5_hartman_trust_comparison <- sonnet5_hartman_trust_comparison |> 
  mutate(
    agreement = if_else(before_trust_sent == after_trust_sent, "Labels match", "Labels differ"),
    line_label = paste0("Line ", line)
  ) |> 
  arrange(desc(agreement == "Labels match"), line)

line_order_sonnet5_hartman_trust_comparison <- rev(plot_data_sonnet5_hartman_trust_comparison$line_label)

plot_lyrics_sonnet5_hartman_trust_comparison <- plot_data_sonnet5_hartman_trust_comparison |> 
  ggplot(aes(y = factor(line_label, levels = line_order_sonnet5_hartman_trust_comparison))) +
  geom_tile(
    aes(x = 1.5, fill = agreement),
    width = 1.6, height = 0.9, alpha = 0.18, show.legend = FALSE
  ) +
  geom_segment(
    aes(x = 1, xend = 2, yend = factor(line_label, levels = line_order_sonnet5_hartman_trust_comparison),
        color = agreement),
    linewidth = 1.8, alpha = 0.95, lineend = "round", show.legend = FALSE
  ) +
  geom_point(aes(x = 1), color = "#264653", size = 4) +
  geom_text(
    aes(x = 1, label = before_trust_sent), 
    color = "#264653", size = 3.2, vjust = -1.4, fontface = "bold"
  ) +
  geom_point(aes(x = 2), color = "#e9c46a", size = 4) +
  geom_text(
    aes(x = 2, label = after_trust_sent), 
    color = "#b8860b", size = 3.1, vjust = -1.4, fontface = "bold"
  ) +
  geom_text(
    aes(x = 2.4, 
        label = if_else(agreement == "Labels match", "✓ match", "✗ differ"),
        color = agreement),
    size = 3.0, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  scale_color_manual(values = c(
    "Labels match"  = "#2a9d8f",
    "Labels differ" = "#e76f51"
  )) +
  scale_fill_manual(values = c(
    "Labels match"  = "#2a9d8f",
    "Labels differ" = "#e76f51"
  )) +
  scale_x_continuous(
    breaks = c(1, 2), 
    labels = c("Sonnet 5 (without Trust)", "Sonnet 5 (with Trust)"), 
    limits = c(0.5, 3)
  ) +
  labs(
    title = "Sonnet 5 without Trust vs. Sonnet 5 with Trust: Same Lines, One Category Added",
    subtitle = paste0(
      sum(plot_data_sonnet5_hartman_trust_comparison$agreement == "Labels match"), " match, ",
      sum(plot_data_sonnet5_hartman_trust_comparison$agreement == "Labels differ"), 
      " differ (", sum(plot_data_sonnet5_hartman_trust_comparison$agreement == "Labels differ"),
      " of 19 lines change once Trust is added as an option)"
    ),
    caption = "Data: The Impossible Dream, Man of La Mancha (1965)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 10, hjust = 0.5),
    axis.text         = element_text(size = 11, color = "black"),
    panel.grid         = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position   = "none"
  )

plot_lyrics_sonnet5_hartman_trust_comparison


ggsave(
  filename = "plots/05-plot_lyrics_sonnet5_hartman_trust_comparison.png",
  plot = plot_lyrics_sonnet5_hartman_trust_comparison,
  width = 14,
  height = 14,
  dpi = 300
)




