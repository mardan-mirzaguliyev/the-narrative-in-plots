library(tidyverse)
library(googlesheets4)
library(janitor)
library(syuzhet)
library(tidytext)
library(ggrepel)
library(scales)
library(gt)
library(ggalluvial)


gs4_deauth()

sonnet_18_id <- "1wYggI5eGENm4xKYk7u68kVsrI9BroYXqKMMA--PxftI"

sonnet_18_raw <- read_sheet(sonnet_18_id, "Sonnet 18, Modern English")
sonnet_18_raw <- sonnet_18_raw |> 
  clean_names()


sonnet_18_syuzhet <- sonnet_18_raw |> 
  unnest_tokens(word, text) |>
  mutate(syuzhet_score = get_sentiment(word, method = "syuzhet")) |> 
  group_by(line) |> 
  summarize(sentc_syzh_score = mean(syuzhet_score), .groups = "drop") |>   # length-normalized first
  right_join(sonnet_18_raw, by = "line") |> 
  mutate(sentc_syzh_score = replace_na(sentc_syzh_score, 0)) |> 
  relocate(text, .before = sentc_syzh_score) |> 
  arrange(line)

max_abs_lines <- max(abs(sonnet_18_syuzhet$sentc_syzh_score))

sonnet_18_syuzhet <- sonnet_18_syuzhet |> 
  mutate(normalized_syuz_score = sentc_syzh_score / max_abs_lines)   # then rescaled to [-1, 1]

sonnet_18_syuzhet


### 3 most positive sentences table
three_most_positive_syuzhet <- sonnet_18_syuzhet |> 
  slice_max(normalized_syuz_score, n = 3, with_ties = FALSE) |> 
  select(line, text, normalized_syuz_score)
three_most_positive_syuzhet


### 3 most negative sentences table
three_most_negative_syuzhet <- sonnet_18_syuzhet |> 
  slice_min(normalized_syuz_score, n = 3, with_ties = FALSE) |> 
  select(line, text, normalized_syuz_score)
three_most_negative_syuzhet


extremes_syuzhet <- bind_rows(three_most_positive_syuzhet, three_most_negative_syuzhet)
extremes_syuzhet


plot_sonnet_18_syuzhet <- sonnet_18_syuzhet |> 
  ggplot(aes(x = line, y = normalized_syuz_score)) +
  geom_smooth(se = FALSE,
              color = "#2a9d8f",
              linewidth = 1.2,
              method = "loess",
              span = 0.75,
              bg = NA) + # adjust 0.1-0.5 for smoother/rougher arc
  annotate("segment",
           x = 1,
           xend = 14,
           y = 0,
           yend = 0,
           linetype = "dashed",
           color = "grey60",
           linewidth = 0.5) +
  ## All points - small and faint
  geom_jitter(aes(color = normalized_syuz_score > 0),
              size = 0.8,
              alpha = 0.2,
              height = 0.008, 
              width = 0,  
              show.legend = FALSE) +
  # Extreme points larger
  geom_point(data = extremes_syuzhet,
             aes(color = normalized_syuz_score > 0),
             size = 3,
             show.legend = FALSE) +
  # Labels for extreme sentences
  geom_label_repel(
    data = extremes_syuzhet,
    aes(label = str_wrap(text, width = 30)),
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
    breaks = seq(1, 14, by = 1), 
    labels = seq(1, 14, by = 1)
  ) +
  scale_y_continuous(
    limits = c(-1.2, 1.2),
    oob = squish,
    breaks = c(-1.0, -0.5, 0, 0.5, 1.0),
    labels = c("1.0", "-0.5", "0", "0.5", "1.5")
  ) +
  labs(
    title = "Sonnet 18: Line-level Emotional Arc",
    subtitle = "Syuzhet scores normalized to -1/+1: Labelled lines are emotional extremes",
    x = "Line",
    y = NULL,
    caption = "William Shakespeare, Sonnet 18 (1609)"
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

plot_sonnet_18_syuzhet 


ggsave(
  filename = "plots/01-plot_sonnet_18_syuzhet.png",
  plot = plot_sonnet_18_syuzhet,
  width = 15,
  height = 10,
  dpi = 300
)


# Checks 
sonnet_18_raw_tokens <- sonnet_18_raw |> 
  unnest_tokens(word, text) |>
  mutate(syuzhet_score = get_sentiment(word, method = "syuzhet"))

sonnet_18_raw_tokens |> 
  filter(syuzhet_score < 0)


sonnet_18_raw_tokens |> 
  filter(syuzhet_score > 0)

extremes_syuzhet


# Gemma 4
## Categorical only — labels required, prompt/schema auto-generated
sonnet_18_gemma4_cat <- sonnet_18_raw |> 
  get_or_run_local(text, output_name = "data/sonnet_18", model = "gemma4:latest",
                   labels = sentiment_levels)

sonnet_18_gemma4_cat


## Categorical + numeric — labels required (schema needs the enum),
## explicit prompt/schema supplied
sonnet_18_gemma4_cat_num <- sonnet_18_raw |> 
  get_or_run_local(text, output_name = "data/sonnet_18", model = "gemma4:latest",
                   labels = sentiment_levels,
                   system_prompt = get_sentiment_numeric_system_prompt(sentiment_levels),
                   json_schema = categorical_numeric_json_schema(sentiment_levels))

sonnet_18_gemma4_cat_num


## Numeric only — no labels at all, "nolabels" in the filename
sonnet_18_gemma4_num <- sonnet_18_raw |> 
  get_or_run_local(text, output_name = "data/sonnet_18", model = "gemma4:latest",
                   system_prompt = numeric_only_system_prompt,
                   json_schema = numeric_only_json_schema())

sonnet_18_gemma4_num


sonnet_18_gemma4_comparison <- sonnet_18_gemma4_cat |> 
  rename(only_cat_reasoning = reasoning,
         only_cat_sentiment = sentiment,
         only_cat_conf_score = confidence_score
  ) |> 
  left_join(sonnet_18_gemma4_cat_num |> 
              select(-text) |> 
              rename(cat_num_reasoning = reasoning,
                     cat_num_sentiment = sentiment,
                     cat_num_score = numeric_score,      # <- added
                     cat_num_conf_score = confidence_score),
            by = "line") |> 
  left_join(sonnet_18_gemma4_num |> 
              select(-text) |> 
              rename(only_num_score = numeric_score,
                     only_num_conf = confidence_score,
                     only_num_reasoning = reasoning),
            by = "line"
  )


# Hypothesis test
n_flipped <- sum(sign(sonnet_18_gemma4_comparison$cat_num_score) != sign(sonnet_18_gemma4_comparison $only_num_score))
n_total   <- nrow(sonnet_18_gemma4_comparison)

binom.test(n_flipped, n_total, p = 0.05, alternative = "greater")


plot_data_gemma4 <- sonnet_18_gemma4_comparison |> 
  mutate(
    score_diff = cat_num_score - only_num_score,
    move_direction = case_when(
      score_diff > 0.02  ~ "Score decreased",
      score_diff < -0.02 ~ "Score increased",
      TRUE ~ "No meaningful change"
    ),
    move_color = case_when(
      move_direction == "Score decreased" ~ "#e76f51",
      move_direction == "Score increased" ~ "#2a9d8f",
      TRUE ~ "#adb5bd"
    ),
    score_gap = abs(score_diff),
    line_label = paste0("Line ", line)
  )

flip_arrows_gemma4 <- plot_data_gemma4 |> 
  filter(move_direction != "No meaningful change") |> 
  mutate(
    arrow_x = (cat_num_score + only_num_score) / 2,
    arrow_label = if_else(move_direction == "Score decreased", "\u2190", "\u2192")
  )

line_order_gemma4 <- paste0("Line ", sort(unique(sonnet_18_gemma4_comparison$line), decreasing = TRUE))


plot_gemma4_score_comparison <- plot_data_gemma4 |> 
  ggplot(aes(y = line_label)) +
  geom_vline(xintercept = 0, color = "#264653", linewidth = 0.9) +
  
  geom_segment(
    data = plot_data_gemma4 |> filter(move_direction != "No meaningful change"),
    aes(x = cat_num_score, xend = only_num_score, yend = line_label,
        color = move_color),
    linewidth = 1.2,   # fixed, matches Sonnet 5's chart
    alpha = 0.85, lineend = "round", show.legend = FALSE
  ) +
  
  geom_text(
    data = flip_arrows_gemma4,
    aes(x = arrow_x, y = line_label, label = arrow_label, color = move_color),
    size = 5, fontface = "bold", vjust = -1.6, show.legend = FALSE
  ) +
  
  geom_point(
    data = plot_data_gemma4 |> filter(move_direction != "No meaningful change"),
    aes(x = cat_num_score), color = "#264653", size = 3.5
  ) +
  geom_point(
    data = plot_data_gemma4 |> filter(move_direction != "No meaningful change"),
    aes(x = only_num_score), color = "#e9c46a", size = 3.5
  ) +
  geom_text(
    data = plot_data_gemma4 |> filter(move_direction != "No meaningful change"),
    aes(x = cat_num_score, label = round(cat_num_score, 2)), 
    color = "#264653", size = 3, vjust = -1.3, fontface = "bold"
  ) +
  geom_text(
    data = plot_data_gemma4 |> filter(move_direction != "No meaningful change"),
    aes(x = only_num_score, label = round(only_num_score, 2)), 
    color = "#b8860b", size = 3, vjust = -1.3, fontface = "bold"
  ) +
  
  geom_point(
    data = plot_data_gemma4 |> filter(move_direction == "No meaningful change"),
    aes(x = cat_num_score), color = "#264653", size = 3.5
  ) +
  geom_text(
    data = plot_data_gemma4 |> filter(move_direction == "No meaningful change"),
    aes(x = cat_num_score, label = round(cat_num_score, 2)), 
    color = "#264653", size = 3, vjust = -1.3, fontface = "bold"
  ) +
  
  geom_point(
    data = tibble(
      x = c(0, 0), 
      y = c(NA_character_, NA_character_),
      legend_label = c("Score decreased", "Score increased"),
      legend_color = c("#e76f51", "#2a9d8f")
    ),
    aes(x = x, y = y, color = legend_color),
    shape = 15, size = 4, na.rm = TRUE
  ) +
  
  scale_color_identity(
    name = NULL,
    breaks = c("#e76f51", "#2a9d8f"),
    labels = c("Score decreased", "Score increased"),
    guide = "legend"
  ) +
  scale_x_continuous(breaks = pretty_breaks(n = 8)) +
  scale_y_discrete(limits = line_order_gemma4) +
  labs(
    title = "Joint vs. Independent Numeric Scoring: Gemma 4",
    subtitle = "4 of 14 lines flip sign (binomial test, p = 0.004); arrow shows whether the score increased or decreased when scored independently vs. with a category attached",
    caption = "Data: Sonnet 18, William Shakespeare (1609)",
    x = "Numeric Score",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 10.5, hjust = 0.5),
    axis.text         = element_text(size = 11, color = "black"),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position   = "bottom",
    legend.text        = element_text(size = 9)
  )

plot_gemma4_score_comparison


ggsave(
  filename = "plots/02-plot_gemma4_score_comparison.png",
  plot = plot_gemma4_score_comparison,
  width = 15,
  height = 10,
  dpi = 300
)


## Flipped lines table
sonnet_18_flipped_lines_gemma4 <- sonnet_18_gemma4_comparison |> 
  filter(line %in% c(11, 5, 8, 13)) |> 
  select(line, text, only_cat_sentiment, only_cat_conf_score, only_cat_reasoning, 
         cat_num_sentiment, cat_num_score, cat_num_conf_score, cat_num_reasoning, only_num_score, only_num_reasoning) |> 
  arrange(match(line, c(11, 5, 8, 13)))   # keep the "sharpest flip first" ordering from the analysis


sonnet_18_flipped_lines_gemma4 |> 
  gt() |> 
  cols_label(
    line = "Line Num",
    text = "Line",
    only_cat_sentiment = "Label",
    only_cat_conf_score = "Conf.",
    only_cat_reasoning = "Reasoning",
    cat_num_sentiment = "Label",
    cat_num_score = "Score",
    cat_num_conf_score = "Conf.",
    cat_num_reasoning = "Reasoning",
    only_num_score = "Score",
    only_num_reasoning = "Reasoning"
  ) |> 
  tab_spanner(label = "Categorical Only", columns = c(only_cat_sentiment, only_cat_conf_score, only_cat_reasoning)) |> 
  tab_spanner(label = "Categorical + Numeric", columns = c(cat_num_sentiment, cat_num_score, cat_num_conf_score, cat_num_reasoning)) |> 
  tab_spanner(label = "Numeric Only", columns = c(only_num_score, only_num_reasoning)) |> 
  tab_header(
    title = "Where Attaching a Label Flips the Score (Gemma 4)",
    subtitle = "The 4 of 14 lines where joint scoring produced the opposite sign from independent scoring"
  ) |> 
  tab_caption("Data: Sonnet 18, William Shakespeare (1609)") |> 
  # Header styling
  tab_style(
    style = list(cell_fill(color = "#2a9d8f"), cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = list(cell_fill(color = "#264653"), cell_text(color = "white", weight = "bold")),
    locations = cells_column_spanners()
  ) |> 
  # Base body styling
  tab_style(
    style = cell_fill(color = "#cbe8f5"),
    locations = cells_body()
  ) |> 
  # Flag the two score columns that flip sign against each other
  tab_style(
    style = list(cell_fill(color = "#f4a261"), cell_text(weight = "bold")),
    locations = cells_body(columns = c(cat_num_score, only_num_score))
  ) |> 
  cols_align(align = "left", columns = c(text, only_cat_reasoning, cat_num_reasoning)) |> 
  cols_align(align = "center", columns = c(only_cat_sentiment, only_cat_conf_score, 
                                           cat_num_sentiment, cat_num_score, cat_num_conf_score, only_num_score)) |> 
  cols_width(
    text ~ px(220),
    only_cat_sentiment ~ px(90),
    only_cat_conf_score ~ px(70),
    only_cat_reasoning ~ px(240),
    cat_num_sentiment ~ px(90),
    cat_num_score ~ px(70), 
    cat_num_conf_score ~ px(70),
    cat_num_reasoning ~ px(240),
    only_num_score ~ px(90),
    only_num_reasoning ~ px(240)
  ) |> 
  gtsave("tables/01-sonnet_18_gemma4_flipped_lines.png")


## Alluvial
plot_gemma4_cat_shift_alluvial <- sonnet_18_gemma4_comparison |> 
  arrange(line) |> 
  mutate(
    only_cat_sentiment = factor(only_cat_sentiment, levels = sentiment_levels),
    cat_num_sentiment  = factor(cat_num_sentiment, levels = sentiment_levels),
    sentiment_changed = if_else(
      as.character(only_cat_sentiment) == as.character(cat_num_sentiment),
      "Sentiment not changed",
      "Sentiment changed"
    )
  ) |> 
  ggplot(aes(axis1 = only_cat_sentiment, axis2 = cat_num_sentiment)) +
  geom_alluvium(aes(fill = sentiment_changed), width = 0.25, alpha = 0.85, color = "white", linewidth = 0.2,
                discern = FALSE) +
  geom_stratum(width = 0.25, fill = "white", color = "#264653", linewidth = 0.6, discern = FALSE) +
  
  # Plain geom_text at the true stratum centroid — no repel, no drift risk
  geom_text(
    stat = "stratum", aes(label = after_stat(stratum)),
    size = 3.5, fontface = "bold", color = "#264653"
  ) +
  
  geom_text_repel(
    stat = "alluvium", aes(label = line),
    size = 2.2, color = "black",
    direction = "y", nudge_x = 0.2, segment.size = 0.15, segment.color = "grey50",
    min.segment.length = 0, box.padding = 0.15, max.overlaps = Inf,
    max.iter = 20000, force = 1.5
  ) +
  
  scale_x_discrete(limits = c("Categorical Only", "Categorical + Numeric"), expand = c(0.2, 0.2)) +
  scale_fill_manual(values = c(
    "Sentiment not changed" = "#2a9d8f",
    "Sentiment changed"     = "#e76f51"
  ), name = NULL) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Does Adding a Numeric Score Gemma 4's Category Choice?",
    subtitle = "Each line's label from categorical-only prompting to categorical+numeric prompting",
    caption = "Data: Sonnet 18, William Shakespeare (1609)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text.x       = element_text(size = 13, face = "bold", color = "black"),
    axis.text.y       = element_blank(),
    panel.grid         = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position   = "bottom",
    plot.margin        = margin(t = 20, r = 60, b = 20, l = 60)
  )

plot_gemma4_cat_shift_alluvial


ggsave(
  filename = "plots/04-plot_gemma4_cat_shift_alluvial.png",
  plot = plot_gemma4_cat_shift_alluvial,
  width = 12,
  height = 12,
  dpi = 300
)



# Claude Sonnet 5
## Categorical only
sonnet_18_sonnet5_cat <- sonnet_18_raw |> 
  get_or_run_claude_synch(text, 
                          output_name = "data/sonnet_18", 
                          model = "claude-sonnet-5",
                          labels = sentiment_levels)

sonnet_18_sonnet5_cat


## Categorical + numeric
sonnet_18_sonnet5_cat_num <- sonnet_18_raw |> 
  get_or_run_claude_synch(text, output_name = "data/sonnet_18",
                          model = "claude-sonnet-5",
                          labels = sentiment_levels, 
                          system_prompt = sentiment_numeric_system_prompt)

sonnet_18_sonnet5_cat_num

## Numeric only — no labels at all
sonnet_18_sonnet5_num <- sonnet_18_raw |> 
  get_or_run_claude_synch(text, output_name = "data/sonnet_18",
                          system_prompt = numeric_only_system_prompt)

sonnet_18_sonnet5_num


sonnet_18_sonnet5_comparison <- sonnet_18_sonnet5_cat |> 
  rename(only_cat_reasoning = reasoning,
         only_cat_sentiment = sentiment,
         only_cat_conf_score = confidence_score
  ) |> 
  left_join(sonnet_18_sonnet5_cat_num |> 
              select(-text) |> 
              rename(cat_num_reasoning = reasoning,
                     cat_num_sentiment = sentiment,
                     cat_num_score = numeric_score,      # <- added
                     cat_num_conf_score = confidence_score),
            by = "line") |> 
  left_join(sonnet_18_sonnet5_num |> 
              select(-text) |> 
              rename(only_num_score = numeric_score,
                     only_num_conf = confidence_score,
                     only_num_reasoning = reasoning),
            by = "line"
  )


# Prepare data 

plot_data_sonnet5 <- sonnet_18_sonnet5_comparison |> 
  mutate(
    score_diff = cat_num_score - only_num_score,
    move_direction = case_when(
      score_diff > 0.02  ~ "Score decreased",
      score_diff < -0.02 ~ "Score increased",
      TRUE ~ "No meaningful change"
    ),
    move_color = case_when(
      move_direction == "Score decreased" ~ "#e76f51",
      move_direction == "Score increased" ~ "#2a9d8f",
      TRUE ~ "#adb5bd"
    ),
    score_gap = abs(score_diff),
    line_label = paste0("Line ", line)
  )

move_arrows_sonnet5 <- plot_data_sonnet5 |> 
  filter(move_direction != "No meaningful change") |> 
  mutate(
    arrow_x = (cat_num_score + only_num_score) / 2,
    arrow_label = if_else(move_direction == "Score decreased", "\u2190", "\u2192")
  )

line_order_sonnet5 <- paste0("Line ", sort(unique(sonnet_18_sonnet5_comparison$line), decreasing = TRUE))


plot_sonnet5_score_comparison <- plot_data_sonnet5 |> 
  ggplot(aes(y = line_label)) +
  geom_vline(xintercept = 0, color = "#264653", linewidth = 0.9) +
  
  geom_segment(
    data = plot_data_sonnet5 |> filter(move_direction != "No meaningful change"),
    aes(x = cat_num_score, xend = only_num_score, yend = line_label,
        color = move_color),
    linewidth = 1.2,
    alpha = 0.85, lineend = "round", show.legend = FALSE
  ) +
  geom_text(
    data = move_arrows_sonnet5,
    aes(x = arrow_x, y = line_label, label = arrow_label, color = move_color),
    size = 5, fontface = "bold", vjust = -1.6, show.legend = FALSE
  ) +
  
  geom_point(
    data = plot_data_sonnet5 |> filter(move_direction != "No meaningful change"),
    aes(x = cat_num_score), color = "#264653", size = 3.5
  ) +
  geom_point(
    data = plot_data_sonnet5 |> filter(move_direction != "No meaningful change"),
    aes(x = only_num_score), color = "#e9c46a", size = 3.5
  ) +
  geom_text(
    data = plot_data_sonnet5 |> filter(move_direction != "No meaningful change"),
    aes(x = cat_num_score, label = round(cat_num_score, 2)), 
    color = "#264653", size = 3, vjust = -1.3, fontface = "bold"
  ) +
  geom_text(
    data = plot_data_sonnet5 |> filter(move_direction != "No meaningful change"),
    aes(x = only_num_score, label = round(only_num_score, 2)), 
    color = "#b8860b", size = 3, vjust = -1.3, fontface = "bold"
  ) +
  
  geom_point(
    data = plot_data_sonnet5 |> filter(move_direction == "No meaningful change"),
    aes(x = cat_num_score), color = "#264653", size = 3.5
  ) +
  geom_text(
    data = plot_data_sonnet5 |> filter(move_direction == "No meaningful change"),
    aes(x = cat_num_score, label = round(cat_num_score, 2)), 
    color = "#264653", size = 3, vjust = -1.3, fontface = "bold"
  ) +
  
  geom_point(
    data = tibble(
      x = c(0, 0), 
      y = c(NA_character_, NA_character_),
      legend_label = c("Score decreased", "Score increased"),
      legend_color = c("#e76f51", "#2a9d8f")
    ),
    aes(x = x, y = y, color = legend_color),
    shape = 15, size = 4, na.rm = TRUE
  ) +
  
  scale_color_identity(
    name = NULL,
    breaks = c("#e76f51", "#2a9d8f"),
    labels = c("Score decreased", "Score increased"),
    guide = "legend"
  ) +
  scale_x_continuous(breaks = pretty_breaks(n = 8)) +
  scale_y_discrete(limits = line_order_sonnet5) +
  labs(
    title = "Joint vs. Independent Numeric Scoring: Claude Sonnet 5",
    subtitle = "No sign flips; arrow shows whether the score increased or decreased when scored independently vs. with a category attached",
    caption = "Data: Sonnet 18, William Shakespeare (1609)",
    x = "Numeric Score",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 10.5, hjust = 0.5),
    axis.text         = element_text(size = 11, color = "black"),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position   = "bottom",
    legend.text        = element_text(size = 9)
  )

plot_sonnet5_score_comparison


ggsave(
  filename = "plots/03-plot_sonnet5_score_comparison.png",
  plot = plot_sonnet5_score_comparison,
  width = 15,
  height = 10,
  dpi = 300
)



## Alluvial
plot_sonnet5_cat_shift_alluvial <- sonnet_18_sonnet5_comparison |> 
  arrange(line) |> 
  mutate(
    only_cat_sentiment = factor(only_cat_sentiment, levels = sentiment_levels),
    cat_num_sentiment  = factor(cat_num_sentiment, levels = sentiment_levels),
    sentiment_changed = if_else(
      as.character(only_cat_sentiment) == as.character(cat_num_sentiment),
      "Sentiment not changed",
      "Sentiment changed"
    )
  ) |> 
  ggplot(aes(axis1 = only_cat_sentiment, axis2 = cat_num_sentiment)) +
  geom_alluvium(aes(fill = sentiment_changed), width = 0.25, alpha = 0.85, color = "white", linewidth = 0.2,
                discern = FALSE) +
  geom_stratum(width = 0.25, fill = "white", color = "#264653", linewidth = 0.6, discern = FALSE) +
  
  # Plain geom_text at the true stratum centroid — no repel, no drift risk
  geom_text(
    stat = "stratum", aes(label = after_stat(stratum)),
    size = 3.5, fontface = "bold", color = "#264653"
  ) +
  
  geom_text_repel(
    stat = "alluvium", aes(label = line),
    size = 2.2, color = "black",
    direction = "y", nudge_x = 0.2, segment.size = 0.15, segment.color = "grey50",
    min.segment.length = 0, box.padding = 0.15, max.overlaps = Inf,
    max.iter = 20000, force = 1.5
  ) +
  
  scale_x_discrete(limits = c("Categorical Only", "Categorical + Numeric"), expand = c(0.2, 0.2)) +
  scale_fill_manual(values = c(
    "Sentiment not changed" = "#2a9d8f",
    "Sentiment changed"     = "#e76f51"
  ), name = NULL) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Does Adding a Numeric Score Change Sonnet 5's Category Choice?",
    subtitle = "Each line's label from categorical-only prompting to categorical+numeric prompting",
    caption = "Data: Sonnet 18, William Shakespeare (1609)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text.x       = element_text(size = 13, face = "bold", color = "black"),
    axis.text.y       = element_blank(),
    panel.grid         = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position   = "bottom",
    plot.margin        = margin(t = 20, r = 60, b = 20, l = 60)
  )

plot_sonnet5_cat_shift_alluvial


ggsave(
  filename = "plots/05-sonnet5_cat_shift_alluvial.png",
  plot = plot_sonnet5_cat_shift_alluvial,
  width = 12,
  height = 12,
  dpi = 300
)



