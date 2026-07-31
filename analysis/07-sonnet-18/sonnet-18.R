library(tidyverse)
library(googlesheets4)
library(janitor)
library(syuzhet)
library(tidytext)
library(ggrepel)
library(scales)


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
    subtitle = "Syuzhet scores normalized to -1/+1 · Labelled lines are emotional extremes",
    x = "Line",
    y = NULL,
    caption = "William Shakespeare, Sonnet 18 (1590s)"
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


sonnet_18_raw_tokens <- sonnet_18_raw |> 
  unnest_tokens(word, text) |>
  mutate(syuzhet_score = get_sentiment(word, method = "syuzhet"))

sonnet_18_raw_tokens |> 
  filter(syuzhet_score < 0)


sonnet_18_raw_tokens |> 
  filter(syuzhet_score > 0)


extremes_syuzhet


# Gemma 4
sonnet_18_numeric_gemma4 <- sonnet_18_raw |> 
  get_or_run_local(text, output_name = "data/sonnet_18", model = "gemma4:latest",
                   system_prompt = sentiment_numeric_system_prompt,
                   json_schema = numeric_json_schema)

sonnet_18_numeric_gemma4



sonnet_18_numeric_gemma4_test <- sonnet_18_raw |> 
  get_or_run_local(text, output_name = "data/sonnet_18", model = "gemma4:latest",
                   system_prompt = numeric_only_prompt,
                   json_schema = numeric_only_schema)


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

sonnet_18_gemma4_numeric


## Numeric only — no labels at all, "nolabels" in the filename
sonnet_18_gemma4_num <- sonnet_18_raw |> 
  get_or_run_local(text, output_name = "data/sonnet_18", model = "gemma4:latest",
                   system_prompt = numeric_only_system_prompt,
                   json_schema = numeric_only_json_schema())

sonnet_18_gemma4_num


sonnet_18_gemma4_cat_num |> select(line, text, sentiment, numeric_score)
sonnet_18_gemma4_num |> select(line, text, numeric_score)



# Sonnet 5

## Categorical only
sonnet_18_sonnet5_cat <- sonnet_18_raw |> 
  get_or_run_claude_synch(text, output_name = "data/sonnet_18", labels = sentiment_levels)

sonnet_18_sonnet5_cat

## Categorical + numeric
sonnet_18_sonnet5_cat_num <- sonnet_18_raw |> 
  get_or_run_claude_synch(text, output_name = "data/sonnet_18", labels = sentiment_levels,
                          system_prompt = sentiment_numeric_system_prompt)

sonnet_18_sonnet5_cat_num

## Numeric only — no labels at all
sonnet_18_sonnet5_num <- sonnet_18_raw |> 
  get_or_run_claude_synch(text, output_name = "data/sonnet_18",
                          system_prompt = numeric_only_system_prompt)

sonnet_18_sonnet5_num

sonnet_18_sonnet5_cat
sonnet_18_sonnet5_cat_num
sonnet_18_sonnet5_num




