library(dplyr)          
library(tidyr)          
library(ggplot2)        
library(forcats)
library(textdata)       
library(stringr)        
library(tibble)         
library(scales)         
library(googlesheets4)  
library(tidytext)       
library(syuzhet)        
library(ggrepel)
library(gt)
library(reticulate)
library(purrr)
library(httr2)   
library(jsonlite)
library(ollamar)
library(patchwork)
library(writexl)
library(here)
library(dotenv)


load_dot_env(file = here(".env"))

# DATA PREPARATION

# Changes, 2Pac syuzhet pipeline

gs4_deauth()

## Read individual sheets of lyrics
lyrics_id <- Sys.getenv("CHANGES_ID")


# Changes, 2Pac
## 1.1 Lexicon based sentiments
changes_2pac_raw <- read_sheet(lyrics_id, "Changes 2Pac")


## Syuzhet scoring for 2Pac, Changes 
changes_2pac_syuzhet <- changes_2pac_raw |>
  mutate(syuzhet_score = get_sentiment(text, method = "syuzhet"))
changes_2pac_syuzhet 

# Normalise to –1 / +1
# syuzhet scores are unbounded, so we divide by the observed maximum
# to get a scale anchored between –1 and +1
max_abs <- max(abs(changes_2pac_syuzhet$syuzhet_score))
max_abs

changes_2pac_syuzhet <- changes_2pac_syuzhet |> 
  mutate(normalized_score = syuzhet_score / max_abs)
changes_2pac_syuzhet


section_order_2pac <- c("Verse 1", "Chorus 1", "Verse 2", "Chorus 2", "Interlude", "Verse 3", "Chorus 3")


## Most negative and most positive scores
extremes <- 
  changes_2pac_syuzhet |> 
  group_by(section) |> 
  summarize(
    section_min = min(normalized_score),
    section_max = max(normalized_score))

## Build a coloring logic for the points
extremes_plot <- extremes|> 
  filter(section_min != 0) |> 
  mutate(min_color = ifelse(section_min < 0, "#bd1515", "#2a9d8f"))


plot_2pac_scores_syuzhet <- changes_2pac_syuzhet |>
  mutate(section = factor(section, levels = section_order_2pac)) |>
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
    data = extremes_plot,
    aes(x = section, 
        y = section_min,
        label = round(section_min, 2)),
    color = extremes_plot$min_color,
    fontface = "bold",
    size     = 3.5,
    fill     = "white",
    box.padding = 0.5,
    point.padding = 0.5,
    segment.color = "grey50"
  ) +
  scale_y_continuous(
    limits = c(-1.2, 1.2),
    oob = squish,
    breaks = c(-1.0, -0.5, 0, 0.5, 1.0),
    labels = c("-1.0", "-0.5", "0", "0.5", "1") # Changed "Neutral" to "0"
  ) +
  labs(
    title    = "Sentiment distributions",
    subtitle = "Lexicon method: syuzhet package",
    x        = NULL,
    y        = NULL,
    caption  = "Data: Changes, 2Pac lyrics (1998)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_2pac_scores_syuzhet


ggsave(
  filename = "plots/01-plot_2pac_scores_syuzhet.png",
  plot = plot_2pac_scores_syuzhet,
  width = 15,
  height = 10,
  dpi = 300
)


changes_2pac_syuzhet |> 
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
  gtsave("tables/01-table_chorus_3_scored_lines_syuzhet.png")



## 1.2 Local LLMs

### llama3.2:latest
changes_2pac_llama3_2_latest <- changes_2pac_raw |>
  get_or_run_local(text, 
                   output_name = "data/changes_2pac_local", 
                   model = "llama3.2:latest",
                   labels = default_sentiment_labels)

changes_2pac_llama3_2_latest


plot_changes_2pac_llama3_2_latest <- changes_2pac_llama3_2_latest |> 
  mutate(section = factor(section, levels = section_order_2pac)) |> 
  group_by(section, sentiment) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  group_by(section) |>
  mutate(share = count / sum(count)) |> 
  ungroup() |> 
  ggplot(aes(x = section, y = share, fill = sentiment)) +
  geom_bar(stat = "identity", position = "fill", show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      sentiment, 
      "\nShare: ", round(share * 100, 0), "%",
      "\nConfidence: ", round(avg_conf, 2), ""
    )), 
    position = position_fill(vjust = 0.5), 
    color = "white", 
    size = 3,
    lineheight = 0.8 
  ) +
  labs(title = "Emotional Distribution",
       subtitle = "Model: llama3.2:latest",
       x = NULL,
       y = NULL,
       caption = "Data: Changes, 2Pac lyrics (1998)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_changes_2pac_llama3_2_latest


ggsave(
  filename = "plots/02-plot_changes_2pac_llama3_2_latest.png",
  plot = plot_changes_2pac_llama3_2_latest,
  width = 15,
  height = 10,
  dpi = 300
)


### phi4-mini:latest
phi_4_mini_latest = "phi4-mini:latest"

changes_2pac_phi4_mini_latest <- changes_2pac_raw |>
  get_or_run_local(text, 
                   output_name = "data/changes_2pac_local", 
                   model = phi_4_mini_latest,
                   labels = default_sentiment_labels)

changes_2pac_phi4_mini_latest


plot_changes_2pac_phi4_mini_latest <- changes_2pac_phi4_mini_latest |> 
  mutate(section = factor(section, levels = section_order_2pac)) |> 
  group_by(section, sentiment) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  group_by(section) |>
  mutate(share = count / sum(count)) |> 
  ungroup() |> 
  ggplot(aes(x = section, y = share, fill = sentiment)) +
  geom_bar(stat = "identity", position = "fill", show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      sentiment, 
      "\nShare: ", round(share * 100, 0), "%",
      "\nConfidence: ", round(avg_conf, 2), ""
    )), 
    position = position_fill(vjust = 0.5), 
    color = "white", 
    size = 3,
    lineheight = 0.8 
  ) +
  labs(title = "Emotional Distribution",
       subtitle = "Model: phi4-mini:latest",
       x = NULL,
       y = NULL,
       caption = "Data: Changes, 2Pac lyrics (1998)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_changes_2pac_phi4_mini_latest


ggsave(
  filename = "plots/03-plot_changes_2pac_phi4_latest.png",
  plot = plot_changes_2pac_phi4_mini_latest,
  width = 15,
  height = 10,
  dpi = 300
)


## 1.3 Hugging Face
# cardiffnlp/twitter-roberta-base-sentiment-latest

changes_2pac_roberta <- changes_2pac_raw |> 
  get_or_run_hf(text,
                output_name = "data/changes_2pac")

changes_2pac_roberta


plot_changes_2pac_roberta <- changes_2pac_roberta |>
  mutate(section = factor(section, levels = section_order_2pac)) |> 
  group_by(section, sentiment) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  group_by(section) |>
  mutate(share = count / sum(count)) |> 
  ungroup() |> 
  ggplot(aes(x = section, y = share, fill = sentiment)) +
  geom_bar(stat = "identity", position = "fill", show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      sentiment, 
      "\nShare: ", round(share * 100, 0), "%",
      "\nConfidence: ", round(avg_conf, 2), ""
    )), 
    position = position_fill(vjust = 0.5), 
    color = "white", 
    size = 3,
    lineheight = 0.8 
  ) +
  labs(title = "Emotional Distribution",
       subtitle = "Model: Twitter-roBERTa",
       x = NULL,
       y = NULL,
       caption = "Data: Changes, 2Pac lyrics (1998)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_changes_2pac_roberta


ggsave(
  filename = "plots/04-plot_changes_2pac_roberta.png",
  plot = plot_changes_2pac_roberta,
  width = 15,
  height = 10,
  dpi = 300
)



## 1.4 Claude API
changes_2pac_sonnet_5 <- changes_2pac_raw |>
  get_or_run_claude_batch(text,
                          output_name = "data/changes_2pac",
                          id_col = line,
                          model = "claude-sonnet-5",
                          labels = default_sentiment_labels)
changes_2pac_sonnet_5


plot_changes_2pac_sonnet_5 <- changes_2pac_sonnet_5 |> 
  mutate(section = factor(section, levels = section_order_2pac)) |> 
  group_by(section, sentiment) |> 
  summarize(count = n(),
            avg_conf = mean(confidence_score, na.rm = TRUE),
            .groups = "drop") |> 
  group_by(section) |>
  mutate(share = count / sum(count)) |> 
  ungroup() |> 
  ggplot(aes(x = section, y = share, fill = sentiment)) +
  geom_bar(stat = "identity", position = "fill", show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      sentiment, 
      "\nShare: ", round(share * 100, 0), "%",
      "\nConfidence: ", round(avg_conf, 2), ""
    )), 
    position = position_fill(vjust = 0.5), 
    color = "white", 
    size = 3,
    lineheight = 0.8 
  ) +
  labs(title = "Emotional Distribution",
       subtitle = "Model: Claude Sonnet 5",
       x = NULL,
       y = NULL,
       caption = "Data: Changes, 2Pac lyrics (1998)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

plot_changes_2pac_sonnet_5


ggsave(
  filename = "plots/05-plot_changes_2pac_sonnet_5.png",
  plot = plot_changes_2pac_sonnet_5,
  width = 15,
  height = 10,
  dpi = 300
)



