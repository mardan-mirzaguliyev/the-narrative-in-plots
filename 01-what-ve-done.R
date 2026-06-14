# Load necessary packages

library(tidyverse)
library(googlesheets4)
library(tidytext)

# Authenticate Google Sheets file that contains lyrics
gs4_deauth()

# Read Google Sheets file with lyrics
lyrics_id <- "13dM_5vWQGdAeRI-thE6nbzlZIbvbZ8dw9eCu3MsP9Kk"
lyrics <- read_sheet(lyrics_id)

# 1. Tokenize your sanitized dataset

## 1.1 Count total words 
lyrics %>%
  unnest_tokens(output = word, input = text) |> 
  nrow()


lyrics |>
  unnest_tokens(output = word, input = text) |>
  inner_join(get_sentiments("bing"), by = "word") |> 
  count(line, sentiment) |> 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |> 
  (\(df) {
    if (!"positive" %in% names(df)) df$positive <- 0
    if (!"negative" %in% names(df)) df$negative <- 0
    mutate(df, sentiment_score = positive - negative)
  })() |> 
  ggplot(aes(x = line, y = sentiment_score)) +
  geom_col(aes(fill = sentiment_score > 0), show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "#2a9d8f", "FALSE" = "#bd1515")) +
  scale_x_continuous(breaks = seq(min(line_sentiment$line), max(line_sentiment$line), by = 1)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "'What I've Done' ",
    subtitle = "Line-by-line emotional trajectory showing narrative shifts",
    x = "Song Timeline (Line Number)",
    y = "Net Sentiment Score"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.title.x = element_text(color = "black", size = 11),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray30", linewidth = 0.1),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#cbe8f5", color = NA),
    panel.background = element_rect(fill = "#cbe8f5", color = NA)
  )













