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
tokenized_lyrics <- lyrics %>%
  unnest_tokens(output = word, input = text)

## 1.1 Count total words 
total_words <- nrow(tokenized_lyrics)
total_words

## 1.2 Sentiment analysis of line by line
### 1.2.1 First two lines

lyrics |> slice_head(n = 2) |> 
    unnest_tokens(output = word, input = text) |>
    inner_join(get_sentiments("bing"), by = "word")

### 1.2.3 Line 3 and Line 4

lyrics |> slice(2:4) |>
  unnest_tokens(output = word, input = text) |>
  inner_join(get_sentiments("bing"), by = "word") |> 
  count(line, sentiment) |> 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(line = as.integer(line)) |> 
  (\(df) mutate(df, positive = ifelse("positive" %in% names(df), positive <- positive, 0)))() |> 
  (\(df) mutate(df, negative = ifelse("negative" %in% names(df), negative <- negative, 0)))() |> 
  mutate(sentiment_score = positive - negative) |> 
  ggplot(aes(x = line, y = sentiment_score)) +
  geom_col(aes(fill = sentiment_score > 0), show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "#2a9d8f", "FALSE" = "#bd1515")) + 
  theme_minimal(base_size = 14) +
  labs(
    title = "'What I've Done' ",
    subtitle = "Line-by-line emotional trajectory showing narrative shifts",
    x = "Song Timeline (Line Number)",
    y = "Net Sentiment Score"
  )








## 1.3 Sentiment analysis of full text
word_sentiments <- tokenized_lyrics |> 
  inner_join(get_sentiments("bing"), by = "word")
word_sentiments

### 1.3.1 Calculate a net sentiment score per line of the song
line_sentiment_raw <- word_sentiments %>%
  count(line, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)

if (!"positive" %in% names(line_sentiment_raw)) line_sentiment_raw$positive <- 0
if (!"negative" %in% names(line_sentiment_raw)) line_sentiment_raw$negative <- 0


# 4. Final calculation of the net score

line_sentiment_raw
line_sentiment <- line_sentiment_raw %>%
  mutate(sentiment_score = positive - negative)

line_sentiment


ggplot(line_sentiment, aes(x = line, y = sentiment_score)) +
  geom_col(aes(fill = sentiment_score > 0), show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "#2a9d8f", "FALSE" = "#bd1515")) + 
  theme_minimal(base_size = 14) +
  labs(
    title = "The Rolling Sentiment of 'What I've Done'",
    subtitle = "Line-by-line emotional trajectory showing narrative shifts",
    x = "Song Timeline (Line Number)",
    y = "Net Sentiment Score"
  )







