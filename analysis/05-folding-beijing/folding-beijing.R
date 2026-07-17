# DATA PREPARATION

# Load necessary packages

library(stringr)
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
library(patchwork)
library(writexl)
library(RColorBrewer)


story_path <- "data/beijing.pdf"
story_raw <- pdf_text(story_path)


beijing_full_text <- paste(story_raw, collapse =" ") |> 
  str_squish()

beijing_full_text

# Anchor the split to require a space (or string start) before the digit,
# so we only catch "N. " as a standalone token, not embedded digits/decimals
sections_list <- str_split(beijing_full_text, "(?<=^|\\s)(?=\\d{1,2}\\.\\s)")[[1]]
sections_list

sections_list <- sections_list[nzchar(str_trim(sections_list))]
sections_list

stopifnot(length(sections_list) == 5)

beijing_tbl <- tibble(
  section_id = 1:5,
  text = str_remove(sections_list, "^\\d+\\.\\s*")
)

beijing_tbl


### Lexicon Based Analysis=
nrc <- get_sentiments("nrc")

beijing_nrc <- beijing_sentences |> 
  unnest_tokens(word, sentence) |> 
  left_join(nrc, by = "word", relationship = "many-to-many") |> 
  filter(!is.na(sentiment)) |> 
  filter(!sentiment %in% c("positive", "negative")) |> 
  count(section_id, sentence_id, sentiment) |> 
  slice_max(n, by = c(section_id, sentence_id), with_ties = FALSE) |> 
  select(section_id, sentence_id, sentiment) |> 
  right_join(
    beijing_sentences |> select(section_id, sentence_id, sentence),
    by = c("section_id", "sentence_id")
  ) |> 
  mutate(sentiment = replace_na(sentiment, "neutral")) |> 
  select(section_id, sentence_id, sentence, sentiment)

beijing_nrc


plot_emotion_profile_nrc <- beijing_nrc |> 
  group_by(section_id, sentiment) |> 
  summarize(n = n(), .groups = "drop") |> 
  group_by(section_id) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  select(-n) |>
  mutate(
    sentiment = fct_relevel(sentiment,
                            "joy", "trust", "anticipation", "surprise",
                            "anger", "disgust", "fear", "sadness", "neutral")) |> 
  ggplot(aes(x = section_id, y = sentiment, fill = prop)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = percent(prop, accuracy = 0.1)),
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  scale_x_continuous(
    breaks = seq(1, 5, by = 1),
    limits = c(0.5, 5.5)
  ) +
  scale_fill_gradient2(
    low = "#f7f7f7",
    mid = "#a8d8d2",
    high = "#2a9d8f",
    midpoint = 0.08,
    labels = percent
  ) +
  labs(
    title = "Emotion Profile Across 5 Sections",
    subtitle = "NRC lexicon — proportion of sentences by dominant emotion",,
    caption = "Data: Folding Beijing, Hao Jingfang (2015)",
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

plot_emotion_profile_nrc


ggsave(
  filename = "plots/01-plot_emotion_profile_nrc.png",
  plot = plot_emotion_profile_nrc,
  width = 15,
  height = 10,
  dpi = 300
)



plot_emotional_breakdown_nrc <- beijing_nrc |> 
  group_by(sentiment) |> 
  summarize(n = n()) |> 
  arrange(desc(n)) |> 
  ggplot(aes(n, fct_reorder(sentiment, n))) +
  # Using a distinct palette for clearer visual separation
  geom_col(aes(fill = sentiment), show.legend = FALSE) +
  scale_fill_manual(values = c("neutral" = "#adb5bd", "trust" = "#2a9d8f", 
                               "anticipation" = "#e9c46a", "fear" = "#264653", 
                               "joy" = "#f4a261", "anger" = "#e76f51",
                               "sadness" = "#6d6875", "surprise" = "#84a59d",
                               "disgust" = "#bc6c25")) +
  scale_x_continuous(limits = c(0, 600), breaks = seq(0, 600, 50)) +
  geom_label(aes(label = n),
             hjust = -0.2,
             colour = "black", 
             fontface = "bold",
             size = 3.5,
             fill = "white",
             linewidth = 0.5) +
  labs(title = "Neutral Dominates, but Anticipation and Trust Lead the Story's Emotional Register",
       subtitle = "Story-level sentiment breakdown using NRC lexicon",
       caption = "Data: Folding Beijing, Hao Jingfang (2015)",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_line(color = "#cbe8f5", linewidth = 0.2),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "#cbe8f5", color = NA),
        panel.background = element_rect(fill = "#cbe8f5", color = NA))

plot_emotional_breakdown_nrc


ggsave(
  filename = "plots/02-plot_emotional_breakdown_nrc.png",
  plot = plot_emotional_breakdown_nrc,
  width = 15,
  height = 10,
  dpi = 300
)


### LLM based analysis
### llama3.2:latest
beijing_llama3_2 <- beijing_sentences |>
  mutate(sentiment_data = map(sentence, ~score_local_llm(.x))) |>
  unnest_wider(sentiment_data)

beijing_llama3_2

beijing_llama3_2 |> saveRDS("beijing_llama3_2.rds")



## 5 most positive sentences table
five_most_positive_llama3_2 <- beijing_llama3_2 |> 
  slice_max(normalized_score, n = 5, with_ties = FALSE) |> 
  select(section_id, sentence_id, sentence, normalized_score)
five_most_positive_syuzhet


### 5 most negative sentences table
five_most_negative_syuzhet <- beijing_syuzhet |> 
  slice_min(normalized_score, n = 5, with_ties = FALSE) |> 
  select(section_id, sentence_id, sentence, normalized_score)
five_most_negative_syuzhet



plot_llama3_2 <- beijing_llama3_2 |> 
  group_by(section_id, sentiment) |> 
  summarize(n = n(), .groups = "drop") |> 
  group_by(section_id) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  select(-n) |> 
  filter(!sentiment %in% c("positive", "negative")) |>
  
  mutate(
    sentiment = fct_relevel(sentiment,
                            "Joy", "Trust", "Anticipation", "Surprise",
                            "Anger", "Disgust", "Fear", "Sadness")) |>
  ggplot(aes(x = section_id, y = sentiment, fill = prop)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = percent(prop, accuracy = 0.1)),
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  scale_x_continuous(
    breaks = seq(1, 5, by = 1),
    limits = c(0.5, 5.5)
  ) +
  scale_fill_gradient2(
    low = "#f7f7f7",
    mid = "#a8d8d2",
    high = "#2a9d8f",
    midpoint = 0.08,
    labels = percent
  ) +
  labs(
    title = "Emotion Profile Across 5 Sections",
    subtitle = "Model: Llama 3.2",
    caption = "Data: Folding Beijing, Hao Jingfang",
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

plot_llama3_2

ggsave(
  filename = "plots/03-plot_llama3_2.png",
  plot = plot_llama3_2,
  width = 15,
  height = 10,
  dpi = 300
)



plot_emotion_profile_nrc <- beijing_nrc |> 
  group_by(section_id, sentiment) |> 
  summarize(n = n(), .groups = "drop") |> 
  group_by(section_id) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  select(-n) |>
  mutate(
    sentiment = fct_relevel(sentiment,
                            "joy", "trust", "anticipation", "surprise",
                            "anger", "disgust", "fear", "sadness", "neutral")) |> 
  ggplot(aes(x = section_id, y = sentiment, fill = prop)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = percent(prop, accuracy = 0.1)),
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  scale_x_continuous(
    breaks = seq(1, 5, by = 1),
    limits = c(0.5, 5.5)
  ) +
  scale_fill_gradient2(
    low = "#f7f7f7",
    mid = "#a8d8d2",
    high = "#2a9d8f",
    midpoint = 0.08,
    labels = percent
  ) +
  labs(
    title = "Emotion Profile Across 5 Sections",
    subtitle = "NRC lexicon — proportion of sentences by dominant emotion",,
    caption = "Data: Folding Beijing, Hao Jingfang (2015)",
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

plot_emotion_profile_nrc



### Claude Sonnet 5

beijing_sonnet_5 <- readRDS("beijing_sonnet_5.rds")

beijing_sonnet_5 <- beijing_sentences |> 
  bind_cols(beijing_sonnet_5)


plot_beijing_sonnet_5 <- beijing_sonnet_5 |> 
  group_by(section_id, sentiment) |> 
  summarize(n = n(), .groups = "drop") |> 
  group_by(section_id) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  select(-n) |> 
  filter(!sentiment %in% c("positive", "negative")) |>
  mutate(
    sentiment = fct_relevel(sentiment,
                            "Joy", "Trust", "Anticipation", "Surprise",
                            "Anger", "Disgust", "Fear", "Sadness")) |>
  ggplot(aes(x = section_id, y = sentiment, fill = prop)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = percent(prop, accuracy = 0.1)),
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  scale_x_continuous(
    breaks = seq(1, 5, by = 1),
    limits = c(0.5, 5.5)
  ) +
  scale_fill_gradient2(
    low = "#f7f7f7",
    mid = "#a8d8d2",
    high = "#2a9d8f",
    midpoint = 0.08,
    labels = percent
  ) +
  labs(
    title = "Emotion Profile Across 5 Sections",
    subtitle = "Model: Sonnet 5",
    caption = "Data: Folding Beijing, Hao Jingfang",
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

plot_beijing_sonnet_5








