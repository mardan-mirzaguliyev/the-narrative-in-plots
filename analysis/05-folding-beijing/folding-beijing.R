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


beijing_sentences <- beijing_tbl |> 
  unnest_tokens(sentence, text, token = "sentences") |> 
  mutate(sentence_id = row_number(),
         sentence = str_to_sentence(sentence)) |> 
  select(section_id, sentence_id, sentence)

beijing_sentences



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

beijing_nrc <- 
  beijing_nrc |> 
  mutate(sentiment = str_to_sentence(sentiment))

beijing_nrc 


sentiment_colors <- c("Neutral" = "#adb5bd",
                      "Trust" = "#2a9d8f", "Anticipation" = "#e9c46a", 
                      "Fear" = "#264653", "Joy" = "#f4a261", 
                      "Anger" = "#e76f51", "Sadness" = "#6d6875", 
                      "Surprise" = "#84a59d", "Disgust" = "#bc6c25")


plot_emotion_profile_nrc <- beijing_nrc |> 
  group_by(section_id, sentiment) |> 
  summarize(n = n(), .groups = "drop") |> 
  group_by(section_id) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  select(-n) |>
  mutate(
    sentiment = fct_relevel(sentiment,
                            "Joy", "Trust", "Anticipation", "Surprise",
                            "Anger", "Disgust", "Fear", "Sadness", "Neutral")) |> 
  ggplot(aes(x = section_id, y = sentiment, fill = prop)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = percent(prop, accuracy = 0.1)),
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  scale_x_continuous(breaks = seq(1, 5, by = 1), limits = c(0.5, 5.5)) +
  scale_fill_gradient2(
    low = "#f7f7f7", mid = "#a8d8d2", high = "#2a9d8f",
    midpoint = 0.08, labels = percent
  ) +
  labs(
    title = "Emotion Profile Across 5 Sections",
    subtitle = "NRC lexicon — proportion of sentences by dominant emotion",
    caption = "Data: Folding Beijing, Hao Jingfang (2015)",
    x = NULL, y = NULL, fill = "Proportion"
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
  scale_fill_manual(values = c("Neutral" = "#adb5bd", "Trust" = "#2a9d8f", 
                               "Anticipation" = "#e9c46a", "Fear" = "#264653", 
                               "Joy" = "#f4a261", "Anger" = "#e76f51",
                               "Sadness" = "#6d6875", "Surprise" = "#84a59d",
                               "Disgust" = "#bc6c25")) +
  scale_x_continuous(limits = c(0, 534), breaks = seq(0, 534, 50)) +
  geom_label(aes(label = n),
             hjust = -0.2,
             colour = "black", 
             fontface = "bold",
             size = 3.5,
             fill = "white",
             linewidth = 0.5) +
  labs(title = "Neutral Dominates, followed by Anticipation and Trust Lead the Story's Emotional Register",
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
  get_or_run_local(sentence, output_name = "beijing_local", model = "llama3.2:latest")

beijing_llama3_2



plot_beijing_llama3_2 <- beijing_llama3_2 |> 
  group_by(section_id, sentiment) |> 
  summarize(n = n(), .groups = "drop") |> 
  group_by(section_id) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  select(-n) |> 
  mutate(
    sentiment = fct_relevel(sentiment,
                            "Joy", "Trust", "Anticipation", "Surprise",
                            "Anger", "Disgust", "Fear", "Sadness", "Neutral")) |>
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

plot_beijing_llama3_2 


ggsave(
  filename = "plots/03-plot_beijing_llama3_2.png",
  plot = plot_beijing_llama3_2 ,
  width = 15,
  height = 10,
  dpi = 300
)



plot_emotional_breakdown_llama3_2 <- beijing_llama3_2 |> 
  group_by(sentiment) |> 
  summarize(n = n()) |> 
  arrange(desc(n)) |> 
  ggplot(aes(n, fct_reorder(sentiment, n))) +
  # Using a distinct palette for clearer visual separation
  geom_col(aes(fill = sentiment), show.legend = FALSE) +
  scale_fill_manual(values = c("Neutral" = "#adb5bd", "Trust" = "#2a9d8f", 
                               "Anticipation" = "#e9c46a", "Fear" = "#264653", 
                               "Joy" = "#f4a261", "Anger" = "#e76f51",
                               "Sadness" = "#6d6875", "Surprise" = "#84a59d",
                               "Disgust" = "#bc6c25")) +
  scale_x_continuous(limits = c(0, 312), breaks = seq(0, 312, 50)) +
  geom_label(aes(label = n),
             hjust = -0.2,
             colour = "black", 
             fontface = "bold",
             size = 3.5,
             fill = "white",
             linewidth = 0.5) +
  labs(title = "Neutral Dropped significantly: Fear Leads the Story's Emotional Register",
       subtitle = "Story-level sentiment breakdown using Llama 3",
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

plot_emotional_breakdown_llama3_2


ggsave(
  filename = "plots/04-plot_emotional_breakdown_llama3_2.png",
  plot = plot_emotional_breakdown_llama3_2,
  width = 15,
  height = 10,
  dpi = 300
)


### Claude Sonnet 5

# Batch — full run, explicit output name, saves as "beijing_claude_sonnet-5.rds"
beijing_sonnet_5 <- beijing_sentences |> 
  get_or_run_claude_batch(sentence, sentence_id, output_name = "beijing_claude")

beijing_sonnet_5


beijing_sonnet_5 |> 
  mutate(row_position = row_number()) |> 
  summarize(mismatches = sum(sentence_id != row_position), total = n())



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
                            "Anger", "Disgust", "Fear", "Sadness", "Neutral")) |>
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

plot_beijing_sonnet_5


ggsave(
  filename = "plots/05-plot_beijing_sonnet_5.png",
  plot = plot_beijing_sonnet_5,
  width = 15,
  height = 10,
  dpi = 300
)



plot_emotional_breakdown_sonnet_5 <- beijing_sonnet_5 |> 
  group_by(sentiment) |> 
  summarize(n = n()) |> 
  arrange(desc(n)) |> 
  ggplot(aes(n, fct_reorder(sentiment, n))) +
  # Using a distinct palette for clearer visual separation
  geom_col(aes(fill = sentiment), show.legend = FALSE) +
  scale_fill_manual(values = c("Neutral" = "#adb5bd", "Trust" = "#2a9d8f", 
                               "Anticipation" = "#e9c46a", "Fear" = "#264653", 
                               "Joy" = "#f4a261", "Anger" = "#e76f51",
                               "Sadness" = "#6d6875", "Surprise" = "#84a59d",
                               "Disgust" = "#bc6c25")) +
  scale_x_continuous(limits = c(0, 335), breaks = seq(0, 335, 50)) +
  geom_label(aes(label = n),
             hjust = -0.2,
             colour = "black", 
             fontface = "bold",
             size = 3.5,
             fill = "white",
             linewidth = 0.5) +
  labs(title = "Neutrality rises significantly followed by Anticipation and Fear",
       subtitle = "Story-level sentiment breakdown using Claude Sonnet 5",
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

plot_emotional_breakdown_sonnet_5 


ggsave(
  filename = "plots/06-plot_emotional_breakdown_sonnet_5.png",
  plot = plot_emotional_breakdown_sonnet_5,
  width = 15,
  height = 10,
  dpi = 300
)



## Checks for Conclusion

nrc |> 
  filter(word == "waste") |> 
  gt() |> 
  cols_label(
    word = "Word",
    sentiment = "Sentiment"
  ) |>
  tab_header(
    title = "Filtered word: waste",
    subtitle = "Labeled sentiment: disgust"
    ) |> 
  tab_source_note("Data: NRC lexicon") |>
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
  gtsave("tables/01-waste-in-nrc.png")



beijing_llama3_2 |> 
  group_by(sentiment) |> 
  summarise(min_conf = min(confidence_score),
            max_conf = max(confidence_score),
            avg_conf = mean(confidence_score))



beijing_sonnet_5 |> 
  group_by(sentiment) |> 
  summarise(min_conf = min(confidence_score),
            max_conf = max(confidence_score),
            avg_conf = mean(confidence_score))





beijing_sonnet_5 |> 
  filter(sentiment == "Neutral", confidence_score < 0.1) |> 
  select(sentence_id, sentence, confidence_score)



beijing_llama3_2 |> 
  filter(sentiment == "Neutral", confidence_score < 0.1) |> 
  select(sentence_id, sentence, confidence_score)



beijing_nrc |> 
  filter(sentence_id == 2)

beijing_sonnet_5 |> 
  filter(sentence_id == 2)


beijing_nrc |> 
  filter(sentence_id == 916)

beijing_llama3_2 |> 
  filter(sentence_id == 916)










