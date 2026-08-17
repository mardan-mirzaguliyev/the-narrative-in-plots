library(tidyverse)
library(ggalluvial)
library(ggrepel)
library(forcats)
library(gt)



dair_ai_judged <- poem_preds_dair_ai |> 
  select(text, sentiment = .pred_class) |> 
  get_or_run_judge_claude(text, 
                          sentiment,
                          output_name = "data/sonnet_18", 
                          labels = dair_ai_labels)


go_emotions_judged <- poem_preds_go_emotions |> 
  select(text, sentiment = .pred_class) |> 
  get_or_run_judge_claude(text, 
                          sentiment,
                          output_name = "data/sonnet_18", 
                          labels = go_emotions_labels)

dair_ai_judged
go_emotions_judged

dair_ai_judged |> summarize(agree_rate = mean(agree))
go_emotions_judged |> summarize(agree_rate = mean(agree))


# Plots

plot_data_dair_ai <- dair_ai_judged |> 
  mutate(
    line = row_number(),
    line_label = paste0("Line ", line),
    agreement = if_else(agree, "Agrees", "Disagrees")
  ) |> 
  arrange(desc(agreement == "Agrees"), line)

line_order_dair_ai <- rev(plot_data_dair_ai$line_label)

plot_dair_ai_dumbbell <- plot_data_dair_ai |> 
  ggplot(aes(y = factor(line_label, levels = line_order_dair_ai))) +
  
  geom_tile(
    aes(x = 1.5, fill = agreement),
    width = 1.6, height = 0.9, alpha = 0.18, show.legend = FALSE
  ) +
  geom_segment(
    aes(x = 1, xend = 2, yend = factor(line_label, levels = line_order_dair_ai),
        color = agreement),
    linewidth = 1.8, alpha = 0.95, lineend = "round", show.legend = FALSE
  ) +
  
  geom_point(aes(x = 1), color = "#264653", size = 4) +
  geom_text(
    aes(x = 1, label = str_to_title(sentiment)), 
    color = "#264653", size = 3.2, vjust = -1.4, fontface = "bold"
  ) +
  
  geom_point(aes(x = 2), color = "#e9c46a", size = 4) +
  geom_text(
    aes(x = 2, label = str_to_title(corrected_sentiment)), 
    color = "#b8860b", size = 3.1, vjust = -1.4, fontface = "bold"
  ) +
  
  geom_text(
    aes(x = 2.5, 
        label = if_else(agreement == "Agrees", "✓ agree", "✗ disagree"),
        color = agreement),
    size = 3.0, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  
  scale_color_manual(values = c("Agrees" = "#2a9d8f", "Disagrees" = "#e76f51")) +
  scale_fill_manual(values = c("Agrees" = "#2a9d8f", "Disagrees" = "#e76f51")) +
  scale_x_continuous(
    breaks = c(1, 2), 
    labels = c("DAIR-AI prediction", "Sonnet 5 judgment"), 
    limits = c(0.5, 3.1)
  ) +
  labs(
    title = "How Claude Sonnet 5's Judged DAIR-AI Classifier predictions?",
    subtitle = "Agreement rate: 7.1% (1 of 14 lines); Sonnet 5 disagreed on every line but one",
    caption = "Data: Ozymandias, Percy Bysshe Shelley (1818)",
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

plot_dair_ai_dumbbell

ggsave(
  filename = "plots/01-dair_ai_sonnet5_dumbbell.png",
  plot = plot_dair_ai_dumbbell,
  width = 11,
  height = 12,
  dpi = 300
)


plot_data_go_emotions <- go_emotions_judged |> 
  mutate(
    line = row_number(),
    line_label = paste0("Line ", line),
    agreement = if_else(agree, "Agrees", "Disagrees")
  ) |> 
  arrange(desc(agreement == "Agrees"), line)

line_order_go_emotions <- rev(plot_data_go_emotions$line_label)

plot_go_emotions_dumbbell <- plot_data_go_emotions |> 
  ggplot(aes(y = factor(line_label, levels = line_order_go_emotions))) +
  
  geom_tile(
    aes(x = 1.5, fill = agreement),
    width = 1.6, height = 0.9, alpha = 0.18, show.legend = FALSE
  ) +
  geom_segment(
    aes(x = 1, xend = 2, yend = factor(line_label, levels = line_order_go_emotions),
        color = agreement),
    linewidth = 1.8, alpha = 0.95, lineend = "round", show.legend = FALSE
  ) +
  
  geom_point(aes(x = 1), color = "#264653", size = 4) +
  geom_text(
    aes(x = 1, label = str_to_title(sentiment)), 
    color = "#264653", size = 3.2, vjust = -1.4, fontface = "bold"
  ) +
  
  geom_point(aes(x = 2), color = "#e9c46a", size = 4) +
  geom_text(
    aes(x = 2, label = str_to_title(corrected_sentiment)), 
    color = "#b8860b", size = 3.1, vjust = -1.4, fontface = "bold"
  ) +
  
  geom_text(
    aes(x = 2.5, 
        label = if_else(agreement == "Agrees", "✓ agree", "✗ disagree"),
        color = agreement),
    size = 3.0, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  
  scale_color_manual(values = c("Agrees" = "#2a9d8f", "Disagrees" = "#e76f51")) +
  scale_fill_manual(values = c("Agrees" = "#2a9d8f", "Disagrees" = "#e76f51")) +
  scale_x_continuous(
    breaks = c(1, 2), 
    labels = c("GoEmotions prediction", "Sonnet 5 judgment"), 
    limits = c(0.5, 3.1)
  ) +
  labs(
    title = "How Claude Sonnet 5 judged GoEmotions Classifier predictions?",
    subtitle = "Agreement rate: 64.3% (9 of 14 lines); but every disagreement falls on the poem's emotional high points",
    caption = "Data: Ozymandias, Percy Bysshe Shelley (1818)",
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

plot_go_emotions_dumbbell


ggsave(
  filename = "plots/02-go_emotions_sonnet5_dumbbell.png",
  plot = plot_go_emotions_dumbbell,
  width = 11,
  height = 12,
  dpi = 300
)


# Tables

# 5. 
go_emotions_disagreements <- go_emotions_judged |> 
  filter(agree == FALSE) |> 
  select(text, sentiment, corrected_sentiment, reasoning)

go_emotions_disagreements |> 
  gt() |> 
  cols_label(
    text = "Line",
    sentiment = "GoEmotions",
    corrected_sentiment = "Sonnet 5",
    reasoning = "Sonnet 5's Reasoning"
  ) |> 
  tab_header(
    title = "Where GoEmotions and Sonnet 5 Disagree",
    subtitle = "The 5 of 14 lines where the classifier's \"Neutral\" default missed the poem's actual emotional content"
  ) |> 
  tab_caption("Data: Ozymandias, Percy Bysshe Shelley (1818)") |> 
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
  # Flag both label columns, since the contrast between them is the point
  tab_style(
    style = list(cell_fill(color = "#f4a261"), cell_text(weight = "bold")),
    locations = cells_body(columns = c(sentiment, corrected_sentiment))
  ) |> 
  cols_align(align = "left", columns = c(text, reasoning)) |> 
  cols_align(align = "center", columns = c(sentiment, corrected_sentiment)) |> 
  cols_width(
    text ~ px(220),
    sentiment ~ px(100),
    corrected_sentiment ~ px(100),
    reasoning ~ px(320)
  ) |> 
  gtsave("tables/05-go-emotions-disagreements.png")

