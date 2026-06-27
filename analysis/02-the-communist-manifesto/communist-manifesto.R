# DATA PREPARATION

# Load necessary packages


library(tidyverse)
library(pdftools)
library(tibble)
library(tidytext)
library(ggimage)
library(magick)
library(rsvg)


# Data preparation
manifesto_path <- "data/the-communist-manifesto.pdf"
manifesto_raw <- pdf_text(manifesto_path)


## 1. Define check objects to check presence and absence of the parts

# Chapter names to check the presence
chapter_titles <- c(
  "Bourgeois and Proletarians",
  "Proletarians and Communists",
  "Socialist and Communist Literature",
  "Position of the Communists"
)

preface_checks <- c(
  "Communist League",                        # Editorial introduction
  "Preface to The 1872 German Edition",      # 1872 preface title
  "Preface to The 1882 Russian Edition",     # 1882 preface title
  "Preface to The 1883 German Edition",      # 1883 preface title
  "Preface to The 1888 English Edition",     # 1888 preface title
  "rests at Highgate Cemetery"               # Engels' personal note about Marx, 1883 preface
)


appendix_checks <- c(
  "Dear Marx",                               # Engels letter
  "Communist Confession of Faith",           # Draft confession
  "Principles of Communism",                 # Principles document
  "Demands of the Communist Party",          # Demands document
  "Paris Commune"                            # Paris Commune address
)

### All should return TRUE
sapply(chapter_titles, 
       str_detect, 
       string = paste(manifesto_raw[14:34], collapse = " "))

### All should return FALSE
sapply(preface_checks, str_detect, string = paste(manifesto_raw[14:34], collapse = " "))
sapply(appendix_checks, str_detect, string = paste(manifesto_raw[14:34], collapse = " "))


# 2. Build a clean, tokenized data frame
chapter_tibble <- tibble(
  chapter = c(
    rep("Ch1_Bourgeois_and_Proletarians", 8),  # pages 14:21
    rep("Ch2_Proletarians_and_Communists", 6), # pages 22:27
    rep("Ch3_Socialist_Literature", 6),        # pages 28:33
    rep("Ch4_Opposition_Parties", 1)           # page 34
  ), 
  text = manifesto_raw[14:34]
) |> 
  unnest_tokens(word, text)

glimpse(chapter_tibble)


chapter_tibble |> 
  count()

# Remove stop words

chapter_tibble_clean <- chapter_tibble |> 
  anti_join(stop_words, by = "word")

chapter_tibble_clean |> count()


# ANALYSIS

chapter_tibble_clean |> 
  mutate(word = fct_recode(word, 
                           "bourgeoisie" = "bourgeois")) |> 
  group_by(word) |> 
  summarize(num_word = n()) |> 
  arrange(desc(num_word))


## Word count plot

circle_crop <- function(path,
                        border_color = "#2a9d8f",
                        border_size = 10) {
  
  img <- image_read(path) |>
    image_resize("300x300!")
  
  # Create mask as SVG — no graphics device needed
  svg_mask <- paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="300" height="300">',
    '<circle cx="150" cy="150" r="145" fill="white"/>',
    '</svg>'
  )
  
  mask <- image_read_svg(svg_mask, width = 300, height = 300)
  
  # Apply mask
  img_circle <- image_composite(img, mask, operator = "CopyOpacity")
  
  
  # Add border as SVG circle
  # Border radius should match the mask radius (145)
  svg_border <- paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="300" height="300">',
    '<circle cx="150" cy="150" r="145" ',
    'fill="none" stroke="', border_color, '" ',
    'stroke-width="', border_size, '"/>',
    '</svg>'
  )
  
  border <- image_read_svg(svg_border, width = 300, height = 300)
  
  # Composite border on image
  img_final <- image_composite(img_circle, border, operator = "Over")
  
  # Save to temp file
  tmp <- tempfile(fileext = ".png")
  image_write(img_final, tmp, format = "png")
  return(tmp)
}


marx_circle   <- circle_crop("images/marx.jpeg", 
                             border_color = "#2a9d8f",   # teal
                             border_size  = 8)

engels_circle <- circle_crop("images/engels.jpg", 
                             border_color = "#2a9d8f",   # teal
                             border_size  = 8)



authors <- tibble(
  image = c(marx_circle, engels_circle),
  x = c(155, 185),
  y = c(1.5, 1.5),
  label  = c("Karl Marx", "Frederick Engels")
)

authors




p1 <- chapter_tibble_clean |> 
  mutate(word = as_factor(word)) |> 
  mutate(word = fct_recode(word, 
                           "bourgeoisie" = "bourgeois")) |> 
  group_by(word) |> 
  summarize(num_word = n()) |> 
  slice_max(order_by = num_word, n = 10) |>
  ggplot(aes(x = num_word, y = fct_reorder(word, num_word))) +
  geom_col(aes(fill = num_word), show.legend = FALSE) +
  geom_image(
    data = authors,
    aes(x = x, y = y + 1, image = image), 
    size = 0.2,
    inherit.aes = FALSE
  ) + 
  geom_text(
    data = authors,
    aes(x = x, y = y - 0.2, label = label), # nudge label below image
    inherit.aes = FALSE,
    size = 3,
    fontface = "bold",
    color = "black"
  ) +
  scale_x_continuous(
    breaks = seq(0, 200, by = 20),
    limits = c(0, 200)
    ) +
  scale_fill_gradient(
    low  = "#a8d8d2",
    high = "#2a9d8f"
  ) +
  geom_label(
    aes(label = num_word),
    vjust    = 0.5,      # pushes label above the point
    colour   = "black",
    fontface = "bold",
    size     = 3.5,
    fill     = "white",   # label background
    linewidth = 0.2      # border thickness around label box
  ) +
  labs(
    title    = "Bourgeoisie is the most frequent word in The Communist Manifesto",
    subtitle = "Two variations merged: 'bourgeoisie' and 'bourgeois'",
    x        = "Word",
    y        = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text         = element_text(color = "black"),
    axis.ticks = element_line(color = "black", ),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA)
  )

print(p1)

ggsave(
  filename = "plots/01-most-frequent-words.png",
  plot = p1,
  width = 15,
  height = 10,
  dpi = 300
)


## Download AFINN lexicon
# AFINN scores words –5 (very negative) to +5 (very positive)
# Valence = sum(scores) / (n_scored_words * 5), giving –1 to +1
# Here, arc will have both the original and normalized scores.
# For visualization I will use mean valence which is the original afinn scores
afinn <- get_sentiments("afinn")

manifesto_scored <- chapter_tibble_clean |> 
  left_join(afinn, by = "word") |> 
  arrange(desc(value))

## Get the original chapter names for better visualizations
### "Ch1: Bourgeois and Proletarians"
### "Ch2: Proletarians and Communists"
### "Ch3: Socialist and Communist Literature"
### "Ch4: Position of the Communists in Relation to the Various Existing Opposition Parties"
### Actually, used just Chapter numbers: Chapter 1, 2, 3, 4

manifesto_scored <- manifesto_scored |> 
  mutate(chapter = as_factor(chapter),
         chapter = fct_recode(chapter,
                              "Chapter 1" = "Ch1_Bourgeois_and_Proletarians",
                              "Chapter 2" = "Ch2_Proletarians_and_Communists",
                              "Chapter 3" = "Ch3_Socialist_Literature",
                              "Chapter 4" = "Ch4_Opposition_Parties"
                              )
         )

arc <- manifesto_scored |> 
  group_by(chapter) |> 
  summarize(
    n_scored = sum(!is.na(value)),
    raw_sum = sum(value, na.rm = TRUE),
    mean_valence = mean(value, na.rm = TRUE),
    # Valence = sum(scores) / (n_scored_words * 5), giving –1 to +1
    chapter_valence = ifelse(
      n_scored > 0,
      raw_sum / (n_scored * 5),
      0
    ),
    .groups = "drop"
  ) |> 
  arrange(chapter)


p2 <- ggplot(arc, aes(x = chapter, y = mean_valence,
                color = chapter,
                group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_point(size = 3, show.legend = FALSE) +
  scale_color_manual(values = c(
    "Chapter 1" = "#0072B2",
    "Chapter 2" = "#D55E00",
    "Chapter 3" = "#009E73",
    "Chapter 4" = "#CC79A7"
  )) +
  geom_label(
    aes(label = round(mean_valence, 2)),
    vjust    = -0.2,      # pushes label above the point
    hjust = -0.8,
    colour   = "black",
    fontface = "bold",
    size     = 5,
    fill     = "white",   # label background
    linewidth = 0.2,      # border thickness around label box
    show.legend = FALSE
  ) +
  labs(
    title = "Emotional Arc - Chapter-by-Chapter Valence",
    subtitle = "AFINN scores between -5 / +5",
    x = "Chapter",
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

p2

ggsave(
  filename = "plots/02-chapter-valences.png",
  plot = p2,
  width = 15,
  height = 10,
  dpi = 300
)


























