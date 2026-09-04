library(googlesheets4)
library(tidyverse)
library(tidymodels)
library(textrecipes)
library(glmnet)
library(gt)
library(here)
library(dotenv)


load_dot_env(file = here(".env"))

lyrics_id <- Sys.getenv("ADIEMUS_ID")


gs4_auth()

lyrics_raw <- read_sheet(lyrics_id, sheet = "Adiemus") 
lyrics_raw

# Claude Sonnet 5
lyrics_sonnet_5 <- lyrics_raw |>
  get_or_run_claude_synch(text_col = text,
                          output_name = "data/output/adiemus",
                          model = "claude-sonnet-5",
                          labels = default_sentiment_labels)

unique(lyrics_sonnet_5$sentiment)
unique(lyrics_sonnet_5$reasoning)


# Gemini 3.6 Flash Results turned into tibble

# Mapping the Gemini audio analysis into a structured timeline

adiemus_arc <- tribble(
  ~stage, ~timestamp, ~section, ~emotion, ~valence,
  1,  "00:00", "Intro",                  "Pastoral Solitude",       -0.3,
  2,  "00:15", "A-Theme Part I",         "Voice Awakens",            0.1,
  3,  "00:33", "A-Theme Part II",        "Rhythmic Lock",            0.4,
  4,  "00:52", "B-Theme",                "Solar Burst",              0.9,
  5,  "01:19", "Interlude",              "Receding Tide",           -0.2,
  6,  "01:56", "B-Theme Return",         "Re-energized Affirmation", 0.95,
  7,  "02:24", "A-Theme Restatement",    "Intimate Compression",     0.2,
  8,  "02:41", "Pre-Climax Surge",       "Coiled Tension",           0.6,
  9,  "02:57", "Climactic Apex",         "Monumental Density",       1.0,
  10, "03:12", "Outro Transition",       "Ancestral Chant",          0.5,
  11, "03:40", "Final Dissolve",         "Residual Reverberation",  -0.1
)

adiemus_arc

plot_adiemus_arc <- adiemus_arc |> 
  ggplot(aes(x = stage, y = valence, group = 1)) +
  # The main arc line
  geom_line(color = "gray80", linewidth = 1) +
  # Color points based on tension (positive = high energy, negative = calm)
  geom_point(aes(color = valence > 0), size = 4, show.legend = FALSE) +
  
  # Zero-baseline for visual reference
  annotate("segment", x = 1, xend = 9, y = 0, yend = 0, 
           linetype = "dashed", color = "gray50", linewidth = 0.6) +
  scale_color_manual(values = c("TRUE" = "#f2a900",  
                                "FALSE" = "#4ba3e3")) + 
  # Custom x-axis using the section names instead of numbers
  scale_x_continuous(breaks = 1:11, labels = adiemus_arc$section) +
  # Add the emotion labels above the points
  geom_label(aes(label = emotion),
             vjust = -0.5,
             fill = "#1e1e1e",
             color = "white",
             linewidth = 0.2,
             size = 3.5,
             fontface = "bold") +
  # Expand limits slightly so labels don't cut off
  coord_cartesian(ylim = c(-0.5, 1.3)) +
  labs(
    title = "Mapping acoustic tension, choral density, and rhythm over time",
    subtitle = "Gemini 3.6 Flash jugments after 'listening' to the song mp3",
    caption = "Data: Adiemus, Karl Jenkins (1995)",
    x = NULL,
    y = "Acoustic Tension & Euphoria"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 10, hjust = 0.5),
    axis.text         = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 25, color = "black"),
    panel.grid         = element_blank(),
    plot.background   = element_rect(fill = "#cbe8f5", color = NA),
    panel.background  = element_rect(fill = "#cbe8f5", color = NA),
    legend.position   = "none",
    plot.margin        = margin(t = 20, r = 25, b = 20, l = 25)
  )

plot_adiemus_arc


ggsave(
  filename = "plots/01-plot_adiemus_arc.png",
  plot = plot_adiemus_arc,
  width = 15,
  height = 10,
  dpi = 300
)

