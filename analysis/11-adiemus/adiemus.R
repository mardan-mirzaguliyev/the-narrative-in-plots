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
                          output_name = "data/adiemus",
                          model = "claude-sonnet-5",
                          labels = default_sentiment_labels)

unique(lyrics_sonnet_5$sentiment)


# Gemini Pro

# Mapping the Gemini audio analysis into a structured timeline
adiemus_arc <- tribble(
  ~stage, ~timestamp, ~section, ~emotion, ~valence,
  1, "00:00", "Prelude", "Pastoral Serenity", -0.2,     # Calm, low tension
  2, "00:15", "Verse 1", "Intimate Curiosity", 0.2,     # Awakening, rhythmic pulse
  3, "00:33", "Verse 1 Ext", "Growing Anticipation", 0.5, # Thicker harmony
  4, "00:52", "Chorus 1", "Exuberant Triumph", 0.9,     # Explosive entrance
  5, "01:19", "Bridge", "Peaceful Restoration", -0.1,   # Dynamic drop, breather
  6, "01:56", "Chorus 2", "Ecstatic Majesty", 1.0,      # Peak energy, full density
  7, "02:23", "Verse 2", "Focused Determination", 0.4,  # Abrupt retreat, trance
  8, "02:42", "Chorus 3", "Climactic Catharsis", 1.0,   # Polyphonic web, peak
  9, "03:13", "Outro", "Solemn Reverence", 0.6          # Majestic resolution
)


adiemus_arc |> 
  ggplot(aes(x = stage, y = valence, group = 1)) +
  # The main arc line
  geom_line(color = "gray80", linewidth = 1) +
  
  # Color points based on tension (positive = high energy, negative = calm)
  geom_point(aes(color = valence > 0), size = 4, show.legend = FALSE) +
  
  # Zero-baseline for visual reference
  annotate("segment", x = 1, xend = 9, y = 0, yend = 0, 
           linetype = "dashed", color = "gray50", linewidth = 0.6) +
  
  scale_color_manual(values = c("TRUE" = "#f2a900",   # Bright gold for high density
                                "FALSE" = "#4ba3e3")) + # Cool blue for calm/repose
  
  # Custom x-axis using the section names instead of numbers
  scale_x_continuous(breaks = 1:9, labels = adiemus_arc$section) +
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
    title = "The Emotional Architecture of 'Adiemus'",
    subtitle = "Mapping acoustic tension, choral density, and rhythm over time",
    x = NULL,
    y = "Acoustic Tension & Euphoria"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(size = 18, face = "bold", color = "white", hjust = 0.5),
    plot.subtitle      = element_text(size = 12, color = "gray70", hjust = 0.5, margin = margin(b = 15)),
    axis.text.x        = element_text(size = 10, color = "gray80", angle = 45, hjust = 1),
    axis.text.y        = element_text(size = 12, color = "gray80"),
    axis.title.y       = element_text(color = "gray80", margin = margin(r = 10)),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray30", linewidth = 0.2),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = "#121212", color = NA),
    panel.background   = element_rect(fill = "#121212", color = NA),
    plot.margin        = margin(t = 20, r = 20, b = 20, l = 20)
  )




