library(tidyverse)
library(pdftools)
library(tibble)
library(tidytext)


manifesto_path <- "data/the-communist-manifesto.pdf"

manifesto_raw <- pdf_text(manifesto_path)


####### Apply steps below to core text

manifesto_df <- tibble(page = 1:length(manifesto_raw), text = manifesto_raw) |> 
  unnest_tokens(word, text)
manifesto_df |> count()


tidy_manifesto <- manifesto_df |> 
  anti_join(stop_words)

nrow(manifesto_df)
nrow(tidy_manifesto)

word_counts <- tidy_manifesto |> 
  count(word, sort = TRUE)

word_counts

####################


## Ready
core_pages <- manifesto_raw[14:34]
core_text <- paste(core_pages, collapse = " ")

core_text <- str_squish(core_text)
core_text <- str_replace_all(core_text, "\\n", " ")

nchar(core_text)


chapter_titles <- c(
  "Bourgeois and Proletarians",
  "Proletarians and Communists",
  "Socialist and Communist Literature",
  "Position of the Communists in Relation to the Various Existing Opposition Parties"
)

# All should return TRUE
sapply(chapter_titles, str_detect, string = core_text)


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

# All should return FALSE
sapply(preface_checks, str_detect, string = core_text)
sapply(appendix_checks, str_detect, string = core_text)



