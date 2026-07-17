library(rulexicon)


# Changes, Kino
## 2.1 Lexicon based sentiments
gs4_auth()

lyrics_id <- "13dM_5vWQGdAeRI-thE6nbzlZIbvbZ8dw9eCu3MsP9Kk"
lyrics_raw_kino <- read_sheet(lyrics_id, "Changes Kino")


## Access to RuSentiLex lexicon
rusentilex_data <- hash_rusentilex_2017
head(rusentilex_data)
