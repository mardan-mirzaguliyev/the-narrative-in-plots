# DATA PREPARATION

# Load necessary packages

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
library(httr2)          
library(jsonlite)   
library(ollamar)
library(patchwork)
library(writexl)


story_path <- "data/beijing.pdf"
story_raw <- pdf_text(story_path)


### Collapse all text first
story_full_text <- paste(story_raw, collapse = " ") |> 
  str_squish()
story_full_text

sections_list <- str_split(story_full_text, "(?=\\d+\\.\\s)")[[1]]
sections_list

beijing_tbl <- tibble(
  section_id = 1:5,
  text = str_remove(sections_list[2:6], "^\\d+\\.\\s*")
)

beijing_tbl |> 
  unnest_tokens(word, text) |>
  count()




