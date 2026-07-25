library(stringr)
library(purrr)
library(tibble)


shared_object_names <- c(
  "sentiment_system_prompt", "judge_system_prompt",
  "extract_json_response", "extract_sentiment_json",
  "sentiment_colors", "sentiment_levels", "valid_sentiment_labels"
)

assignment_pattern <- function(name) paste0("^\\s*", name, "\\s*(<-|=)\\s*")

find_shadow_definitions <- function(path) {
  lines <- readLines(path, warn = FALSE)
  purrr::map_dfr(shared_object_names, function(nm) {
    hits <- str_which(lines, assignment_pattern(nm))
    if (length(hits) == 0) return(NULL)
    tibble::tibble(file = path, object = nm, line_number = hits)
  })
}

method_scripts <- list.files(pattern = "^\\d{2}-.*\\.R$", full.names = TRUE)
method_scripts <- method_scripts[!str_detect(method_scripts, "00-shared_objects\\.R$")]

shadow_report <- purrr::map_dfr(method_scripts, find_shadow_definitions)
shadow_report   # should be empty


purrr::map_dfr(method_scripts, function(path) {
  lines <- readLines(path, warn = FALSE)
  tibble::tibble(file = path, has_source_line = any(str_detect(lines, 'source\\("00-shared_objects\\.R"\\)')))
})


