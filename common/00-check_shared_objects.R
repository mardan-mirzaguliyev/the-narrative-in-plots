library(stringr)
library(purrr)
library(tibble)


shared_object_names <- c(
  # Prompts — generic functions
  "get_sentiment_system_prompt", "get_sentiment_numeric_system_prompt",
  "get_judge_system_prompt",
  # Prompts — fixed defaults (derived, should never be independently defined elsewhere)
  "sentiment_system_prompt", "sentiment_numeric_system_prompt",
  "numeric_only_system_prompt", "judge_system_prompt",
  # Worked-example fragments
  "categorical_prompt_examples", "categorical_numeric_prompt_examples",
  # Schemas
  "categorical_json_schema", "categorical_numeric_json_schema",
  "numeric_only_json_schema", "judge_json_schema",
  # Label set / chart constants
  "sentiment_levels", "sentiment_colors",
  # Helpers
  "extract_json_response", "labels_tag_for",
  # Legacy names — should never reappear; kept here specifically to catch stragglers
  "extract_sentiment_json", "default_json_schema", "numeric_json_schema",
  "valid_sentiment_labels"
)

assignment_pattern <- function(name) paste0("^\\s*", name, "\\s*(<-|=)\\s*")

find_shadow_definitions <- function(path) {
  lines <- readLines(path, warn = FALSE)
  map_dfr(shared_object_names, function(nm) {
    hits <- str_which(lines, assignment_pattern(nm))
    if (length(hits) == 0) return(NULL)
    tibble(file = path, object = nm, line_number = hits)
  })
}

method_scripts <- list.files(pattern = "^\\d{2}-.*\\.R$", full.names = TRUE)
method_scripts <- method_scripts[!str_detect(method_scripts, "00-shared_objects\\.R$")]

shadow_report <- map_dfr(method_scripts, find_shadow_definitions)
shadow_report   # should be empty

## Confirm every method script sources the shared file
map_dfr(method_scripts, function(path) {
  lines <- readLines(path, warn = FALSE)
  tibble(file = path, has_source_line = any(str_detect(lines, 'source\\("00-shared_objects\\.R"\\)')))
})

## Existence check — same pattern as before, but now driven by the full
## legacy-name list rather than just valid_sentiment_labels, so any
## straggling reference to an old name is caught in one pass rather than
## needing a new ad hoc check every time something gets renamed.
legacy_names <- c("extract_sentiment_json", "default_json_schema", 
                  "numeric_json_schema", "valid_sentiment_labels")

purrr::map_dfr(method_scripts, function(path) {
  lines <- readLines(path, warn = FALSE)
  purrr::map_dfr(legacy_names, function(nm) {
    hits <- stringr::str_which(lines, nm)
    if (length(hits) == 0) return(NULL)
    tibble::tibble(file = path, object = nm, line_number = hits, line_text = lines[hits])
  })
})

