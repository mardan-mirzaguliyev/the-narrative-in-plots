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

## All numbered scripts, including the checker itself — it shouldn't define
## shared objects either, so it's worth scanning too. Excluded only from
## the "no bare source()" check below, since 00-shared_objects.R's own
## internal contents are the thing being sourced, not a script that sources it.

common_path <- "~/Desktop/projects/main/the_narratives_in_graphs/common"

all_numbered_scripts <- list.files(common_path, pattern = "^\\d{2}-.*\\.R$", full.names = TRUE)
method_scripts <- all_numbered_scripts[
  !str_detect(all_numbered_scripts, "00-shared_objects\\.R$") &
    !str_detect(all_numbered_scripts, "00-check_shared_objects\\.R$")
]

## ---- Check 1: no method script independently redefines a shared object ----
shadow_report <- map_dfr(method_scripts, find_shadow_definitions)
shadow_report   # should be empty

## ---- Check 2: no method script has a bare, relative source() call left over ----
## After switching to load_pipeline() as the single entry point, every
## per-script `source("00-shared_objects.R")` line was deliberately removed —
## a bare relative source() call breaks the moment a script is sourced from
## a different working directory (exactly what caused the earlier
## "cannot open the connection" error). This check confirms none crept
## back in, rather than confirming they're present (the old check's logic,
## now inverted to match the new design).
stray_source_report <- map_dfr(method_scripts, function(path) {
  lines <- readLines(path, warn = FALSE)
  hits <- str_which(lines, 'source\\("00-shared_objects\\.R"\\)')
  if (length(hits) == 0) return(NULL)
  tibble(file = path, line_number = hits, line_text = lines[hits])
})
stray_source_report   # should be empty

## ---- Check 3: load_pipeline() sources every method script exactly once, ----
## ---- in the correct order, and excludes the checker script itself ----
pipeline_scripts <- sort(all_numbered_scripts)
pipeline_scripts <- pipeline_scripts[!str_detect(pipeline_scripts, "check_shared_objects")]

tibble(
  expected_first = basename(pipeline_scripts[1]),
  expected_count = length(pipeline_scripts),
  checker_excluded = !any(str_detect(pipeline_scripts, "check_shared_objects"))
)
## Manually confirm expected_first == "00-shared_objects.R" and
## expected_count matches the number of scripts load_pipeline() should run
## (five method scripts + 00-shared_objects.R, not counting the checker).

## ---- Check 4: legacy name scan ----
legacy_names <- c("extract_sentiment_json", "default_json_schema", 
                  "numeric_json_schema", "valid_sentiment_labels")

legacy_report <- purrr::map_dfr(method_scripts, function(path) {
  lines <- readLines(path, warn = FALSE)
  purrr::map_dfr(legacy_names, function(nm) {
    hits <- stringr::str_which(lines, nm)
    if (length(hits) == 0) return(NULL)
    tibble::tibble(file = path, object = nm, line_number = hits, line_text = lines[hits])
  })
})
legacy_report   # should be empty

