## ============================================================================
## load_pipeline.R
## ----------------------------------------------------------------------------
## Single entry point for the Folding Beijing / "If—" / Sonnet 18 sentiment
## analysis pipeline. Sources every numbered method script in common/, in
## dependency order (00-shared_objects.R first, then 01-05), skipping the
## diagnostic checker (00-check_shared_objects.R), which is run on demand,
## never automatically.
##
## IMPORTANT: this file is intentionally NOT named with a numeric prefix
## (e.g. NOT "00-load-pipeline.R") and does NOT call load_pipeline() at its
## own bottom. Both are deliberate — either one alone would let this file
## get swept up by its own scan pattern and recurse infinitely the same way
## 00-check_shared_objects.R once did. Keep it this way.
## ============================================================================

library(stringr)   # str_detect() — used to exclude the checker script

load_pipeline <- function(base_path = "~/Desktop/projects/main/the_narratives_in_graphs/common",
                          max_scripts = 10) {
  
  scripts <- list.files(base_path, pattern = "^\\d{2}-.*\\.R$", full.names = TRUE)
  scripts <- scripts[!str_detect(scripts, "check_shared_objects")]
  scripts <- sort(scripts)   # numeric prefixes guarantee correct dependency order
  
  if (length(scripts) == 0) {
    stop("No numbered scripts found at: ", base_path, " — check the path.")
  }
  
  if (length(scripts) > max_scripts) {
    stop("Expected at most ", max_scripts, " scripts, found ", length(scripts),
         " — check for duplicate or unexpected files before proceeding.")
  }
  
  for (script in scripts) {
    source(script)
    message("Sourced: ", basename(script))
  }
  
  message("Pipeline loaded — ", length(scripts), " scripts.")
}


