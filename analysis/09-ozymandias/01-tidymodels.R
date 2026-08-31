# Session A everything reticulate/Python - import and save DAIR-AI emotion data set
library(dplyr)
library(reticulate)
library(purrr)


use_condaenv("r-reticulate", required = TRUE)
hf_datasets <- import("datasets")

# DAIR-AI data set import
dair_ai_raw <- hf_datasets$load_dataset(
  "dair-ai/emotion", 
  "unsplit"
)

glimpse(dair_ai_raw)


dair_ai <- dair_ai_raw$train$to_pandas()
full_dair_ai <- py_to_r(dair_ai)
glimpse(full_dair_ai)

dair_ai_labels <- c("sadness", "joy", "love", "anger", "fear", "surprise")

full_dair_ai <- full_dair_ai |> 
  mutate(sentiment = factor(dair_ai_labels[label + 1], levels = dair_ai_labels)) |> 
  select(text, sentiment)
glimpse(full_dair_ai)

saveRDS(full_dair_ai, "data/hf-datasets/full_dair_ai.rds")


# Google go_emotions data set import
go_emotions_raw <- hf_datasets$load_dataset(
  "google-research-datasets/go_emotions", 
  "raw", 
  split = "train"
)

glimpse(go_emotions_raw)

# Convert Hugging Face dataset split directly to Pandas / R data frame
full_go_emotions <- py_to_r(go_emotions_raw$to_pandas())
glimpse(full_go_emotions)

# Official 28 class labels for GoEmotions (indices 0 to 27)
go_emotions_labels <- c(
  "admiration", "amusement", "anger", "annoyance", "approval", 
  "caring", "confusion", "curiosity", "desire", "disappointment", 
  "disapproval", "disgust", "embarrassment", "excitement", "fear", 
  "gratitude", "grief", "joy", "love", "nervousness", 
  "optimism", "pride", "realization", "relief", "remorse", 
  "sadness", "surprise", "neutral"
)

# Extract primary sentiment from the 28 binary indicator columns
full_go_emotions <- full_go_emotions |>
  mutate(
    # Find which of the 28 columns equals 1 for each row
    sentiment_str = apply(across(all_of(go_emotions_labels)), 1, function(row) {
      match_idx <- which(row == 1)
      if (length(match_idx) > 0) go_emotions_labels[match_idx[1]] else NA_character_
    }),
    sentiment = as.factor(sentiment_str)
  ) |>
  filter(!is.na(sentiment)) |>
  select(text, sentiment, everything())

# Verify your transformed dataset
glimpse(full_go_emotions)


saveRDS(full_go_emotions, "data/hf-datasets/full_go_emotions.rds")


# Restart the session after saving the train results

# Session B — tidymodels/textrecipes work, reticulate never loaded here
library(readxl)
library(dplyr)
library(stringr)
library(tidymodels)
library(textrecipes)
library(glmnet)
library(gt)
library(here)
library(dotenv)
library(googlesheets4)  # <-- Added missing library for read_sheet()


load_dot_env(file = here(".env"))


# Clean data with global replacement for quotes
poem_id <- Sys.getenv("OZYMANDIAS_ID")

gs4_auth()

poem_raw <- read_sheet(poem_id, sheet = "Ozymandias") 
poem_raw


# Load DAIR-AI data set
full_dair_ai <- readRDS("data/hf-datasets/full_dair_ai.rds")
glimpse(full_dair_ai)


# DAIR-AI Results

# Split Data
set.seed(123)
split_dair_ai <- initial_split(full_dair_ai, strata = sentiment, prop = 0.8)
train_dair_ai <- training(split_dair_ai)
test_dair_ai <- testing(split_dair_ai)


recipe_dair_ai <- recipe(sentiment ~ text, data = train_dair_ai) |>  
  step_tokenize(text) |>  
  step_stopwords(text) |>  
  step_tokenfilter(text, max_tokens = 2500, min_times = 20) |>  
  step_tfidf(text)

model_spec_dair_ai <- multinom_reg(penalty = tune(), mixture = 1) |>  
  set_engine("glmnet") |>  
  set_mode("classification")

workflow_dair_ai <- workflow() |>  
  add_recipe(recipe_dair_ai) |>  
  add_model(model_spec_dair_ai)

set.seed(456)
emotion_folds_dair_ai <- vfold_cv(train_dair_ai, v = 3, strata = sentiment)
penalty_grid_dair_ai <- grid_regular(penalty(range = c(-2, 0)), levels = 5)


tuned_results_dair_ai <- tune_grid(
  workflow_dair_ai,
  resamples = emotion_folds_dair_ai,
  grid = penalty_grid_dair_ai,
  metrics = metric_set(accuracy, roc_auc)
)

best_penalty_dair_ai <- select_best(tuned_results_dair_ai, metric = "accuracy")
final_workflow_dair_ai <- finalize_workflow(workflow_dair_ai, best_penalty_dair_ai)
fit_dair_ai <- fit(final_workflow_dair_ai, data = train_dair_ai)

# Evaluate 2500-token model on test set
preds_test_dair_ai <- fit_dair_ai |>  
  predict(test_dair_ai, type = "class") |>  
  bind_cols(fit_dair_ai |> predict(test_dair_ai, type = "prob")) |>  
  bind_cols(test_dair_ai |> select(sentiment))

metrics_dair_ai <- preds_test_dair_ai |> metrics(truth = sentiment, estimate = .pred_class)
conf_dair_ai <- preds_test_dair_ai |> conf_mat(truth = sentiment, estimate = .pred_class)

# Apply 2500-token model to Ozymandias (Corrected from fit_2_dair_ai to fit_dair_ai)
poem_preds_dair_ai <- fit_dair_ai |>  
  predict(poem_raw, type = "class") |>  
  bind_cols(fit_dair_ai |> predict(poem_raw, type = "prob")) |>  
  bind_cols(poem_raw)

print(metrics_2500)
print(conf_2500)


# Google Go Emotions

# Load Google Go Emotions data set
full_go_emotions <- readRDS("data/hf-datasets/full_go_emotions.rds")
full_go_emotions <- full_go_emotions |> 
  select(text, sentiment)
glimpse(full_go_emotions)


# Google Go Emotions Results with max_tokens set to 2500

# Split Data
set.seed(123)
split_go_emotions <- initial_split(full_go_emotions, strata = sentiment, prop = 0.8)
train_go_emotions <- training(split_go_emotions)
test_go_emotions <- testing(split_go_emotions)


recipe_go_emotions <- recipe(sentiment ~ text, data = train_go_emotions) |>  
  step_tokenize(text) |>  
  step_stopwords(text) |>  
  step_tokenfilter(text, max_tokens = 2500, min_times = 15) |>  
  step_tfidf(text)

model_spec_go_emotions <- multinom_reg(penalty = tune(), mixture = 1) |>  
  set_engine("glmnet") |>  
  set_mode("classification")

workflow_go_emotions <- workflow() |>  
  add_recipe(recipe_go_emotions) |>  
  add_model(model_spec_go_emotions)

set.seed(456)
emotion_folds_go_emotions <- vfold_cv(train_go_emotions, v = 3, strata = sentiment)
penalty_grid_go_emotions <- grid_regular(penalty(range = c(-2, 0)), levels = 5)


tuned_results_go_emotions <- tune_grid(
  workflow_go_emotions,
  resamples = emotion_folds_go_emotions,
  grid = penalty_grid_go_emotions,
  metrics = metric_set(accuracy, roc_auc)
)

best_penalty_go_emotions <- select_best(tuned_results_go_emotions, metric = "accuracy")
final_workflow_go_emotions <- finalize_workflow(workflow_go_emotions, best_penalty_go_emotions)
fit_go_emotions <- fit(final_workflow_go_emotions, data = train_go_emotions)

# Evaluate 2500-token model on test set (Corrected from go_emoitons to fit_go_emotions)
preds_test_go_emotions <- fit_go_emotions |>  
  predict(test_go_emotions, type = "class") |>  
  bind_cols(fit_go_emotions |> predict(test_go_emotions, type = "prob")) |>  
  bind_cols(test_go_emotions |> select(sentiment))

metrics_go_emotions <- preds_test_go_emotions |> metrics(truth = sentiment, estimate = .pred_class)
conf_go_emotions <- preds_test_go_emotions |> conf_mat(truth = sentiment, estimate = .pred_class)

# Apply 2500-token model to Ozymandias
poem_preds_go_emotions <- fit_go_emotions |>  
  predict(poem_raw, type = "class") |>  
  bind_cols(fit_go_emotions |> predict(poem_raw, type = "prob")) |>  
  bind_cols(poem_raw)


# Tables

# 1.
dominant_class_comparison <- bind_rows(
  train_dair_ai |> 
    count(sentiment, sort = TRUE) |> 
    mutate(prop = n / sum(n), dataset = "dair-ai/emotion (6 categories)"),
  train_go_emotions |> 
    count(sentiment, sort = TRUE) |> 
    mutate(prop = n / sum(n), dataset = "GoEmotions (28 categories)")
) |> 
  group_by(dataset) |> 
  slice_max(n, n = 1) |> 
  ungroup() |> 
  select(dataset, dominant_class = sentiment, n, prop)

dominant_class_comparison |> 
  gt() |> 
  cols_label(
    dataset = "Training Set",
    dominant_class = "Most Common Class",
    n = "Rows",
    prop = "Share of Training Data"
  ) |> 
  fmt_number(columns = n, decimals = 0, use_seps = TRUE) |> 
  fmt_percent(columns = prop, decimals = 1) |> 
  tab_header(
    title = "The Poem's Predicted Label Matches Each Model's Own Training Bias",
    subtitle = "Both classifiers defaulted to their single largest training category on nearly every line of Ozymandias"
  ) |> 
  tab_caption("Data: DAIR-AI/emotion and GoEmotions training splits") |> 
  # Header styling
  tab_style(
    style = list(cell_fill(color = "#2a9d8f"), cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) |> 
  # Base body styling
  tab_style(
    style = cell_fill(color = "#cbe8f5"),
    locations = cells_body()
  ) |> 
  # Flag the dominant-class and share columns, since those are the headline numbers
  tab_style(
    style = list(cell_fill(color = "#f4a261"), cell_text(weight = "bold")),
    locations = cells_body(columns = c(dominant_class, prop))
  ) |> 
  cols_align(align = "left", columns = dataset) |> 
  cols_align(align = "center", columns = c(dominant_class, n, prop)) |> 
  gtsave("tables/01-dominant_categories.png")

# 2
poem_preds_dair_ai |> 
  select(line, text, .pred_class, .pred_sadness, .pred_joy, .pred_love, .pred_anger, .pred_fear, .pred_surprise) |> 
  gt() |> 
  cols_label(
    line = "Line",
    text = "Text",
    .pred_class = "Predicted",
    .pred_sadness = "Sadness",
    .pred_joy = "Joy",
    .pred_love = "Love",
    .pred_anger = "Anger",
    .pred_fear = "Fear",
    .pred_surprise = "Surprise"
  ) |> 
  fmt_number(columns = c(.pred_sadness, .pred_joy, .pred_love, .pred_anger, .pred_fear, .pred_surprise), decimals = 3) |> 
  tab_header(
    title = "tidymodels Classifier Predictions: \"Ozymandias\"",
    subtitle = "Trained on DAIR-AI/emotion, applied line by line to Shelley's sonnet"
  ) |> 
  tab_caption("Data: Ozymandias, Percy Bysshe Shelley (1818)") |> 
  # Header styling
  tab_style(
    style = list(cell_fill(color = "#2a9d8f"), cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) |> 
  # Base body styling
  tab_style(
    style = cell_fill(color = "#cbe8f5"),
    locations = cells_body()
  ) |> 
  # Flag the two lines that break from the repeated baseline — lines 5 and 6
  tab_style(
    style = list(cell_fill(color = "#f4a261"), cell_text(weight = "bold")),
    locations = cells_body(rows = line %in% c(5, 6))
  ) |> 
  cols_align(align = "left", columns = text) |> 
  cols_align(align = "center", columns = c(line, .pred_class, .pred_sadness, .pred_joy, .pred_love, .pred_anger, .pred_fear, .pred_surprise)) |> 
  cols_width(
    line ~ px(50),
    text ~ px(260),
    .pred_class ~ px(90)
  ) |> 
  gtsave("tables/02-poem_preds_dair_ai.png")


# 3.
poem_preds_dair_ai |> 
  count(.pred_sadness, .pred_joy, .pred_love, .pred_anger, .pred_fear, .pred_surprise, name = "n_lines") |> 
  arrange(desc(n_lines)) |> 
  gt() |> 
  cols_label(
    .pred_sadness = "Sadness",
    .pred_joy = "Joy",
    .pred_love = "Love",
    .pred_anger = "Anger",
    .pred_fear = "Fear",
    .pred_surprise = "Surprise",
    n_lines = "Lines (count)"
  ) |> 
  fmt_number(columns = c(.pred_sadness, .pred_joy, .pred_love, .pred_anger, .pred_fear, .pred_surprise), decimals = 3) |> 
  tab_header(
    title = "How Many Distinct Predictions Did the DAIR-AI Actually Make?",
    subtitle = "14 lines of poem share just 3 unique probability vectors"
  ) |> 
  tab_caption("Data: Ozymandias, Percy Bysshe Shelley (1818)") |> 
  # Header styling
  tab_style(
    style = list(cell_fill(color = "#2a9d8f"), cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) |> 
  # Base body styling
  tab_style(
    style = cell_fill(color = "#cbe8f5"),
    locations = cells_body()
  ) |> 
  # Flag the "n_lines" column, since that's the headline number
  tab_style(
    style = list(cell_fill(color = "#f4a261"), cell_text(weight = "bold")),
    locations = cells_body(columns = n_lines)
  ) |> 
  cols_align(align = "center", columns = everything()) |> 
  gtsave("tables/03-unique-preds_dair_ai.png")

# 4.
go_emotions_pred_cols <- poem_preds_go_emotions |> 
  select(starts_with(".pred_"), -.pred_class) |> 
  names()

poem_preds_go_emotions |> 
  count(across(all_of(go_emotions_pred_cols)), name = "n_lines") |> 
  nrow()   # just the count of distinct probability vectors — one number


distinct_vector_count <- poem_preds_go_emotions |> 
  count(across(all_of(go_emotions_pred_cols))) |> 
  nrow()

tibble(
  method = "GoEmotions (28-category classifier)",
  total_lines = nrow(poem_preds_go_emotions),
  distinct_probability_vectors = distinct_vector_count
) |> 
  gt() |> 
  cols_label(
    method = "Method",
    total_lines = "Total Lines",
    distinct_probability_vectors = "Distinct Predictions"
  ) |> 
  tab_header(
    title = "How Many Distinct Predictions Did the GoEmotions Classifier Make?",
  ) |> 
  tab_caption("Data: Ozymandias, Percy Bysshe Shelley (1818)") |> 
  tab_style(
    style = list(cell_fill(color = "#2a9d8f"), cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = cell_fill(color = "#cbe8f5"),
    locations = cells_body()
  ) |> 
  tab_style(
    style = list(cell_fill(color = "#f4a261"), cell_text(weight = "bold")),
    locations = cells_body(columns = distinct_probability_vectors)
  ) |> 
  cols_align(align = "center", columns = everything()) |> 
  gtsave("tables/04-unique-preds_go_emotions.png")




if (!dir.exists("data/output-data")) dir.create("data/output-data")

# Save DAIR-AI outputs directly into data/
saveRDS(fit_dair_ai, "data/output-data/fit_dair_ai.rds")
saveRDS(preds_test_dair_ai, "data/output-data/preds_test_dair_ai.rds")
saveRDS(poem_preds_dair_ai, "data/output-data/poem_preds_dair_ai.rds")

# Save Go emotions outputs directly into data/
saveRDS(fit_go_emotions, "data/output-data/fit_go_emotions.rds")
saveRDS(preds_test_go_emotions, "data/output-data/preds_test_go_emotions.rds")
saveRDS(poem_preds_go_emotions, "data/output-data/poem_preds_go_emotions.rds")


fit_dair_ai        <- readRDS("data/output-data/fit_dair_ai.rds")
preds_test_dair_ai  <- readRDS("data/output-data/preds_test_dair_ai.rds")
poem_preds_dair_ai <- readRDS("data/output-data/poem_preds_dair_ai.rds")

fit_go_emotions         <- readRDS("data/output-data/fit_go_emotions.rds")
preds_test_go_emotions  <- readRDS("data/output-data/preds_test_go_emotions.rds")
poem_preds_go_emotions  <- readRDS("data/output-data/poem_preds_go_emotions.rds")


