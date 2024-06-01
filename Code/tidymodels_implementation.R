options(tidymodels.dark = TRUE)

library(here)
library(tidyverse)
library(tidymodels)
library(workflowsets)
library(future)

# Load data
pneumonia_data <- read_csv(here("DATA_SETS", "pneumonia_data.csv")) |> 
  mutate(pneumonia = as.factor(pneumonia))

# Split data and create folds (k-folded cross-validation)
set.seed(12345)
pneumonia_split <- initial_split(pneumonia_data, prop = 0.8, strata = pneumonia)
pneumonia_train <- training(pneumonia_split)
pneumonia_test <- training(pneumonia_split)

folds <- vfold_cv(pneumonia_train, v = 10, repeats = 3)

dummy_recipe <- 
  recipe(formula = pneumonia ~ age + gender + tobacco_use + PM2_5, 
         data = pneumonia_train) |> 
  step_dummy(all_nominal_predictors())

rpart_spec <- 
  decision_tree(cost_complexity = tune(), 
                tree_depth = tune(), 
                min_n = tune()) |> 
  set_mode("classification") |> 
  set_engine("rpart")

rf_spec <- 
  rand_forest(mtry = tune(), 
              trees = tune(), 
              min_n = tune()) |> 
  set_mode("classification") |> 
  set_engine("ranger")

xgb_spec <- 
  boost_tree(mtry = tune(),
             trees = tune(),
             min_n = tune()) |> 
  set_mode("classification") |> 
  set_engine("xgboost")

model_set <-  workflow_set(
  preproc = list(dummy_encoded = dummy_recipe),
  models = list(rpart = rpart_spec, 
                ranger = rf_spec, 
                xgb = xgb_spec),
  cross = TRUE
)

# Set up parallel processing
max_cores <- parallel::detectCores()
plan(multisession, workers = max_cores)

set.seed(12345)
model_set <- model_set |> 
  workflow_map(
    fn = "tune_grid", 
    resamples = folds, 
    grid = 50, 
    verbose = FALSE,
    seed = 12345,
    control = control_grid(save_pred = TRUE, parallel_over = "everything")
  )

# Evaluate performance
autoplot(model_set)

# Select best of each
autoplot(model_set, select_best = TRUE)

# Rank results
rank_results(model_set, rank_metric = "roc_auc", select_best = TRUE) |>  
  select(rank, mean, model, wflow_id, .config)
