## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(tidylearn)
library(dplyr)

## -----------------------------------------------------------------------------
tuned_tree <- tl_tune_grid(
  iris, Species ~ .,
  method = "tree",
  param_grid = list(cp = c(0.001, 0.01, 0.1), minsplit = c(5, 20)),
  folds = 3,
  verbose = FALSE
)

## -----------------------------------------------------------------------------
print(tuned_tree)

## -----------------------------------------------------------------------------
tuning <- attr(tuned_tree, "tuning_results")
names(tuning)

## -----------------------------------------------------------------------------
tuning$results

## -----------------------------------------------------------------------------
# The settings that won, and the score they won with
tuning$best_params
tuning$best_metric

## -----------------------------------------------------------------------------
tuned_tree$fit$control$cp

## -----------------------------------------------------------------------------
tuned_reg <- tl_tune_grid(
  mtcars, mpg ~ .,
  method = "forest",
  param_grid = list(mtry = c(2, 4), ntree = c(100, 300)),
  folds = 3,
  metric = "rmse",
  verbose = FALSE
)

attr(tuned_reg, "tuning_results")$best_params

## -----------------------------------------------------------------------------
tl_default_param_grid("tree", size = "small")

## -----------------------------------------------------------------------------
tl_default_param_grid("forest", size = "medium")

## -----------------------------------------------------------------------------
tuned_default <- tl_tune_grid(
  iris, Species ~ .,
  method = "tree",
  param_grid = tl_default_param_grid("tree", size = "small"),
  folds = 3,
  verbose = FALSE
)

attr(tuned_default, "tuning_results")$best_params

## -----------------------------------------------------------------------------
tuned_random <- tl_tune_random(
  iris, Species ~ .,
  method = "tree",
  param_space = list(
    cp = c(0.0001, 0.2),                 # continuous
    minsplit = c(2, 5, 10, 20, 30, 40)   # drawn from these six
  ),
  n_iter = 8,
  folds = 3,
  seed = 42,
  verbose = FALSE
)

attr(tuned_random, "tuning_results")$best_params

## -----------------------------------------------------------------------------
tl_plot_tuning_results(tuned_tree, plot_type = "scatter")

## -----------------------------------------------------------------------------
tl_plot_tuning_results(tuned_tree, plot_type = "grid")

## -----------------------------------------------------------------------------
tl_plot_tuning_results(tuned_tree, plot_type = "parallel")

## -----------------------------------------------------------------------------
tl_plot_tuning_results(tuned_tree, plot_type = "importance")

## -----------------------------------------------------------------------------
split <- tl_split(iris, prop = 0.7, stratify = "Species", seed = 42)

pipe <- tl_pipeline(
  split$train, Species ~ .,
  preprocessing = list(standardize = TRUE),
  models = list(
    tree = list(method = "tree"),
    forest = list(method = "forest", ntree = 300)
  ),
  evaluation = list(
    validation = "cv",
    cv_folds = 3,
    metrics = c("accuracy", "f1"),
    best_metric = "accuracy"
  )
)

print(pipe)

## ----error = TRUE-------------------------------------------------------------
try({
tl_pipeline(split$train, Species ~ .,
            preprocessing = list(scale_method = "standardize"))
})

## -----------------------------------------------------------------------------
run <- tl_run_pipeline(pipe, verbose = FALSE)

names(run$models)

## -----------------------------------------------------------------------------
print(run)

## -----------------------------------------------------------------------------
best <- tl_get_best_model(run)
best$spec$method

## -----------------------------------------------------------------------------
preds <- tl_predict_pipeline(run, new_data = split$test, model_name = "forest")
head(preds)

## -----------------------------------------------------------------------------
mean(preds$.pred == split$test$Species)

## -----------------------------------------------------------------------------
path <- tempfile(fileext = ".rds")
tl_save_pipeline(run, path)

reloaded <- tl_load_pipeline(path)
names(reloaded$models)

## -----------------------------------------------------------------------------
# Predictions survive the round trip, preprocessing included
reloaded_preds <- tl_predict_pipeline(
  reloaded, new_data = split$test, model_name = "forest"
)
identical(reloaded_preds$.pred, preds$.pred)

## ----include = FALSE----------------------------------------------------------
unlink(path)

## -----------------------------------------------------------------------------
tuned <- tl_tune_grid(
  split$train, Species ~ .,
  method = "forest",
  param_grid = list(mtry = c(2, 3), ntree = c(100, 300)),
  folds = 3,
  verbose = FALSE
)

best_params <- attr(tuned, "tuning_results")$best_params
best_params

## -----------------------------------------------------------------------------
final <- tl_pipeline(
  split$train, Species ~ .,
  models = list(
    forest = c(list(method = "forest"), best_params)
  ),
  evaluation = list(cv_folds = 3, metrics = "accuracy",
                    best_metric = "accuracy")
)

final_run <- tl_run_pipeline(final, verbose = FALSE)
final_preds <- tl_predict_pipeline(final_run, new_data = split$test)

mean(final_preds$.pred == split$test$Species)

