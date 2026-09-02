## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  # tl_auto_ml() narrates its progress through message(); the narration is
  # useful at the console and noise in a document
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(tidylearn)
library(dplyr)

## -----------------------------------------------------------------------------
result <- tl_auto_ml(
  iris, Species ~ .,
  time_budget = 30,
  cv_folds = 3
)

## -----------------------------------------------------------------------------
result$leaderboard

## -----------------------------------------------------------------------------
result$best_model

## -----------------------------------------------------------------------------
result$task
result$metric
round(as.numeric(result$runtime, units = "secs"), 1)

## -----------------------------------------------------------------------------
result_reg <- tl_auto_ml(
  mtcars, mpg ~ .,
  time_budget = 30,
  cv_folds = 3
)

result_reg$task

## -----------------------------------------------------------------------------
result_reg$leaderboard

## -----------------------------------------------------------------------------
names(result$models)

## ----warning = FALSE----------------------------------------------------------
# The same search on a two-class problem picks up the logistic variants
iris_binary <- iris %>%
  filter(Species != "setosa") %>%
  mutate(Species = droplevels(Species))

binary_result <- tl_auto_ml(iris_binary, Species ~ ., time_budget = 30,
                            cv_folds = 3)
names(binary_result$models)

## -----------------------------------------------------------------------------
tl_model(iris_binary, Species ~ ., method = "logistic")$spec$method

## -----------------------------------------------------------------------------
table(result$leaderboard$evaluation)

## -----------------------------------------------------------------------------
budgets <- c(2, 5, 10, 30)

sweep <- lapply(budgets, function(b) {
  t0 <- Sys.time()
  r <- tl_auto_ml(iris, Species ~ ., time_budget = b, cv_folds = 3)
  data.frame(
    budget = b,
    elapsed = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1),
    models = nrow(r$leaderboard),
    cv_scored = sum(r$leaderboard$evaluation == "cv"),
    best = r$leaderboard$model[1]
  )
})

do.call(rbind, sweep)

## -----------------------------------------------------------------------------
# Baselines only -- no PCA, no cluster features
baseline_only <- tl_auto_ml(
  iris, Species ~ .,
  use_reduction = FALSE,
  use_clustering = FALSE,
  time_budget = 30,
  cv_folds = 3
)

names(baseline_only$models)

## -----------------------------------------------------------------------------
# Keep PCA, drop clustering
no_clustering <- tl_auto_ml(
  iris, Species ~ .,
  use_clustering = FALSE,
  time_budget = 30,
  cv_folds = 3
)

names(no_clustering$models)

## ----warning = FALSE----------------------------------------------------------
by_f1 <- tl_auto_ml(iris_binary, Species ~ ., metric = "f1",
                    time_budget = 30, cv_folds = 3)

by_f1$metric
by_f1$leaderboard

## -----------------------------------------------------------------------------
split <- tl_split(iris, prop = 0.7, stratify = "Species", seed = 123)

automl <- tl_auto_ml(split$train, Species ~ ., time_budget = 30, cv_folds = 3)

test_preds <- predict(automl$best_model, new_data = split$test)
mean(test_preds$.pred == split$test$Species)

## -----------------------------------------------------------------------------
available <- names(automl$models)
available

## -----------------------------------------------------------------------------
scores <- vapply(available, function(nm) {
  preds <- predict(automl$models[[nm]], new_data = split$test)
  mean(preds$.pred == split$test$Species)
}, numeric(1))

data.frame(model = available, test_accuracy = round(scores, 3),
           row.names = NULL) %>%
  arrange(desc(test_accuracy))

## -----------------------------------------------------------------------------
manual <- tl_model(split$train, Species ~ ., method = "forest")
manual_acc <- mean(
  predict(manual, new_data = split$test)$.pred == split$test$Species
)

automl_acc <- mean(test_preds$.pred == split$test$Species)

data.frame(
  approach = c("forest, chosen by hand", "AutoML best"),
  test_accuracy = round(c(manual_acc, automl_acc), 3)
)

## -----------------------------------------------------------------------------
processed <- tl_prepare_data(
  split$train, Species ~ .,
  scale_method = "standardize",
  remove_correlated = TRUE
)

automl_processed <- tl_auto_ml(processed$data, Species ~ .,
                               time_budget = 30, cv_folds = 3)

automl_processed$leaderboard$model[1]

## -----------------------------------------------------------------------------
sum(is.na(result$leaderboard$score))

## -----------------------------------------------------------------------------
winner <- automl$best_model$spec$method
winner

## -----------------------------------------------------------------------------
tuned <- tl_tune_grid(
  split$train, Species ~ .,
  method = winner,
  param_grid = tl_default_param_grid(winner, size = "small"),
  folds = 3,
  verbose = FALSE
)

attr(tuned, "tuning_results")$best_params

## -----------------------------------------------------------------------------
mean(predict(tuned, new_data = split$test)$.pred == split$test$Species)

