## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

# car, lmtest and moments back individual assumption tests; gridExtra
# arranges the dashboard. All are in Suggests.
has_assumption_pkgs <- requireNamespace("car", quietly = TRUE) &&
  requireNamespace("lmtest", quietly = TRUE)
has_gridextra <- requireNamespace("gridExtra", quietly = TRUE)

## ----setup--------------------------------------------------------------------
library(tidylearn)
library(dplyr)

## -----------------------------------------------------------------------------
model <- tl_model(mtcars, mpg ~ wt + hp + disp, method = "linear")

## ----eval = has_assumption_pkgs-----------------------------------------------
assumptions <- tl_check_assumptions(model, verbose = FALSE)
names(assumptions)

## ----eval = has_assumption_pkgs-----------------------------------------------
assumptions$overall

## ----eval = has_assumption_pkgs-----------------------------------------------
assumptions$normality

## ----eval = has_assumption_pkgs-----------------------------------------------
assumptions$multicollinearity

## ----eval = has_assumption_pkgs-----------------------------------------------
checks <- c("linearity", "independence", "homoscedasticity",
            "normality", "multicollinearity", "outliers")

data.frame(
  assumption = vapply(checks, function(x) assumptions[[x]]$assumption,
                      character(1)),
  holds = vapply(checks, function(x) isTRUE(assumptions[[x]]$check),
                 logical(1)),
  detail = vapply(checks, function(x) assumptions[[x]]$details, character(1)),
  row.names = NULL
)

## ----eval = has_assumption_pkgs && has_gridextra, fig.height = 7--------------
tl_diagnostic_dashboard(model)

## -----------------------------------------------------------------------------
influence <- tl_influence_measures(model)
dim(influence)

## -----------------------------------------------------------------------------
influence |>
  filter(is_influential) |>
  select(observation, cooks_distance, leverage, dffits, std_residual)

## -----------------------------------------------------------------------------
influence |>
  select(observation, starts_with("dfbetas_")) |>
  arrange(desc(abs(dfbetas_wt))) |>
  head(4)

## -----------------------------------------------------------------------------
keep <- !influence$is_influential
refit <- tl_model(mtcars[keep, ], mpg ~ wt + hp + disp, method = "linear")

data.frame(
  term = names(coef(model$fit)),
  all_rows = round(unname(coef(model$fit)), 4),
  without_influential = round(unname(coef(refit$fit)), 4)
)

## -----------------------------------------------------------------------------
sum(!keep)

## -----------------------------------------------------------------------------
outliers <- tl_detect_outliers(
  mtcars,
  variables = c("mpg", "hp", "wt"),
  method = "iqr",
  plot = FALSE
)

outliers$outlier_counts$total
outliers$outlier_counts$by_variable

## -----------------------------------------------------------------------------
mtcars[outliers$outlier_indices, c("mpg", "hp", "wt")]

## -----------------------------------------------------------------------------
mahal <- tl_detect_outliers(
  mtcars,
  variables = c("mpg", "hp", "wt"),
  method = "mahalanobis",
  plot = FALSE
)

mahal$outlier_indices

## -----------------------------------------------------------------------------
simple <- tl_model(mtcars, mpg ~ wt, method = "linear")
full <- tl_model(mtcars, mpg ~ wt + hp + disp, method = "linear")
tree <- tl_model(mtcars, mpg ~ wt + hp + disp, method = "tree")

cv <- tl_compare_cv(
  mtcars,
  models = list(simple = simple, full = full, tree = tree),
  folds = 5,
  metrics = c("rmse", "rsq")
)

names(cv)

## -----------------------------------------------------------------------------
cv$summary

## -----------------------------------------------------------------------------
head(cv$fold_metrics)

## -----------------------------------------------------------------------------
tl_test_model_difference(
  cv,
  baseline_model = "simple",
  metric = "rmse",
  test = "t.test"
)

## -----------------------------------------------------------------------------
tl_test_model_difference(
  cv,
  baseline_model = "simple",
  metric = "rmse",
  test = "wilcox"
)

## -----------------------------------------------------------------------------
interactions <- tl_test_interactions(
  mtcars, mpg ~ wt + hp + disp,
  all_pairs = TRUE
)

interactions

## -----------------------------------------------------------------------------
model_int <- tl_model(mtcars, mpg ~ wt * hp, method = "linear")
effects <- tl_interaction_effects(model_int, var = "wt", by_var = "hp")

effects$slopes

## -----------------------------------------------------------------------------
tl_coefficients(model_int, conf_int = TRUE)

## -----------------------------------------------------------------------------
auto <- tl_auto_interactions(mtcars, mpg ~ wt + hp + disp)
auto$spec$formula

## -----------------------------------------------------------------------------
eda <- tl_explore(iris, response = "Species", max_components = 4, k_range = 2:5)
names(eda)

## -----------------------------------------------------------------------------
eda$optimal_k

## -----------------------------------------------------------------------------
get_pca_variance(eda$pca)

## ----fig.height = 6-----------------------------------------------------------
plot(eda)

