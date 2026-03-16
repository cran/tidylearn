## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  message = FALSE,
  warning = FALSE
)

## ----setup--------------------------------------------------------------------
library(tidylearn)
library(dplyr)
library(ggplot2)
library(gt)

## ----plot-regression----------------------------------------------------------
model_reg <- tl_model(mtcars, mpg ~ wt + hp, method = "linear")

# Actual vs predicted — one call
plot(model_reg, type = "actual_predicted")

## ----plot-classification------------------------------------------------------
split <- tl_split(iris, prop = 0.7, stratify = "Species", seed = 42)
model_clf <- tl_model(split$train, Species ~ ., method = "forest")

plot(model_clf, type = "confusion")

## ----plot-pca-----------------------------------------------------------------
pca <- tidy_pca(USArrests, scale = TRUE)

tidy_pca_screeplot(pca)
tidy_pca_biplot(pca, label_obs = TRUE)

## ----plot-lasso---------------------------------------------------------------
model_lasso <- tl_model(mtcars, mpg ~ ., method = "lasso")

tl_plot_regularization_path(model_lasso)
tl_plot_regularization_cv(model_lasso)

## ----table-auto, eval = FALSE-------------------------------------------------
# tl_table(model)                       # auto-selects the best table type
# tl_table(model, type = "coefficients") # specific type

## ----table-metrics------------------------------------------------------------
tl_table_metrics(model_reg)

## ----table-coef---------------------------------------------------------------
tl_table_coefficients(model_reg)

## ----table-coef-lasso---------------------------------------------------------
tl_table_coefficients(model_lasso)

## ----table-confusion----------------------------------------------------------
tl_table_confusion(model_clf, new_data = split$test)

## ----table-importance---------------------------------------------------------
tl_table_importance(model_clf)

## ----table-variance-----------------------------------------------------------
pca_model <- tl_model(USArrests, method = "pca")
tl_table_variance(pca_model)

## ----table-loadings-----------------------------------------------------------
tl_table_loadings(pca_model)

## ----table-clusters-----------------------------------------------------------
km <- tl_model(iris[, 1:4], method = "kmeans", k = 3)
tl_table_clusters(km)

## ----table-comparison---------------------------------------------------------
m1 <- tl_model(split$train, Species ~ ., method = "logistic")
m2 <- tl_model(split$train, Species ~ ., method = "forest")
m3 <- tl_model(split$train, Species ~ ., method = "tree")

tl_table_comparison(
  m1, m2, m3,
  new_data = split$test,
  names = c("Logistic", "Random Forest", "Decision Tree")
)

## ----plotly, eval = FALSE-----------------------------------------------------
# library(plotly)
# 
# ggplotly(plot(model_reg, type = "actual_predicted"))
# ggplotly(tidy_pca_biplot(pca, label_obs = TRUE))
# ggplotly(tl_plot_regularization_path(model_lasso))

## ----workflow-----------------------------------------------------------------
# Fit
model <- tl_model(split$train, Species ~ ., method = "forest")

# Evaluate
tl_table_metrics(model, new_data = split$test)

# Visualise
plot(model, type = "confusion")

# Drill into feature importance
tl_table_importance(model, top_n = 4)

