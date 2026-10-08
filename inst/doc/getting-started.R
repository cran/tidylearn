## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----eval = FALSE-------------------------------------------------------------
# # From CRAN
# install.packages("tidylearn")
# 
# # Development version
# # devtools::install_github("ces0491/tidylearn") # nolint

## ----setup--------------------------------------------------------------------
library(tidylearn)
library(dplyr)

## -----------------------------------------------------------------------------
# versicolor and virginica overlap, so this is a real classification
# problem -- setosa is linearly separable from the other two, which makes
# logistic regression fail to converge
iris_binary <- iris |>
  filter(Species %in% c("versicolor", "virginica")) |>
  mutate(Species = droplevels(Species))

model_logistic <- tl_model(iris_binary, Species ~ ., method = "logistic")
print(model_logistic)

## -----------------------------------------------------------------------------
# Predicted class labels
predictions <- predict(model_logistic, type = "class")
head(predictions)

## -----------------------------------------------------------------------------
# Class probabilities
head(predict(model_logistic, type = "prob"))

## -----------------------------------------------------------------------------
tl_evaluate(model_logistic, metrics = c("accuracy", "f1"))

## -----------------------------------------------------------------------------
# Regression with linear model
model_linear <- tl_model(mtcars, mpg ~ wt + hp, method = "linear")
print(model_linear)

## -----------------------------------------------------------------------------
# Predictions
predictions_reg <- predict(model_linear)
head(predictions_reg)

## -----------------------------------------------------------------------------
# Principal Component Analysis
model_pca <- tl_model(iris[, 1:4], method = "pca")
print(model_pca)

## -----------------------------------------------------------------------------
# Transform data
transformed <- predict(model_pca)
head(transformed)

## -----------------------------------------------------------------------------
# K-means clustering
model_kmeans <- tl_model(iris[, 1:4], method = "kmeans", k = 3)
print(model_kmeans)

## -----------------------------------------------------------------------------
# Get cluster assignments
clusters <- model_kmeans$fit$clusters
head(clusters)

## -----------------------------------------------------------------------------
# Compare with actual species
table(clusters$cluster, iris$Species)

## -----------------------------------------------------------------------------
# Prepare data with multiple preprocessing steps
processed <- tl_prepare_data(
  iris,
  Species ~ .,
  impute_method = "mean",
  scale_method = "standardize",
  encode_categorical = FALSE
)

## -----------------------------------------------------------------------------
# Check preprocessing steps applied
names(processed$preprocessing_steps)

## -----------------------------------------------------------------------------
# Use processed data for modeling
model_processed <- tl_model(processed$data, Species ~ ., method = "forest")

## -----------------------------------------------------------------------------
# Simple random split
split <- tl_split(iris, prop = 0.7, seed = 123)

# Train model (three species, so a multiclass-capable method)
model_train <- tl_model(split$train, Species ~ ., method = "forest")

# Test predictions
predictions_test <- predict(model_train, new_data = split$test)
head(predictions_test)

## -----------------------------------------------------------------------------
# Stratified split (maintains class proportions)
split_strat <- tl_split(iris, prop = 0.7, stratify = "Species", seed = 123)

# Check proportions are maintained
prop.table(table(split_strat$train$Species))
prop.table(table(split_strat$test$Species))
prop.table(table(iris$Species))

## -----------------------------------------------------------------------------
# Example: Access the raw randomForest object
model_forest <- tl_model(iris, Species ~ ., method = "forest")
class(model_forest$fit)  # This is the randomForest object

# Use package-specific functions if needed
# randomForest::varImpPlot(model_forest$fit) # nolint

## -----------------------------------------------------------------------------
model_pca <- tl_model(iris, ~ ., method = "pca")
names(model_pca$fit)
class(model_pca$fit$model)  # This is the prcomp object

## -----------------------------------------------------------------------------
# Quick example combining everything
data_split <- tl_split(iris, prop = 0.7, stratify = "Species", seed = 42)

# Random forests are scale-invariant, so no scaling is needed here. When a
# method does need scaled inputs, the same transformation has to be applied
# to the test set -- see the Supervised Learning vignette.
model_final <- tl_model(data_split$train, Species ~ ., method = "forest")
test_preds <- predict(model_final, new_data = data_split$test)

accuracy <- mean(test_preds$.pred == data_split$test$Species)
cat("Test accuracy:", round(accuracy * 100, 1), "%\n")

