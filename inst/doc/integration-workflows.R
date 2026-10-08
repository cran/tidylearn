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
library(ggplot2)

## -----------------------------------------------------------------------------
# Reduce dimensions before classification
reduced <- tl_reduce_dimensions(iris,
                                response = "Species",
                                method = "pca",
                                n_components = 3)

# Inspect reduced data
head(reduced$data)

## -----------------------------------------------------------------------------
# Train classifier on the reduced features. iris has three species, so this
# needs a multiclass-capable method -- logistic regression is binary only.
model_reduced <- tl_model(reduced$data, Species ~ ., method = "forest")
print(model_reduced)

## -----------------------------------------------------------------------------
# Training-set accuracy. Use tl_evaluate() rather than comparing $.pred by
# hand: what $.pred holds depends on the method and prediction type.
tl_evaluate(model_reduced)

## -----------------------------------------------------------------------------
# Split data for fair comparison
split <- tl_split(iris, prop = 0.7, stratify = "Species", seed = 123)

# Model with original features
model_original <- tl_model(split$train, Species ~ ., method = "forest")
eval_original <- tl_evaluate(model_original, new_data = split$test)

# Model with PCA features
reduced_train <- tl_reduce_dimensions(split$train,
                                      response = "Species",
                                      method = "pca",
                                      n_components = 3)

model_pca <- tl_model(reduced_train$data, Species ~ ., method = "forest")

# The test set must be projected through the PCA fitted on the training
# data -- refitting PCA on the test set would leak information
test_transformed <- predict(
  reduced_train$reduction_model,
  new_data = split$test |> select(-Species)
)
test_transformed$Species <- split$test$Species

eval_pca <- tl_evaluate(model_pca, new_data = test_transformed)

# Compare results
acc <- function(x) round(x$value[x$metric == "accuracy"] * 100, 1)
n_original <- ncol(split$train) - 1
n_reduced <- sum(grepl("^PC", names(reduced_train$data)))
cat("Original features:", n_original, "->", acc(eval_original), "%\n")
cat("PCA features:", n_reduced, "->", acc(eval_pca), "%\n")
cat("Feature reduction:",
    round((1 - n_reduced / n_original) * 100, 1), "%\n")

## -----------------------------------------------------------------------------
# Add cluster features
data_clustered <- tl_add_cluster_features(iris,
                                          response = "Species",
                                          method = "kmeans",
                                          k = 3)

# Check new features
names(data_clustered)

## -----------------------------------------------------------------------------
# Train model with cluster features
model_cluster <- tl_model(data_clustered, Species ~ ., method = "forest")
print(model_cluster)

## -----------------------------------------------------------------------------
# Compare models with and without cluster features
split_comp <- tl_split(iris, prop = 0.7, stratify = "Species", seed = 42)

# Without cluster features
model_no_cluster <- tl_model(split_comp$train, Species ~ ., method = "forest")
preds_no_cluster <- predict(model_no_cluster, new_data = split_comp$test)
acc_no_cluster <- mean(preds_no_cluster$.pred == split_comp$test$Species)

# With cluster features
train_clustered <- tl_add_cluster_features(split_comp$train,
                                           response = "Species",
                                           method = "kmeans",
                                           k = 3)
model_with_cluster <- tl_model(train_clustered, Species ~ ., method = "forest")

# Need to get cluster model for test data
cluster_model <- attr(train_clustered, "cluster_model")
test_clusters <- predict(cluster_model, new_data = split_comp$test[, -5])
test_clustered <- split_comp$test
test_clustered$cluster_kmeans <- as.factor(test_clusters$cluster)

preds_with_cluster <- predict(model_with_cluster, new_data = test_clustered)
acc_with_cluster <- mean(preds_with_cluster$.pred == split_comp$test$Species)

cat("Without cluster features:", round(acc_no_cluster * 100, 1), "%\n")
cat("With cluster features:", round(acc_with_cluster * 100, 1), "%\n")

## -----------------------------------------------------------------------------
# Use only 10% of labels
set.seed(123)
labeled_indices <- sample(nrow(iris), size = 15)  # 15 of 150 labelled

# supervised_method defaults to "tree". Named here because a forest is
# the better fit for propagated labels: the propagation step introduces
# noise, and averaging over trees absorbs more of it than one tree does.
model_semi <- tl_semisupervised(iris, Species ~ .,
                                labeled_indices = labeled_indices,
                                cluster_method = "kmeans",
                                supervised_method = "forest")

print(model_semi)

## -----------------------------------------------------------------------------
# Check how labels were propagated
label_mapping <- model_semi$semisupervised_info$label_mapping
print(label_mapping)

## -----------------------------------------------------------------------------
# Evaluate against the true labels
preds_semi <- predict(model_semi, new_data = iris, type = "class")
accuracy_semi <- mean(preds_semi$.pred == iris$Species)

cat("Accuracy with only", length(labeled_indices), "labels:",
    round(accuracy_semi * 100, 1), "%\n")
labeled_pct <- round(
  length(labeled_indices) / nrow(iris) * 100, 1
)
cat("Proportion of data labeled:", labeled_pct, "%\n")

## -----------------------------------------------------------------------------
# Fully supervised with same amount of data
labeled_data <- iris[labeled_indices, ]
model_full <- tl_model(labeled_data, Species ~ ., method = "forest")
preds_full <- predict(model_full, new_data = iris, type = "class")
accuracy_full <- mean(preds_full$.pred == iris$Species)

cat("Fully supervised (15 samples):", round(accuracy_full * 100, 1), "%\n")
cat("Semi-supervised (15 labels + propagation):",
    round(accuracy_semi * 100, 1), "%\n")

## ----eval=FALSE---------------------------------------------------------------
# # Flag anomalies as a feature
# model_anomaly_flag <- tl_anomaly_aware(iris, Species ~ .,
#                                        response = "Species",
#                                        anomaly_method = "dbscan",
#                                        action = "flag",
#                                        supervised_method = "forest")
# 
# # Check anomaly info
# cat("Anomalies detected:", model_anomaly_flag$anomaly_info$n_anomalies, "\n")

## ----eval=FALSE---------------------------------------------------------------
# # Remove anomalies before training
# model_anomaly_remove <- tl_anomaly_aware(iris, Species ~ .,
#                                          response = "Species",
#                                          anomaly_method = "dbscan",
#                                          action = "remove",
#                                          supervised_method = "forest")
# 
# cat("Anomalies removed:", model_anomaly_remove$anomalies_removed, "\n")

## -----------------------------------------------------------------------------
# Train separate models for different clusters
stratified_models <- tl_stratified_models(mtcars, mpg ~ .,
                                          cluster_method = "kmeans",
                                          k = 3,
                                          supervised_method = "linear")

# Check structure
names(stratified_models)
length(stratified_models$supervised_models)

## -----------------------------------------------------------------------------
# Predictions using stratified models
preds_stratified <- predict(stratified_models)
head(preds_stratified)

## -----------------------------------------------------------------------------
# Calculate RMSE
rmse_stratified <- sqrt(mean((preds_stratified$.pred - mtcars$mpg)^2))
cat("Stratified Model RMSE:", round(rmse_stratified, 2), "\n")

# Compare with single model
model_single <- tl_model(mtcars, mpg ~ ., method = "linear")
preds_single <- predict(model_single)
rmse_single <- sqrt(mean((preds_single$.pred - mtcars$mpg)^2))
cat("Single Model RMSE:", round(rmse_single, 2), "\n")

## -----------------------------------------------------------------------------
# Step 1: Split data
workflow_split <- tl_split(iris, prop = 0.7, stratify = "Species", seed = 42)

# Step 2: Reduce dimensions
workflow_reduced <- tl_reduce_dimensions(workflow_split$train,
                                         response = "Species",
                                         method = "pca",
                                         n_components = 3)

# Step 3: Add cluster features to reduced data
workflow_clustered <- tl_add_cluster_features(workflow_reduced$data,
                                              response = "Species",
                                              method = "kmeans",
                                              k = 3)

# Step 4: Train final model
workflow_model <- tl_model(workflow_clustered, Species ~ ., method = "forest")

print(workflow_model)

## -----------------------------------------------------------------------------
# Transform test data through same pipeline
# 1. Apply PCA transformation
test_pca <- predict(workflow_reduced$reduction_model,
                    new_data = workflow_split$test[, -5])
test_pca$Species <- workflow_split$test$Species

# 2. Get cluster assignments. The cluster model was fitted on the PC
# columns; predict() matches new_data to those columns by name and
# errors on a mismatch rather than assigning against the wrong ones.
cluster_model_wf <- attr(workflow_clustered, "cluster_model")
test_clusters_wf <- predict(cluster_model_wf, new_data = test_pca)
test_pca$cluster_kmeans <- as.factor(test_clusters_wf$cluster)

# 3. Predict
workflow_preds <- predict(workflow_model, new_data = test_pca)
workflow_accuracy <- mean(workflow_preds$.pred == workflow_split$test$Species)

cat("Complete Workflow Accuracy:", round(workflow_accuracy * 100, 1), "%\n")

## -----------------------------------------------------------------------------
# Simulate credit data
set.seed(42)
n <- 500
credit_data <- data.frame(
  age = rnorm(n, 40, 12),
  income = rnorm(n, 50000, 20000),
  debt_ratio = runif(n, 0, 0.5),
  credit_score = rnorm(n, 700, 100),
  years_employed = rpois(n, 5)
)

# Create target variable (default risk)
credit_data$default <- factor(
  ifelse(
    credit_data$debt_ratio > 0.4 & credit_data$credit_score < 650,
    "Yes", "No"
  )
)

# Split data
credit_split <- tl_split(
  credit_data, prop = 0.7, stratify = "default", seed = 123
)

## -----------------------------------------------------------------------------
# Strategy 1: Add customer segments as features
credit_clustered <- tl_add_cluster_features(credit_split$train,
                                            response = "default",
                                            method = "kmeans",
                                            k = 4)

model_credit <- tl_model(credit_clustered, default ~ ., method = "forest")

# Transform test data
cluster_model_credit <- attr(credit_clustered, "cluster_model")
test_clusters_credit <- predict(cluster_model_credit,
                                new_data = credit_split$test[, -6])
test_credit <- credit_split$test
test_credit$cluster_kmeans <- as.factor(test_clusters_credit$cluster)

preds_credit <- predict(model_credit, new_data = test_credit)
accuracy_credit <- mean(preds_credit$.pred == credit_split$test$default)

cat("Credit Risk Model Accuracy:", round(accuracy_credit * 100, 1), "%\n")

