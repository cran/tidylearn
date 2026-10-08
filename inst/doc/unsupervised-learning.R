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

# k-means, CLARA and the gap statistic start from random draws. Seeding once
# here makes the whole page reproduce when run from the top.
set.seed(42)

## -----------------------------------------------------------------------------
# Same algorithm, two interfaces
model <- tl_model(iris[, 1:4], method = "kmeans", k = 3)
km <- tidy_kmeans(iris[, 1:4], k = 3)

names(km)

## -----------------------------------------------------------------------------
iris_scaled <- standardize_data(iris[, 1:4])

sapply(iris_scaled, function(x) round(c(mean = mean(x), sd = sd(x)), 3))

## -----------------------------------------------------------------------------
pca <- tidy_pca(iris[, 1:4], scale = TRUE)
names(pca)

## -----------------------------------------------------------------------------
get_pca_variance(pca)

## -----------------------------------------------------------------------------
get_pca_loadings(pca, n_components = 2)

## -----------------------------------------------------------------------------
plot_variance_explained(get_pca_variance(pca), threshold = 0.9)

## -----------------------------------------------------------------------------
tidy_pca_screeplot(pca)

## -----------------------------------------------------------------------------
scored <- augment_pca(pca, iris, n_components = 2)
head(scored, 3)

## -----------------------------------------------------------------------------
ggplot(scored, aes(x = PC1, y = PC2, color = Species)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "PCA of Iris",
    x = paste0("PC1 (",
               round(get_pca_variance(pca)$prop_variance[1] * 100, 1), "%)"),
    y = paste0("PC2 (",
               round(get_pca_variance(pca)$prop_variance[2] * 100, 1), "%)")
  ) +
  theme_minimal()

## -----------------------------------------------------------------------------
tidy_pca_biplot(pca, color_by = iris$Species)

## -----------------------------------------------------------------------------
opt <- optimal_clusters(iris[, 1:4], max_k = 8)
names(opt)

## -----------------------------------------------------------------------------
opt$wss

## -----------------------------------------------------------------------------
opt$silhouette

## -----------------------------------------------------------------------------
c(silhouette = attr(opt$silhouette, "optimal_k"),
  gap = opt$gap$recommended_k)

## -----------------------------------------------------------------------------
plot_elbow(opt$wss, suggested_k = 3)

## -----------------------------------------------------------------------------
plot_gap_stat(opt$gap)

## -----------------------------------------------------------------------------
calc_wss(iris[, 1:4], max_k = 6)

## -----------------------------------------------------------------------------
km <- tidy_kmeans(iris[, 1:4], k = 3)
km$centers

## -----------------------------------------------------------------------------
km$clusters

## -----------------------------------------------------------------------------
iris_clustered <- augment_kmeans(km, iris)
table(Cluster = iris_clustered$cluster, Species = iris_clustered$Species)

## -----------------------------------------------------------------------------
plot_cluster_sizes(km$clusters$cluster)

## -----------------------------------------------------------------------------
plot_clusters(iris_clustered, cluster_col = "cluster",
              x_col = "Petal.Length", y_col = "Petal.Width")

## -----------------------------------------------------------------------------
dist_mat <- tidy_dist(iris[, 1:4])
sil <- tidy_silhouette(km$clusters$cluster, dist_mat)

sil$avg_width

## -----------------------------------------------------------------------------
sil$cluster_avg

## -----------------------------------------------------------------------------
plot_silhouette(sil)

## -----------------------------------------------------------------------------
calc_validation_metrics(km$clusters$cluster, iris[, 1:4], dist_mat)

## -----------------------------------------------------------------------------
pam_result <- tidy_pam(iris[, 1:4], k = 3)
pam_result$medoids

## -----------------------------------------------------------------------------
pam_result$silhouette_avg

## -----------------------------------------------------------------------------
table(Cluster = augment_pam(pam_result, iris)$cluster, Species = iris$Species)

## -----------------------------------------------------------------------------
large_data <- iris[rep(seq_len(nrow(iris)), 10), 1:4]
clara_result <- tidy_clara(large_data, k = 3, samples = 5)

table(clara_result$clusters$cluster)

## -----------------------------------------------------------------------------
hc <- tidy_hclust(iris[, 1:4], method = "average")
plot_dendrogram(hc, k = 3)

## -----------------------------------------------------------------------------
optimal_hclust_k(hc, method = "silhouette", max_k = 8)$optimal_k

## -----------------------------------------------------------------------------
cuts <- tidy_cutree(hc, k = 3)
head(cuts, 3)

## -----------------------------------------------------------------------------
hc_data <- augment_hclust(hc, iris, k = 3)
table(Cluster = hc_data$cluster, Species = hc_data$Species)

## -----------------------------------------------------------------------------
linkages <- c("single", "average", "complete", "ward.D2")

sapply(linkages, function(m) {
  cl <- tidy_cutree(tidy_hclust(iris[, 1:4], method = m), k = 3)$cluster
  max(table(cl))
})

## -----------------------------------------------------------------------------
eps_suggestion <- suggest_eps(iris[, 1:4], minPts = 5)
eps_suggestion$eps

## -----------------------------------------------------------------------------
plot_knn_dist(iris[, 1:4], k = 4)

## -----------------------------------------------------------------------------
db <- tidy_dbscan(iris[, 1:4], eps = eps_suggestion$eps, minPts = 5)

c(clusters = db$n_clusters, noise = db$n_noise)

## -----------------------------------------------------------------------------
db_data <- augment_dbscan(db, iris)
table(Cluster = db_data$cluster, Species = db_data$Species)

## -----------------------------------------------------------------------------
explore_dbscan_params(
  iris[, 1:4],
  eps_values = c(0.4, 0.6, 0.8, 1.0),
  minPts_values = c(4, 5, 10)
)

## -----------------------------------------------------------------------------
mds <- tidy_mds(iris[, 1:4], method = "classical", ndim = 2)
head(mds$config, 3)

## -----------------------------------------------------------------------------
plot_mds(mds, color_by = iris$Species, label_points = FALSE)

## ----error = TRUE-------------------------------------------------------------
try({
tidy_mds(iris[, 1:4], method = "sammon", ndim = 2)
})

## -----------------------------------------------------------------------------
distinct_iris <- iris[!duplicated(iris[, 1:4]), 1:4]
sammon <- tidy_mds(distinct_iris, method = "sammon", ndim = 2)
sammon$stress

## -----------------------------------------------------------------------------
comparison <- compare_clusterings(
  list(
    kmeans = km$clusters$cluster,
    pam = pam_result$clusters$cluster,
    hclust = cuts$cluster,
    dbscan = db$clusters$cluster
  ),
  iris[, 1:4],
  dist_mat
)

comparison

## -----------------------------------------------------------------------------
plot_cluster_comparison(
  iris[, 1:4] |>
    mutate(kmeans = km$clusters$cluster, hclust = cuts$cluster),
  cluster_cols = c("kmeans", "hclust"),
  x_col = "Petal.Length",
  y_col = "Petal.Width"
)

## -----------------------------------------------------------------------------
names(compare_distances(iris[, 1:4]))

## -----------------------------------------------------------------------------
plot_distance_heatmap(dist_mat)

## -----------------------------------------------------------------------------
data_matrix <- standardize_data(iris[, 1:4])

# 1. How many clusters does the data support?
choice <- optimal_clusters(data_matrix, max_k = 8)
k <- attr(choice$silhouette, "optimal_k")
k

## -----------------------------------------------------------------------------
# 2. Cluster at that k
final_km <- tidy_kmeans(data_matrix, k = k)

# 3. Score the result before believing it
final_sil <- tidy_silhouette(final_km$clusters$cluster, tidy_dist(data_matrix))
final_sil$avg_width

## -----------------------------------------------------------------------------
# 4. Attach the assignment and look at it
final_data <- augment_kmeans(final_km, iris)
table(Cluster = final_data$cluster, Species = final_data$Species)

## -----------------------------------------------------------------------------
plot_clusters(final_data, cluster_col = "cluster",
              x_col = "Petal.Length", y_col = "Petal.Width")

