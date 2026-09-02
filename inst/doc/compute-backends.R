## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(tidylearn)

## ----check-gpu, eval = FALSE--------------------------------------------------
# tl_check_gpu()

## ----gpu-routing, eval = FALSE------------------------------------------------
# # Routed to CUDA when a capable backend is present
# model <- tl_model(data, y ~ ., method = "xgboost", compute = "gpu")
# 
# # Warns and falls back to CPU: randomForest has no GPU implementation
# model <- tl_model(data, y ~ ., method = "forest", compute = "gpu")
# 
# # Let tidylearn decide per call
# model <- tl_model(data, y ~ ., method = "xgboost", compute = "auto")

## ----advisor------------------------------------------------------------------
advice <- tl_compute_advisor(
  "xgboost", iris, Species ~ .,
  hyperparams = list(nrounds = 1000)
)

advice$recommendation

## ----advisor-print------------------------------------------------------------
advice

## ----consent------------------------------------------------------------------
# For the session
tl_cloud_consent()

# Revoke early
tl_cloud_consent(FALSE)

## ----endpoint, eval = FALSE---------------------------------------------------
# Sys.setenv(
#   TIDYLEARN_MODAL_ENDPOINT = "https://you--tidylearn-fit.modal.run"
# )

## ----hosts--------------------------------------------------------------------
tl_cloud_allowed_hosts()

## ----allow-host---------------------------------------------------------------
tl_cloud_allow_host("fits.example.com")
tl_cloud_allowed_hosts()

## ----allow-host-reset, include = FALSE----------------------------------------
tl_cloud_allow_host(NULL)

## ----cost, eval = FALSE-------------------------------------------------------
# model <- tl_model(data, y ~ ., method = "xgboost", compute = "cloud",
#                   confirm_upload = TRUE, max_cost = 5)

## ----jobs---------------------------------------------------------------------
tl_cloud_jobs()

## ----threat-model, eval = FALSE-----------------------------------------------
# file.show(
#   system.file("security/threat-model.md", package = "tidylearn")
# )

