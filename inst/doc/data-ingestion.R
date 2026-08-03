## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  message = FALSE,
  warning = FALSE
)

# Each backend demo is guarded on its reader package, which lives in
# Suggests -- the vignette has to build without them
has_sqlite <- requireNamespace("DBI", quietly = TRUE) &&
  requireNamespace("RSQLite", quietly = TRUE)

## ----setup--------------------------------------------------------------------
library(tidylearn)
library(dplyr)

## ----dispatcher-concept, eval = FALSE-----------------------------------------
# # Format is auto-detected from the file extension
# data <- tl_read("sales.csv")
# data <- tl_read("results.xlsx", sheet = "Q1")
# data <- tl_read("experiment.parquet")
# data <- tl_read("config.json")
# data <- tl_read("model_data.rds")
# 
# # Override format detection when the extension is ambiguous
# data <- tl_read("export.txt", format = "tsv")

## ----print-demo---------------------------------------------------------------
tmp <- tempfile(fileext = ".csv")
write.csv(mtcars, tmp, row.names = FALSE)

data <- tl_read(tmp, .quiet = TRUE)
data

## ----cleanup-1, include = FALSE-----------------------------------------------
unlink(tmp)

## ----csv-demo-----------------------------------------------------------------
# Create example files
tmp_csv <- tempfile(fileext = ".csv")
tmp_tsv <- tempfile(fileext = ".tsv")
write.csv(iris, tmp_csv, row.names = FALSE)
write.table(iris, tmp_tsv, sep = "\t", row.names = FALSE)

csv_data <- tl_read_csv(tmp_csv)
tsv_data <- tl_read_tsv(tmp_tsv)
nrow(csv_data)

## ----cleanup-csv, include = FALSE---------------------------------------------
unlink(c(tmp_csv, tmp_tsv))

## ----excel-demo, eval = requireNamespace("readxl", quietly = TRUE)------------
library(readxl)

path <- readxl_example("datasets.xlsx")
excel_data <- tl_read_excel(path, sheet = "mtcars")
head(excel_data, 3)

## ----parquet-demo, eval = requireNamespace("nanoparquet", quietly = TRUE)-----
library(nanoparquet)

tmp_pq <- tempfile(fileext = ".parquet")
write_parquet(iris, tmp_pq)

pq_data <- tl_read_parquet(tmp_pq)
nrow(pq_data)

## ----cleanup-pq, include = FALSE, eval = requireNamespace("nanoparquet", quietly = TRUE)----
unlink(tmp_pq)

## ----json-demo, eval = requireNamespace("jsonlite", quietly = TRUE)-----------
library(jsonlite)

tmp_json <- tempfile(fileext = ".json")
write_json(mtcars[1:5, ], tmp_json)

json_data <- tl_read_json(tmp_json)
json_data

## ----cleanup-json, include = FALSE, eval = requireNamespace("jsonlite", quietly = TRUE)----
unlink(tmp_json)

## ----rds-demo-----------------------------------------------------------------
tmp_rds <- tempfile(fileext = ".rds")
saveRDS(iris, tmp_rds)

rds_data <- tl_read_rds(tmp_rds)
nrow(rds_data)

## ----rdata-demo---------------------------------------------------------------
tmp_rdata <- tempfile(fileext = ".rdata")
my_data <- mtcars
save(my_data, file = tmp_rdata)

# Name is auto-detected when there is a single data frame
rdata_data <- tl_read_rdata(tmp_rdata)
nrow(rdata_data)

## ----cleanup-rds, include = FALSE---------------------------------------------
unlink(c(tmp_rds, tmp_rdata))

## ----sqlite-demo, eval = has_sqlite-------------------------------------------
library(DBI)
library(RSQLite)

# Create an example database
tmp_db <- tempfile(fileext = ".sqlite")
conn <- dbConnect(SQLite(), tmp_db)
dbWriteTable(conn, "iris_tbl", iris)
dbDisconnect(conn)

# Read with tl_read_sqlite
db_data <- tl_read_sqlite(
  tmp_db,
  "SELECT * FROM iris_tbl WHERE Species = 'setosa'"
)
nrow(db_data)

## ----cleanup-sqlite, include = FALSE, eval = has_sqlite-----------------------
unlink(tmp_db)

## ----db-demo, eval = has_sqlite-----------------------------------------------
conn <- dbConnect(SQLite(), ":memory:")
dbWriteTable(conn, "mtcars_tbl", mtcars)

sql <- "SELECT mpg, wt, hp FROM mtcars_tbl WHERE mpg > 20"
db_result <- tl_read_db(conn, sql)
db_result

dbDisconnect(conn)

## ----remote-db, eval = FALSE--------------------------------------------------
# # PostgreSQL
# pg_data <- tl_read_postgres(
#   dsn = "localhost",
#   query = "SELECT * FROM sales WHERE year = 2025",
#   dbname = "analytics",
#   user = "myuser",
#   password = "mypass"
# )
# 
# # MySQL / MariaDB # nolint: commented_code_linter.
# mysql_data <- tl_read_mysql(
#   dsn = "mysql://user:pass@host:3306/mydb",
#   query = "SELECT * FROM customers LIMIT 1000"
# )
# 
# # BigQuery
# bq_data <- tl_read_bigquery(
#   project = "my-gcp-project",
#   query = "SELECT * FROM `dataset.table` LIMIT 1000"
# )

## ----s3, eval = FALSE---------------------------------------------------------
# data <- tl_read_s3("s3://my-bucket/data/sales_2025.csv")
# data <- tl_read_s3("s3://my-bucket/data/results.parquet", region = "eu-west-1")

## ----github, eval = FALSE-----------------------------------------------------
# # Read a CSV from a public GitHub repository
# data <- tl_read_github("tidyverse/dplyr",
#   path = "data-raw/starwars.csv", ref = "main"
# )

## ----kaggle, eval = FALSE-----------------------------------------------------
# data <- tl_read_kaggle("zillow/zecon", file = "Zip_time_series.csv")
# data <- tl_read_kaggle("titanic", file = "train.csv", type = "competition")

## ----multi-path---------------------------------------------------------------
dir <- tempdir()
write.csv(iris[1:50, ], file.path(dir, "batch1.csv"), row.names = FALSE)
write.csv(iris[51:100, ], file.path(dir, "batch2.csv"), row.names = FALSE)

paths <- file.path(dir, c("batch1.csv", "batch2.csv"))
combined <- tl_read(paths, .quiet = TRUE)
table(combined$source_file)

## ----cleanup-multi, include = FALSE-------------------------------------------
unlink(file.path(dir, c("batch1.csv", "batch2.csv")))

## ----dir-demo-----------------------------------------------------------------
dir <- tempfile(pattern = "tl_vignette_")
dir.create(dir)
write.csv(iris[1:50, ], file.path(dir, "jan.csv"), row.names = FALSE)
write.csv(iris[51:100, ], file.path(dir, "feb.csv"), row.names = FALSE)
write.csv(iris[101:150, ], file.path(dir, "mar.csv"), row.names = FALSE)

# Read all CSVs from the directory
all_data <- tl_read_dir(dir, format = "csv", .quiet = TRUE)
nrow(all_data)
table(all_data$source_file)

## ----dir-pattern--------------------------------------------------------------
# Filter with a regex pattern
subset <- tl_read_dir(dir, pattern = "^(jan|feb)", .quiet = TRUE)
nrow(subset)

## ----cleanup-dir, include = FALSE---------------------------------------------
unlink(dir, recursive = TRUE)

## ----dir-dispatch, eval = FALSE-----------------------------------------------
# data <- tl_read("data/monthly_exports/")

## ----zip-demo-----------------------------------------------------------------
# Create an example zip
dir <- tempfile(pattern = "tl_zip_src_")
dir.create(dir)
write.csv(iris, file.path(dir, "iris.csv"), row.names = FALSE)
zip_path <- tempfile(fileext = ".zip")
# -j stores the file without its directory path, so no setwd() is needed
utils::zip(zip_path, file.path(dir, "iris.csv"), flags = "-j9X")

zip_data <- tl_read_zip(zip_path, .quiet = TRUE)
nrow(zip_data)
attr(zip_data, "tl_format")

## ----cleanup-zip, include = FALSE---------------------------------------------
unlink(c(dir, zip_path), recursive = TRUE)

## ----zip-dispatch, eval = FALSE-----------------------------------------------
# data <- tl_read("download.zip")
# data <- tl_read("download.zip", file = "train.csv")

## ----class-demo---------------------------------------------------------------
tmp <- tempfile(fileext = ".csv")
write.csv(mtcars, tmp, row.names = FALSE)
data <- tl_read(tmp, .quiet = TRUE)

# Check metadata
attr(data, "tl_format")

# Works with dplyr
data %>%
  filter(mpg > 20) %>%
  select(mpg, wt, hp) %>%
  head(3)

## ----cleanup-class, include = FALSE-------------------------------------------
unlink(tmp)

## ----pipeline-----------------------------------------------------------------
# 1. Ingest
tmp <- tempfile(fileext = ".csv")
write.csv(iris, tmp, row.names = FALSE)
data <- tl_read(tmp, .quiet = TRUE)

# CSV files lose factor information, so convert character columns as needed
data <- data %>% mutate(Species = as.factor(Species))

# 2. Split
split <- tl_split(data, prop = 0.7, stratify = "Species", seed = 42)

# 3. Model
model <- tl_model(split$train, Species ~ ., method = "forest")

# 4. Evaluate
eval_result <- tl_evaluate(model, new_data = split$test)
eval_result

## ----cleanup-pipeline, include = FALSE----------------------------------------
unlink(tmp)

