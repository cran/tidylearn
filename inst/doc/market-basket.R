## ----include = FALSE----------------------------------------------------------
# arules supplies the algorithm and the example data; arulesViz backs the
# graph and grouped-matrix plots. Both are in Suggests, so the whole
# vignette is conditional on them.
has_arules <- requireNamespace("arules", quietly = TRUE)
has_arulesviz <- requireNamespace("arulesViz", quietly = TRUE)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  message = FALSE,
  warning = FALSE,
  eval = has_arules
)

## ----echo = FALSE, results = "asis", eval = TRUE------------------------------
if (!has_arules) {
  cat(
    "> **Note:** the arules package is not installed, so the examples",
    "below are shown without output.\n"
  )
}

## ----setup--------------------------------------------------------------------
library(tidylearn)
library(dplyr)

## -----------------------------------------------------------------------------
data("Groceries", package = "arules")
Groceries

## -----------------------------------------------------------------------------
rules <- tidy_apriori(
  Groceries,
  support = 0.001,      # at least ~10 of the 9,835 transactions
  confidence = 0.5,     # right-hand side follows at least half the time
  minlen = 2            # rules with something on both sides
)

## -----------------------------------------------------------------------------
print(rules)

## -----------------------------------------------------------------------------
rules$rules_tbl

## -----------------------------------------------------------------------------
names(rules)

## -----------------------------------------------------------------------------
grid <- expand.grid(
  support = c(0.001, 0.005, 0.01),
  confidence = c(0.3, 0.5, 0.7)
)

grid$n_rules <- mapply(function(s, c) {
  tidy_apriori(Groceries, support = s, confidence = c)$n_rules
}, grid$support, grid$confidence)

grid

## -----------------------------------------------------------------------------
inspect_rules(rules, by = "lift", n = 10)

## -----------------------------------------------------------------------------
summary_stats <- summarize_rules(rules)
summary_stats$n_rules

## -----------------------------------------------------------------------------
data.frame(
  measure = c("support", "confidence", "lift"),
  min = c(summary_stats$support$min, summary_stats$confidence$min,
          summary_stats$lift$min),
  median = c(summary_stats$support$median, summary_stats$confidence$median,
             summary_stats$lift$median),
  max = c(summary_stats$support$max, summary_stats$confidence$max,
          summary_stats$lift$max)
)

## -----------------------------------------------------------------------------
rules$rules_tbl %>%
  filter(lift > 5, count >= 15) %>%
  arrange(desc(confidence)) %>%
  select(lhs, rhs, confidence, lift, count)

## -----------------------------------------------------------------------------
# What predicts a purchase of whole milk?
filter_rules_by_item(rules, "whole milk", where = "rhs") %>%
  arrange(desc(lift)) %>%
  select(lhs, confidence, lift, count) %>%
  head(5)

## -----------------------------------------------------------------------------
# And what does a basket containing yoghurt lead to?
filter_rules_by_item(rules, "yogurt", where = "lhs") %>%
  arrange(desc(lift)) %>%
  select(lhs, rhs, confidence, lift) %>%
  head(5)

## -----------------------------------------------------------------------------
find_related_items(rules, "yogurt", min_lift = 1.5, top_n = 5) %>%
  select(lhs, rhs, confidence, lift)

## -----------------------------------------------------------------------------
recommend_products(
  rules,
  basket = c("flour", "baking powder"),
  top_n = 5
)

## -----------------------------------------------------------------------------
recommend_products(rules, basket = c("whole milk", "butter"))

## -----------------------------------------------------------------------------
broad <- tidy_apriori(
  Groceries,
  support = 0.001, confidence = 0.15, minlen = 2
)

broad$n_rules

## -----------------------------------------------------------------------------
recommend_products(
  broad,
  basket = c("whole milk", "butter"),
  min_confidence = 0.15,
  top_n = 5
)

## -----------------------------------------------------------------------------
visualize_rules(rules, method = "scatter", top_n = 200)

## ----eval = has_arules && has_arulesviz---------------------------------------
visualize_rules(rules, method = "graph", top_n = 20)

## -----------------------------------------------------------------------------
receipts <- data.frame(
  basket_id = c(1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5),
  item = c("bread", "butter", "jam",
           "bread", "butter",
           "bread", "butter", "jam",
           "milk", "bread",
           "bread", "butter", "jam"),
  stringsAsFactors = TRUE
)

baskets <- split(as.character(receipts$item), receipts$basket_id)
transactions <- as(baskets, "transactions")
transactions

## -----------------------------------------------------------------------------
small_rules <- tidy_apriori(
  transactions,
  support = 0.4, confidence = 0.6, minlen = 2
)

small_rules$rules_tbl %>%
  arrange(desc(lift)) %>%
  select(lhs, rhs, support, confidence, lift)

