## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(simpleEnsembleGroup09)

## -----------------------------------------------------------------------------
data <- read.csv("https://raw.githubusercontent.com/adityarana14/MainData/main/Price.csv", header = TRUE)
y <- data$price  # Select the response variable column
X <- data[, !names(data) %in% c("price")]
result <- random_forest(y, X)
model <- result$model
accuracy <- result$accuracy
other_metrics <- result$other_metrics
print(model)
print(accuracy)
print(other_metrics)

