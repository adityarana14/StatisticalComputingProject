## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(simpleEnsembleGroup09)

## -----------------------------------------------------------------------------
# Sample data for ensemble
set.seed(123)
data <- data.frame(
  response = rnorm(100),
  predictor1 = rnorm(100),
  predictor2 = rnorm(100),
  predictor3 = rnorm(100)
)
y <- data$response
X <- data[, c("predictor1", "predictor2", "predictor3")]

# Perform an ensemble with Ridge, Lasso, and Elastic Net
ensemble_results <- ensembleModel(
  X = X,
  y = y,
  modelTypes = c("autoRidgeRegression", "autoLassoRegression", "Elastic_net"),
  combine = "average"
)

# Print combined predictions
print("Combined predictions from ensemble:")
print(ensemble_results$combined_predictions)


