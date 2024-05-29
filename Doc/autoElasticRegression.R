## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(simpleEnsembleGroup09)
if (!requireNamespace("glmnet", quietly = TRUE)) {
    # If not, install glmnet
    install.packages("glmnet")
    message("The 'glmnet' package was not installed. It has been installed now.")
  }
  library(glmnet)

## -----------------------------------------------------------------------------
set.seed(123)
X <- matrix(rnorm(100 * 4), ncol = 4)
y <- sample(0:1, 100, replace = TRUE)

# Running the function without bagging
result <- autoElasticRegression(X, y)

# Running the function with bagging to improve estimates
result_bagged <- autoElasticRegression(X, y, bagging = TRUE, n_bags = 50)

## -----------------------------------------------------------------------------

# Use the function with bagging

# Example with 50 bootstrapped samples

result_with_bagging <- autoElasticRegression(y = y, X = X,  bagging = TRUE, n_bags = 50)
# Display final predictions from all bagged models
print("Final predictions from bagged models:")
print(head(result_with_bagging$predictions))# Print first 5 predictions

# Display importance scores
print("Importance scores for predictors:") # Print importance scores
print(result_with_bagging$importance_scores)

