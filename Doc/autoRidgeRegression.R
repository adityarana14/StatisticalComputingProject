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
  n <- 100
  p <- 5
  X <- matrix(rnorm(n * p), n, p)
  y <- 1 + X[,1] - 2 * X[,2] + rnorm(n)
  colnames(X) <- paste0("Var", 1:ncol(X))

## -----------------------------------------------------------------------------
# Use the function without bagging
  result_without_bagging <- autoRidgeRegression(y = y, X = X, family = 'continuous')
  print(result_without_bagging$model)

## -----------------------------------------------------------------------------
 # Use the function with bagging
  result_with_bagging <- autoRidgeRegression(y = y, X = X, family = 'continuous', bagging = TRUE, n_bags = 50)
  print(head(result_with_bagging$predictions))  # Print first 5 predictions
  print(result_with_bagging$importance_scores)

