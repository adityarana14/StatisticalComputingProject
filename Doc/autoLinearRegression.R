## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(simpleEnsembleGroup09)

## -----------------------------------------------------------------------------
# Set seed for reproducibility
set.seed(123)
n <- 100  # number of observations
p <- 5    # number of predictors
X <- matrix(rnorm(n * p), n, p)
y <- 1 + X[,1] - 2 * X[,2] + rnorm(n)
colnames(X) <- paste0("Var", 1:ncol(X))

# Apply the function without bagging
result_without_bagging <- autoLinearRegression(X = X, y = y)
print(result_without_bagging$lm)

## -----------------------------------------------------------------------------
# Apply the function with bagging
result_with_bagging <- autoLinearRegression(X = X, y = y, bagging = TRUE, B = 50)
print(head(result_with_bagging$fitted.values))  # Print first 5 fitted values
print(result_with_bagging$coefficients)  # Print coefficients


