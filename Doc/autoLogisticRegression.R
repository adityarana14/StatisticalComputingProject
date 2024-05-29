## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(simpleEnsembleGroup09)

## -----------------------------------------------------------------------------
set.seed(123)
n <- 100  # number of observations
p <- 5    # number of predictors
X <- matrix(rnorm(n * p), n, p)
y <- factor(sample(0:1, n, replace = TRUE))
colnames(X) <- paste0("Var", 1:ncol(X))

# Apply the function without bagging
result_without_bagging <- autoLogisticRegression(X = X, y = y)
print(result_without_bagging$glm)

## -----------------------------------------------------------------------------
# Apply the function with bagging
result_with_bagging <- autoLogisticRegression(X = X, y = y, bagging = TRUE, B = 50)
print(head(result_with_bagging$fitted.values))  # Print first 5 fitted values
print(result_with_bagging$coefficients)  # Print coefficients


