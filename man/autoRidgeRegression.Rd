% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ridge.R
\name{autoRidgeRegression}
\alias{autoRidgeRegression}
\title{Auto Ridge Regression Model with Optional Bagging}
\usage{
autoRidgeRegression(
  y,
  X,
  lambda = NULL,
  family = NULL,
  bagging = FALSE,
  n_bags = 100
)
}
\arguments{
\item{y}{A vector representing the response variable, which can be binary or continuous.}

\item{X}{A matrix of predictors.}

\item{lambda}{A numeric value representing the regularization parameter.
If NULL, the optimal lambda is chosen via cross-validation.}

\item{family}{A character string specifying the model family.
Accepts 'binary' or 'continuous', which are automatically detected if not provided.}

\item{bagging}{A logical indicating whether bagging should be used.
Defaults to FALSE.}

\item{n_bags}{An integer specifying the number of bootstrap samples to use if bagging is TRUE.
Defaults to 100.}
}
\value{
A list containing the following components:
        \itemize{
          \item{model}{The fitted glmnet model object.}
          \item{type}{The detected or specified family of the response variable.}
          \item{predictions}{Predicted values using the fitted model.}
          \item{importance_scores}{A numeric vector of importance scores for each predictor,
          indicating how often each predictor was selected in the models from the bagging process.}
}
}
\description{
This function performs ridge regression with the ability to handle missing data,
automatically detect the type of the response variable (binary or continuous),
and optionally use bagging to enhance model robustness. Bagging involves fitting
multiple models on bootstrap samples and averaging their predictions.
}
\examples{
\dontrun{
  # Generate synthetic data
  set.seed(123)
  n <- 100
  p <- 5
  X <- matrix(rnorm(n * p), n, p)
  y <- 1 + X[,1] - 2 * X[,2] + rnorm(n)
  colnames(X) <- paste0("Var", 1:ncol(X))

  # Use the function without bagging
  result_without_bagging <- autoRidgeRegression(y = y, X = X, family = 'continuous')
  print(result_without_bagging$model)

  # Use the function with bagging
  result_with_bagging <- autoRidgeRegression(y = y, X = X, family = 'continuous', bagging = TRUE, n_bags = 50)
  print(head(result_with_bagging$predictions))  # Print first 5 predictions
  print(result_with_bagging$importance_scores)  # Print importance scores
}
}
