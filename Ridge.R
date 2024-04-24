library(glmnet)

#' Auto Ridge Regression Model with Simplified Family Input
#'
#' This function accepts a simplified family input ('binary' or 'continuous'), detects
#' the type of the response variable accordingly, handles missing values, and 
#' chooses the appropriate ridge regression model. It selects the best lambda 
#' using cross-validation if not provided.
#'
#' @param y response variable, either binary or continuous
#' @param X matrix of predictors
#' @param lambda regularization parameter, optional
#' @param family optional; simplified input 'binary' or 'continuous'
#' @return a list containing the model and its type
autoRidgeRegression <- function(y, X, lambda = NULL, family = NULL) {
  # Handle missing data in X and y
  missing_data <- sum(is.na(X)) + sum(is.na(y))
  if (missing_data > 0) {
    cat("Missing values detected:", missing_data, "\n")
    na_indices <- which(is.na(y) | rowSums(is.na(X)) > 0)
    X <- X[-na_indices, , drop = FALSE]
    y <- y[-na_indices]
    cat("Missing values removed:", length(na_indices), "\n")
  }
  
  # Auto-detect data type based on unique values
  detected_type <- ifelse(length(unique(y)) == 2 && all(unique(as.numeric(y)) %in% c(0, 1)), "binary", "continuous")
  
  # Map 'binary' to 'binomial' and 'continuous' to 'gaussian' for glmnet usage
  glmnet_family <- ifelse(detected_type == "binary", "binomial", "gaussian")
  
  # Verify provided family against detected type
  if (!is.null(family)) {
    if (!tolower(family) %in% c("binary", "continuous")) {
      stop("Invalid family specified. Use 'binary' or 'continuous'.")
    }
    if (family != detected_type) {
      stop("Provided family does not match the detected data type.")
    }
    cat("Provided family and detected type match: ", family, "\n")
  } else {
    family <- detected_type
    cat("Family auto-detected as: ", family, "\n")
  }
  
  # If lambda is not provided, use cross-validation to find the optimal lambda
  if (is.null(lambda)) {
    cv_model <- cv.glmnet(X, y, family = glmnet_family, alpha = 0)
    lambda <- cv_model$lambda.min
    cat("Optimal lambda determined by CV:", lambda, "\n")
  }
  
  # Fit the model using the specified or chosen lambda
  model <- glmnet(X, y, family = glmnet_family, lambda = lambda, alpha = 0)
  
  # Print the model's non-zero coefficients
  fitCoefs <- coef(model)
  terms <- which(fitCoefs != 0)
  cat("\nThe predictors retained in the final model are:\n", names(fitCoefs[terms]), "\n")
  
  return(list(model = model, type = family))
}
