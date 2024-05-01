#' Linear regression
#'
#' linr is a function to run linear regression for a data set with a continuous
#' dependent variable.
#'
#' @param y Independent variable, a continuous vector
#' @param X Dependent variables, an n by p matrix
#' @param select_top_k Logical indicating whether to select the top k predictors
#' @return A list containing the lm object, coefficients, and fitted values
#' @export
autoLinearRegression <- function(y, X, select_top_k = FALSE) {
  if (length(unique(y)) == 2) {
    warning("The response variable is binary, should apply logistic regression. Use the function \"logr()\".")
  }

  data_df <- data.frame(y, X)

  num_observations <- dim(X)[1]    # number of observations
  num_parameters <- dim(X)[2]    # number of parameters
  if (num_observations < num_parameters) {
    warning("The number of predictors is greater than the number of observations. Insignificant variables are eliminated.")
    if (select_top_k) {
      k <- as.integer(readline(prompt = "Enter the number of top predictors to select: "))
      data_df <- data_df[, forward_selection(y, X, vmax = k, model = "lm", shrinkage = TRUE)]
    } else {
      data_df <- data_df[, forward_selection(y, X, vmax = num_observations - 1, model = "lm", shrinkage = TRUE)]
    }
  }

  lm_model <- lm(y ~ ., data = data_df)
  coefficients <- summary(lm_model)$coefficients
  fitted_values <- lm_model$fitted.values

  result <- list(lm = lm_model, coefficients = coefficients, fitted_values = fitted_values)

  return(result)
}
