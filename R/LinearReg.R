
#' Auto Linear Regression Model with Optional Bagging
#'
#' The autoLinearRegression function automates linear regression modeling on a given dataset X
#' and response variable y. It checks for binary responses and suggests logistic regression if
#' applicable. The function handles scenarios where the number of predictors exceeds the number
#' of observations by selecting the most significant predictors. It then estimates coefficients
#' using ordinary least squares regression (lm). If the bagging option is enabled, it performs
#' bagging by repeatedly sampling with replacement, calculates standard errors, z-values, and p-values
#' for each coefficient. The function returns a summary including the linear model object,
#' coefficients, and fitted values, offering a streamlined approach to linear regression analysis.
#'
#' @param X A matrix of predictors.
#' @param y A vector representing the response variable, which can be binary or continuous.
#' @param bagging A logical indicating whether bagging should be used.
#'        Defaults to FALSE.
#' @param B An integer specifying the number of bootstrap samples to use if bagging is TRUE.
#'        Defaults to 100.
#' @return A list containing the following components:
#'         \itemize{
#'           \item{lm}{The fitted lm model object.}
#'           \item{coefficients}{A matrix containing estimates, standard errors, z-values, and p-values for each coefficient.}
#'           \item{fitted.values}{The fitted values using the fitted model.}
#' }
#' @examples
#' \dontrun{
#'   # Generate synthetic data
#'   set.seed(123)
#'   n <- 100
#'   p <- 5
#'   X <- matrix(rnorm(n * p), n, p)
#'   y <- 1 + X[,1] - 2 * X[,2] + rnorm(n)
#'   colnames(X) <- paste0("Var", 1:ncol(X))
#'
#'   # Use the function without bagging
#'   result_without_bagging <- autoLinearRegression(X = X, y = y)
#'   print(result_without_bagging$lm)
#'
#'   # Use the function with bagging
#'   result_with_bagging <- autoLinearRegression(X = X, y = y, bagging = TRUE, B = 50)
#'   print(head(result_with_bagging$fitted.values))  # Print first 5 fitted values
#'   print(result_with_bagging$coefficients)  # Print coefficients
#' }
#'
#'
#' @examples
#' \dontrun{
#'   set.seed(42)  # For reproducibility
#'   # Number of samples
#'   num_samples <- 100
#'
#'   # Create continuous predictors
#'   continuous_predictors <- data.frame(
#'     Age = rnorm(num_samples, mean = 30, sd = 10),  # Normally distributed
#'     Salary = rnorm(num_samples, mean = 50000, sd = 15000),  # Normally distributed
#'     Height = rnorm(num_samples, mean = 170, sd = 10)  # Normally distributed
#'   )
#'
#'   # Create discrete predictors
#'   discrete_predictors <- data.frame(
#'     Children = rpois(num_samples, lambda = 2),  # Poisson distribution
#'     Siblings = sample(0:5, num_samples, replace = TRUE)  # Random integers
#'   )
#'
#'   # Create binary predictors
#'   binary_predictors <- data.frame(
#'     OwnsCar = rbinom(num_samples, 1, 0.5),  # 0 or 1 with equal probability
#'     Gender = factor(sample(c("Male", "Female"), num_samples, replace = TRUE)),  # Factor with two levels
#'     Married = sample(c("Yes", "No"), num_samples, replace = TRUE)  # Factor with two levels
#'   )
#'
#'   # Combine all predictors into one data frame
#'   mixed_data <- cbind(continuous_predictors, discrete_predictors, binary_predictors)
#'
#'   # Check the first few rows of the dataset
#'   head(mixed_data)
#'   dim(mixed_data)
#'
#'   mixed_data$Response_Continuous <- 10000 + 100 * mixed_data$Salary + rnorm(nrow(mixed_data), mean = 0, sd = 5000)
#'   mixed_data$Response_Binary <- ifelse(mixed_data$Age > 30, 1, 0)
#'
#'   b=mixed_data[,-ncol(mixed_data)]
#'   v=mixed_data$Response_Binary
#'   res = autoLinearRegression(mixed_data, mixed_data$Response_Continuous, bagging = TRUE)
#' }


autoLinearRegression <- function(X, y, bagging = FALSE, B = 100) {

  missing_data <- sum(is.na(X)) + sum(is.na(y))
  if (missing_data > 0) {
    cat("Missing values detected:", missing_data, "\n")
    na_indices <- which(is.na(y) | rowSums(is.na(X)) > 0)
    X <- X[-na_indices, , drop = FALSE]
    y <- y[-na_indices]
    cat("Missing values removed:", length(na_indices), "\n")
  }

  if (length(unique(y)) == 2) {
    warning("The response variable is binary, should apply logistic regression. Use the function \"logr()\".")
  }

  df <- data.frame(y, X)

  n <- dim(X)[1]    # number of observations
  p <- dim(X)[2]    # number of parameters

  if (n < p) {

    warning("The number of predictors is greater than the number of observations. Insignificant variables are elimated.")

    cat("The number of predictors is greater than the number of observations. Insignificant variables are eliminated.\n")
    k <- as.integer(readline(prompt = "Enter the number of top predictors to select: "))

    while (is.na(k) || k >= p) {

      cat("The number of top predictors must be less than the number of predictors.\n")
      k <- as.integer(readline(prompt = "Enter the number of top predictors to select: "))
    }

    res <- autoPreScreenPredictors(X, y, k)
    res <- res$predictor
    df <- df[, c("y", res), drop = FALSE]

  }

  est <- coef(lm(y ~ (.), data = df))

  if (bagging) {
    est.b <- matrix(NA, nrow = length(est), ncol = B)  # Initialize est.b with correct dimensions
    for (i in 1:B) {
      idx <- sample(1:n, n, replace = TRUE)
      df.b <- df[idx, ]
      model <- lm(y ~ ., data = df.b)
      est.b[, i] <- coef(model)[names(model$coefficients) %in% colnames(df)]
    }

    sd.err <- apply(est.b, 1, sd)

    z <- est / sd.err
    pvals <- 2 * pt(abs(z), df = Inf, lower.tail = FALSE)

    coefficients <- cbind(est, sd.err, z, pvals)
    colnames(coefficients) <- c("Estimate", "Std. Error", "z value", "p-value")
    fitted.values <- predict(lm(y ~ (.), data = df), newdata = df)

    r <- list(lm = lm(y ~ (.), data = df), coefficients = coefficients, fitted.values = fitted.values)
  } else {
    f <- lm(y ~ (.), data = df)
    coefficients <- summary(f)$coefficients
    fitted.values <- f$fitted.values

    r <- list(lm = f, coefficients = coefficients, fitted.values = fitted.values)
  }

  return(r)
}


