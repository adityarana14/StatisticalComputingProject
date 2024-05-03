#' Auto Logistic Regression Model with Optional Bagging
#'
#' The autoLogisticRegression function automates logistic regression modeling on a given dataset X
#' and response variable y. It checks for binary responses and suggests logistic regression if
#' applicable. The function handles scenarios where the number of predictors exceeds the number
#' of observations by selecting the most significant predictors. It then estimates coefficients
#' using logistic regression (glm). If the bagging option is enabled, it performs
#' bagging by repeatedly sampling with replacement, calculates standard errors, z-values, and p-values
#' for each coefficient. The function returns a summary including the logistic model object,
#' coefficients, and fitted values, offering a streamlined approach to logistic regression analysis.
#'
#' @param X A matrix of predictors.
#' @param y A vector representing the response variable, which must be binary.
#' @param bagging A logical indicating whether bagging should be used.
#'        Defaults to FALSE.
#' @param B An integer specifying the number of bootstrap samples to use if bagging is TRUE.
#'        Defaults to 100.
#' @return A list containing the following components:
#'         \itemize{
#'           \item{glm}{The fitted glm model object.}
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
#'   y <- rbinom(n, 1, 0.5)
#'   colnames(X) <- paste0("Var", 1:ncol(X))
#'
#'   # Use the function without bagging
#'   result_without_bagging <- autoLogisticRegression(X = X, y = y)
#'   print(result_without_bagging$glm)
#'
#'   # Use the function with bagging
#'   result_with_bagging <- autoLogisticRegression(X = X, y = y, bagging = TRUE, B = 50)
#'   print(head(result_with_bagging$fitted.values))  # Print first 5 fitted values
#'   print(result_with_bagging$coefficients)  # Print coefficients
#' }
autoLogisticRegression <- function(X, y, bagging = FALSE, B = 100) {

  missing_data <- sum(is.na(X)) + sum(is.na(y))
  if (missing_data > 0) {
    cat("Missing values detected:", missing_data, "\n")
    na_indices <- which(is.na(y) | rowSums(is.na(X)) > 0)
    X <- X[-na_indices, , drop = FALSE]
    y <- y[-na_indices]
    cat("Missing values removed:", length(na_indices), "\n")
  }

  if (length(unique(y)) != 2) {
    stop("The response variable must be binary for logistic regression.")
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
    res <- get_top_k_predictors(X, y, k)$Top_Predictors$Predictor
    df <- df[, c("y", res)]
  }

  est <- coef(glm(y ~ (.), data = df, family = "binomial"))

  if (bagging) {
    est.b <- matrix(NA, nrow = p+1, ncol = B)
    for (i in 1:B) {
      idx <- sample(1:n, n, replace = TRUE)
      df.b <- df[idx, ]
      est.b[, i] <- coef(glm(y ~ (.), data = df.b, family = "binomial"))
    }

    sd.err <- apply(est.b, 1, sd)

    z <- est / sd.err
    pvals <- 2 * pt(abs(z), df = Inf, lower.tail = FALSE)

    coefficients <- cbind(est, sd.err, z, pvals)
    colnames(coefficients) <- c("Estimate", "Std. Error", "z value", "p-value")
    fitted.values <- predict(glm(y ~ (.), data = df, family = "binomial"), newdata = df, type = "response")

    r <- list(glm = glm(y ~ (.), data = df, family = "binomial"), coefficients = coefficients, fitted.values = fitted.values)
  } else {
    f <- glm(y ~ (.), data = df, family = "binomial")
    coefficients <- summary(f)$coefficients
    fitted.values <- predict(f, newdata = df, type = "response")

    r <- list(glm = f, coefficients = coefficients, fitted.values = fitted.values)
  }

  return(r)
}


  set.seed(123)
  n <- 100
  p <- 5
  X <- matrix(rnorm(n * p), n, p)
  y <- rbinom(n, 1, 0.5)
  colnames(X) <- paste0("Var", 1:ncol(X))

  # Use the function without bagging
  result_without_bagging <- autoLogisticRegression(X = X, y = y)
  print(result_without_bagging$glm)

  # Use the function with bagging
  result_with_bagging <- autoLogisticRegression(X = X, y = y, bagging = TRUE, B = 50)
  print(head(result_with_bagging$fitted.values))  # Print first 5 fitted values
  print(result_with_bagging$coefficients)  # Print coefficients
