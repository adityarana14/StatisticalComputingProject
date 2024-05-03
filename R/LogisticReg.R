#' Auto Logistic Regression Model with Optional Bagging
#'
#' The autoLogisticRegression function automates logistic regression modeling on a given dataset X
#' and binary response variable y. The function handles scenarios where the number of predictors exceeds
#' the number of observations by selecting the most significant predictors. It then estimates coefficients
#' using logistic regression (glm with binomial family). If the bagging option is enabled, it performs
#' bagging by repeatedly sampling with replacement, calculates standard errors, z-values, and p-values
#' for each coefficient. The function returns a summary including the logistic model object,
#' coefficients, and fitted probabilities, offering a streamlined approach to logistic regression analysis.
#'
#' @param X A matrix of predictors.
#' @param y A vector representing the binary response variable.
#' @param bagging A logical indicating whether bagging should be used.
#'        Defaults to FALSE.
#' @param B An integer specifying the number of bootstrap samples to use if bagging is TRUE.
#'        Defaults to 100.
#' @return A list containing the following components:
#'         \itemize{
#'           \item{glm}{The fitted glm model object.}
#'           \item{coefficients}{A matrix containing estimates, standard errors, z-values, and p-values for each coefficient.}
#'           \item{fitted.values}{The fitted probabilities using the fitted model.}
#' }

autoLogisticRegression <- function(X, y, bagging = FALSE, B = 100) {
  # Handling NA values
  missing_data <- sum(is.na(X)) + sum(is.na(y))
  if (missing_data > 0) {
    cat("Missing values detected:", missing_data, "\n")
    na_indices <- which(is.na(y) | rowSums(is.na(X)) > 0)
    X <- X[-na_indices, , drop = FALSE]
    y <- y[-na_indices]
    cat("Missing values removed:", length(na_indices), "\n")
  }

  unique_y <- unique(y)
  if (length(unique_y) != 2) {
    stop("Not good data: y should have exactly two unique values.")
  }

  if (!is.numeric(y)) {
    # Explicitly map the two unique levels to 0 and 1
    levels <- sort(unique_y)  # Sort to ensure consistent mapping
    y <- factor(y, levels = levels)
    y <- as.integer(y) - 1  # Convert factor to integer and adjust to 0 and 1
    cat(sprintf("Response variable y converted to numeric: %s as 0, %s as 1.\n", levels[1], levels[2]))
  }

  print(y)
  df <- data.frame(y, X)

  # Check predictor count versus observation count
  n <- dim(X)[1]  # number of observations
  p <- dim(X)[2]  # number of parameters

  if (n < p) {
    warning("The number of predictors is greater than the number of observations. Some variables are eliminated.")
    cat("Reducing the number of predictors due to more predictors than observations.\n")
    k <- min(p - 1, as.integer(readline(prompt = "Enter the number of top predictors to select: ")))
    while (is.na(k) || k >= p) {
      cat("The number of top predictors must be less than the number of predictors.\n")
      k <- as.integer(readline(prompt = "Enter the number of top predictors to select: "))
    }
    res <- autoPreScreenPredictors(X, y, k)
    res <- res$predictor
    df <- df[, c("y", res), drop = FALSE]
  }
  # Fitting the logistic regression model
  fit <- glm(y ~ ., data = df, family = binomial)

  if (bagging) {
    coefficient_names <- names(fit$coefficients)
    est.b <- matrix(NA, nrow = length(fit$coefficients), ncol = B)
    for (i in 1:B) {
      idx <- sample(1:n, n, replace = TRUE)
      df.b <- df[idx, ]
      model <- glm(y ~ ., data = df.b, family = binomial)
      est.b[, i] <- coef(model)[coefficient_names]
    }

    sd.err <- apply(est.b, 1, sd)

    z <- fit$coefficients / sd.err
    pvals <- 2 * pnorm(abs(z), lower.tail = FALSE)

    coefficients <- cbind(Estimate = fit$coefficients, `Std. Error` = sd.err, `z value` = z, `p-value` = pvals)
    fitted.values <- predict(fit, type = "response")

    r <- list(glm = fit, coefficients = coefficients, fitted.values = fitted.values)
  } else {
    coefficients <- cbind(Estimate = coef(summary(fit)), pvals = coef(summary(fit))[,"Pr(>|z|)"])
    fitted.values <- predict(fit, type = "response")

    r <- list(glm = fit, coefficients = coefficients, fitted.values = fitted.values)
  }

  return(r)
}


set.seed(42)  # For reproducibility
# Number of samples
num_samples <- 100

# Create continuous predictors
continuous_predictors <- data.frame(
  Age = rnorm(num_samples, mean = 30, sd = 10),  # Normally distributed
  Salary = rnorm(num_samples, mean = 50000, sd = 15000),  # Normally distributed
  Height = rnorm(num_samples, mean = 170, sd = 10)  # Normally distributed
)

# Create discrete predictors
discrete_predictors <- data.frame(
  Children = rpois(num_samples, lambda = 2),  # Poisson distribution
  Siblings = sample(0:5, num_samples, replace = TRUE)  # Random integers
)

# Create binary predictors
binary_predictors <- data.frame(
  OwnsCar = rbinom(num_samples, 1, 0.5),  # 0 or 1 with equal probability
  Gender = factor(sample(c("Male", "Female"), num_samples, replace = TRUE)),  # Factor with two levels
  Married = sample(c("Yes", "No"), num_samples, replace = TRUE)  # Factor with two levels
)

# Combine all predictors into one data frame
mixed_data <- cbind(continuous_predictors, discrete_predictors, binary_predictors)

# Check the first few rows of the dataset
head(mixed_data)
dim(mixed_data)

Response_Continuous <- 10000 + 100 * mixed_data$Salary + rnorm(nrow(mixed_data), mean = 0, sd = 5000)
Response_Binary <- ifelse(mixed_data$Age > 30, 1, 0)

res_continuous_bagging = autoLogisticRegression(mixed_data, Response_Binary, bagging = TRUE)
res_continous = autoLogisticRegression(mixed_data, Response_Binary, bagging = FALSE)



