#' Pre-Screening for Top Predictors
#'
#' This function pre-screens a set of predictors to find the top `k` most informative ones in terms of their relationship with a given response variable. It applies different statistical methods depending on the types of predictors and the response.
#'
#' @param X A data frame containing the predictor variables.
#' @param y A vector representing the response variable. It can be continuous or categorical.
#' @param k An integer specifying the number of top predictors to return. Default is 10.
#' @return A data frame containing:
#' \itemize{
#'   \item \code{predictor}: The name of the predictor variable.
#'   \item \code{p_value}: The p-value indicating the strength of the relationship between the predictor and the response.
#'   \item \code{method}: The statistical test used to evaluate the relationship.
#' }
#'
#' @details
#' The function applies the following statistical tests based on the type of predictors and response:
#' \itemize{
#'   \item If both the predictor and response are continuous, it uses Pearson Correlation.
#'   \item If the predictor is binary and the response is continuous, or vice versa, it uses the T-test.
#'   \item If the predictor is categorical with more than two levels and the response is continuous, it uses ANOVA.
#'   \item If both the predictor and response are categorical, it uses the Chi-squared test.
#' }
#'
#' Before applying these tests, the function checks for and omits rows with missing values, issuing a warning if null values are found.
#' If `k` is greater than the number of predictors in `X`, it adjusts `k` to the number of columns in `X`, issuing a warning.
#'
#' @examples
#' # Create a data frame with different types of predictors and a continuous response
#' set.seed(123)
#' X <- data.frame(
#'   Continuous1 = rnorm(100, 50, 10),
#'   Continuous2 = rnorm(100, 100, 15),
#'   Binary1 = sample(c("GroupA", "GroupB"), 100, replace = TRUE),
#'   Binary2 = rbinom(100, 1, 0.5),
#'   Categorical = sample(c("Red", "Green", "Blue"), 100, replace = TRUE)
#' )
#' y <- rnorm(100, 75, 20)  # continuous response
#'
#' # Get top 3 predictors for a continuous response
#' pre_screen_predictors(X, y, k = 3)
#'
#' # Create a binary response and get top 3 predictors
#' y_binary <- sample(c("Yes", "No"), 100, replace = TRUE)  # binary response
#' pre_screen_predictors(X, y_binary, k = 3)
#'
#' @seealso \code{cor.test}, \code{t.test}, \code{aov}, \code{chisq.test}
#'
#' @export

autoPreScreenPredictors <- function(X, y, k = 10) {
  # Ensure k is a valid number
  if (k > ncol(X)) {
    warning(paste("k is greater than the number of columns in X. Adjusting k to", ncol(X)))
    k <- ncol(X)
  }
  
  # Check for NA values and give a warning if found
  if (any(!complete.cases(X, y))) {
    warning("Null values found in the dataset. Omitting rows with missing values.")
  }
  
  # Data pre-processing: Handle NA by omitting rows with NA values
  complete_cases <- complete.cases(X, y)
  X <- X[complete_cases, ]
  y <- y[complete_cases]
  
  if (nrow(X) == 0 || length(y) == 0) {
    stop("Data became empty after removing missing values. Please check your data.")
  }
  
  # Create a data frame to store the test results
  test_results <- data.frame(
    predictor = colnames(X),
    p_value = NA,
    method = "",
    stringsAsFactors = FALSE
  )
  
  # Determine the type of response variable
  is_y_numeric = is.numeric(y)
  
  # Loop through each predictor and perform the appropriate test
  for (i in seq_along(X)) {
    predictor = X[[i]]
    predictor_name = colnames(X)[i]
    
    if (is.numeric(predictor) && (is.factor(y) || 
                                       length(unique(y)) == 2)) {
      # Binary predictor with continuous response
      test = t.test(predictor ~ y)
      test_results$p_value[i] = test$p.value
      test_results$method[i] = "T-test"
    } else if (is.factor(predictor) || is.character(predictor)) {
      if (is_y_numeric) {
        # Categorical predictor with continuous response - ANOVA
        test = aov(y ~ predictor)
        test_results$p_value[i] = summary(test)[[1]]$`Pr(>F)`[1]
        test_results$method[i] = "ANOVA"
      } else {
        # Categorical predictor with categorical response - Chi-squared Test
        test = chisq.test(table(predictor, y))
        test_results$p_value[i] = test$p.value
        test_results$method[i] = "Chi-squared Test"
      }
    } else if ((is.numeric(predictor) && !is_y_numeric) || 
               (is_y_numeric && length(unique(predictor)) == 2)) {
      # T-test - ensure the response or predictor is binary
      if (is.numeric(y) && length(unique(y)) == 2) {
        # Continuous predictor with binary response
        test = t.test(predictor ~ y)
        test_results$p_value[i] = test$p.value
        test_results$method[i] = "T-test"
      } 
      else if (is.numeric(predictor) && is_y_numeric) {
        # Continuous predictor with continuous response - Pearson Correlation
        test = cor.test(predictor, y)
        test_results$p_value[i] = test$p.value
        test_results$method[i] = "Pearson Correlation"
      }
    }
  }
  
  # Sort results by p-value (lower p-values indicate stronger associations)
  sorted_results = test_results[order(test_results$p_value), ]
  
  # Return the top K predictors with their p-values and method used
  return(sorted_results[1:k, c("predictor", "p_value", "method")])
}
