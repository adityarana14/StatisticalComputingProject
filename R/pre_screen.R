auto_pre_screen_predictors <- function(X, y, k=10) {
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
    
    if (is.numeric(predictor) && is_y_numeric) {
      # Continuous predictor with continuous response - Pearson Correlation
      test = cor.test(predictor, y)
      test_results$p_value[i] = test$p.value
      test_results$method[i] = "Pearson Correlation"
    } else if ((is.factor(predictor) || is.character(predictor)) && is_y_numeric) {
      # Categorical predictor with continuous response - ANOVA
      test = aov(y ~ predictor)
      test_results$p_value[i] = summary(test)[[1]]$`Pr(>F)`[1]
      test_results$method[i] = "ANOVA"
    } else if ((is.factor(predictor) || is.character(predictor)) && !is_y_numeric) {
      # Categorical predictor with categorical response - Chi-squared Test
      test = chisq.test(table(predictor, y))
      test_results$p_value[i] = test$p.value
      test_results$method[i] = "Chi-squared Test"
    } else if (is.numeric(predictor) && !is_y_numeric) {
      # Continuous predictor with binary response - T-test
      test = t.test(predictor ~ y)
      test_results$p_value[i] = test$p.value
      test_results$method[i] = "T-test"
    }
  }
  
  # Sort results by p-value (lower p-values indicate stronger associations)
  sorted_results = test_results[order(test_results$p_value), ]
  
  # Return the top K predictors with their p-values and method used
  return(sorted_results[1:k, c("predictor", "p_value", "method")])
}
