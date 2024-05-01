get_top_k_predictors <- function(X, y, k = 10, method = NULL) {

  # Check if y has missing values or is empty
  if (length(y) == 0 || any(is.na(y))) {
    stop("y must contain valid non-missing values and consistent length.")
  }
  
  # Default method based on response type if not provided
  if (is.null(method)) {
    if (is.factor(y) && length(unique(y)) == 2) {
      method <- "t-test"
    }  else {
      method <- "linear regression"
    }
  }
  
  # Check if the method is valid
  if (!method %in% c("t-test", "pearson", "linear regression")) {
    stop("Invalid method. Choose from 't-test', 'pearson', or 'linear regression'.")
  }
  
  # Check if k is larger than the total number of predictors
  if (k > ncol(X)) {
    warning("k is larger than the total number of predictors. Using all available predictors.")
    k = ncol(X)
  }
  
  # Initialize result variables
  top_k_indices <- NULL
  top_k_scores <- NULL
  
  if (method == "t-test") {
    if (length(unique(y)) != 2) {
      stop("The response variable must have exactly two unique levels for t-test.")
    }
    if (!is.matrix(X)) {
      X <- model.matrix(~ . - 1, data = X)  # Convert to a model matrix, excluding the intercept
      warning("Converted X in to model matrix to perform T-test")
      }
    
    # Calculate p-values with t-tests
    p_values <- sapply(1:ncol(X), function(i) {
      t.test(X[, i] ~ y)$p.value  # Perform t-test for valid numeric data
    })
    
    top_k_indices <- order(p_values, na.last = TRUE)[1:k]
    top_k_scores <- p_values[top_k_indices]
    
  } else if (method == "pearson") {
    # Calculate Pearson correlation
    pearson_correlations <- sapply(1:ncol(X), function(i) {
      abs(cor(X[, i], y))  # Pearson correlation for numeric data
    })
    
    top_k_indices <- order(-pearson_correlations)[1:k]
    top_k_scores <- pearson_correlations[top_k_indices]
    
  } else if (method == "linear regression") {
    # Calculate p-values for each predictor with linear regression
    p_values <- sapply(1:ncol(X), function(i) {
      summary(lm(y ~ X[, i]))$coefficients[2, 4]  # Get the p-value for the predictor
    })
    
    top_k_indices <- order(p_values)[1:k]
    top_k_scores = p_values[top_k_indices]
  }
  
  # Create a data frame with the top k predictors
  top_k_results <- data.frame(
    Predictor = colnames(X)[top_k_indices],
    P_values = top_k_scores
  )
  
  # Return the results and the method as a list
  list(
    Top_Predictors = top_k_results,
    Method = method
  )
}

m$Top_Predictors$Predictor
###########################################################################

library(glmnet)

Elastic_net <- function(X, y, alpha = NULL, lambda = NULL, nfolds = 10, family = NULL) {
  
  # Check data integrity
  if (!is.vector(y) && !is.factor(y)) {
    stop("y must be a vector or factor.")
  }
  if (any(is.na(X))) {
    stop("X contains missing values. Please handle missing data before proceeding.")
  }
  if (any(is.na(y))) {
    stop("y contains missing values. Please handle missing data before proceeding.")
  }
  
  # Check if X has more columns than the length of y
  if (ncol(X) > length(y)) {
    response <- readline(prompt = "Number of predictors exceeds the number of samples. Would you like to use a reduced number of predictors? (yes/no): ")
    
    if (tolower(response) == "yes") {
      k <- as.integer(readline(prompt = "Enter the number of top predictors to use: "))
      if (is.na(k) || k <= 0) {
        stop("Invalid input. The number of top predictors must be a positive integer.")
      }
      
      # Ask the user to choose a method for selecting top predictors
      method <- readline(prompt = "Choose a method for selecting top predictors (t-test/pearson/linear regression): ")
      
      if (!method %in% c("t-test", "pearson", "linear regression")) {
        stop("Invalid method. Choose from 't-test', 'pearson', or 'linear regression'.")
      }
      
      # Get the top k predictors using the chosen method
      top_k_results <- get_top_k_predictors(X, y, k = k, method = method)
      X <- X[, top_k_results$Top_Predictors$Predictor]  # Use only the top k predictors
    } else {
      warning("Proceeding with all predictors, which may lead to overfitting or multicollinearity.")
    }
  }
  
  # Determine family if not provided
  if (is.null(family)) {
    warning("No 'family' provided. Using default based on y.")
    if (length(unique(y)) == 2) {
      family <- "binomial"
    } else {
      family <- "gaussian"
    }
    cat("family used as it is not provided:", family, "\n")
  }
  
  # Determine alpha and lambda if not provided
  if (is.null(alpha) || is.null(lambda)) {
    alpha_lambda <- data.frame(matrix(ncol = 3, nrow = 0))
    alpha_vec = seq(0, 1, 0.1)
    
    for (alpha_value in alpha_vec) {
      fit <- cv.glmnet(X, y, alpha = alpha_value, nfolds = 10, family = family)
      min_cvm <- min(fit$cvm)
      lambda_opt <- fit$lambda[which(fit$cvm == min_cvm)]
      
      alpha_lambda <- rbind(alpha_lambda, c(alpha_value, lambda_opt, min_cvm))
    }
    
    colnames(alpha_lambda) <- c("alpha", "lambda", "cvm")
    
    optimal_alpha <- alpha_lambda[which.min(alpha_lambda$cvm), 1]
    optimal_lambda <- alpha_lambda[which.min(alpha_lambda$cvm), 2]
    
    if (is.null(alpha)) {
      warning("No 'alpha' provided. Using cross-validation to determine optimal alpha.")
      cat("Optimal alpha used:", optimal_alpha, "\n")
      alpha = optimal_alpha
    }
    
    if (is.null(lambda)) {
      warning("No 'lambda' provided. Using cross-validation to determine optimal lambda.")
      cat("Optimal lambda used:", optimal_lambda, "\n")
      lambda = optimal_lambda
    }
  }
  
  # Fit the Elastic Net model
  model_elastic <- glmnet(X, y, family = family, alpha = alpha, lambda = lambda)
  
  # Display non-zero coefficients in the final model
  coefficients <- coef(model_elastic)
  terms <- rownames(coefficients)[which(coefficients != 0)]
  cat("The predictors retained in the final model are:\n", terms, "\n")
  
  return(model_elastic)
}

