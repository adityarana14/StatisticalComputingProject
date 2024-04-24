get_top_k_predictors <- function(X, y, k = 5) {
  # Ensure X is a matrix
  if (!is.matrix(X)) {
    stop("X must be a matrix.")
  }
  
  # Check if y has missing values or is empty
  if (length(y) == 0 || any(is.na(y))) {
    stop("y must contain valid non-missing values.")
  }
  
  # Determine which method to use
  method <- "pearson"
  if (is.factor(y) && length(unique(y)) == 2) {
    # Binary data
    method <- "t-test"
  } else if (is.factor(y) || is.ordered(y)) {
    # Categorical or ordinal data
    method <- "spearman"
  } 
  cat("method used:",method,"\n")
  top_k_indices <- NULL
  predictor_names <- NULL
  top_k_pvalues <- NULL
  
  if (method == "t-test") {
    # Calculate p-values with t-tests
    p_values <- sapply(1:ncol(X), function(i) {
      t.test(X[, i] ~ y)$p.value
    })
    # Get the top k predictors with the lowest p-values
    top_k_indices <- order(p_values)[1:k]
    top_k_pvalues <- p_values[top_k_indices]
  } else if (method == "spearman") {
    # Calculate Spearman's Rank Correlation for categorical/ordinal data
    abs_spearman_correlations <- sapply(1:ncol(X), function(i) {
      abs(cor(X[, i], as.numeric(y), method = "spearman"))
    })
    top_k_indices <- order(-abs_spearman_correlations)[1:k]
    top_k_pvalues <- abs_spearman_correlations[top_k_indices]
  } else if (method == "pearson") {
    # Calculate Pearson's Correlation for continuous data
    abs_pearson_correlations <- sapply(1:ncol(X), function(i) {
      abs(cor(X[, i], y))
    })
    top_k_indices <- order(-abs_pearson_correlations)[1:k]
    top_k_pvalues <- abs_pearson_correlations[top_k_indices]
  }
  
  # Get the names of the top k predictors
  predictor_names <- colnames(X)[top_k_indices]
  
  # Return the names and p-values of the top k predictors
  result <- data.frame(
    Predictor = predictor_names,
    P_value = top_k_pvalues
  )
  
  return(result)
}




data = read.csv("C:/Users/skristam/Downloads/AMS 580/GreatUnknown.csv")
ncol(data)

X <- data[, -ncol(data)]

# Use the last column as the response variable
y <- data[, ncol(data)]

X <- model.matrix(y ~., data)[, -1]
y <- as.factor(data$y)

get_top_k_predictors(X, y, k = 5)