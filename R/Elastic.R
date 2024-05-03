autoElasticRegression <- function(X, y, lambda = NULL, alpha = NULL, family = NULL, bagging = FALSE, n_bags = 50) {

  # if (is.data.frame(X)) {
  #   X <- as.matrix(X)
  # }
  X <- model.matrix(~ . - 1, data = X)
  # Handle missing data in X and y
  missing_data <- sum(is.na(X)) + sum(is.na(y))
  if (missing_data > 0) {
    cat("Missing values detected:", missing_data, "\n")
    na_indices <- which(is.na(y) | rowSums(is.na(X)) > 0)
    X <- X[-na_indices, , drop = FALSE]
    y <- y[-na_indices]
    cat("Missing values removed:", length(na_indices), "\n")
  }

  unique_y <- unique(y)
  if (length(unique_y) == 2) {
    if (!is.numeric(y)) {
      # Explicitly map the two unique levels to 0 and 1
      levels <- sort(unique_y)  # Sort to ensure consistent mapping
      y <- factor(y, levels = levels)
      y <- as.integer(y) - 1  # Convert factor to integer and adjust to 0 and 1
      cat(sprintf("Response variable y converted to numeric: %s as 0, %s as 1.\n", levels[1], levels[2]))
    }
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

  # Initialize importance scores
  importance_scores <- rep(0, ncol(X))

  # Initialize predictions
  predictions <- matrix(0, nrow = nrow(X), ncol = n_bags)

  # Determine alpha and lambda if not provided
  if (is.null(alpha) || is.null(lambda)){
    alph_lamb <- data.frame(matrix(ncol = 3, nrow = 0))
    alphavec = seq(0, 1, 0.1)

    for (i in 1:length(alphavec)) {
      fit <- cv.glmnet(X, y, alpha = alphavec[i], nfolds = 10, family = glmnet_family)
      min_mse <- min(fit$cvm)
      lam <- fit$lambda[which(fit$cvm == min_mse)]
      alph_lamb <- rbind(alph_lamb, c(alphavec[i], lam, min_mse))
    }

    colnames(alph_lamb) <- c("alpha", "lambda", "cvm")

    min_cvm <- min(alph_lamb$cvm)
    opt_alpha <- alph_lamb[which(alph_lamb$cvm == min_cvm), 1]
    opt_lambda <- alph_lamb[which(alph_lamb$cvm == min_cvm), 2]

    if (is.null(alpha)) {
      warning("No 'alpha' provided. Using cross-validation to determine optimal alpha.")
      cat("Optimal alpha used:", opt_alpha, "\n")
      alpha = opt_alpha
    }

    if (is.null(lambda)) {
      warning("No 'lambda' provided. Using cross-validation to determine optimal lambda.")
      cat("Optimal lambda used:", opt_lambda, "\n")
      lambda = opt_lambda
    }
  }

  if (bagging) {
    for (i in 1:n_bags) {
      # Sample with replacement
      indices <- sample(1:nrow(X), replace = TRUE)
      X_bag <- X[indices, , drop = FALSE]
      y_bag <- y[indices]

      model <- glmnet(X_bag, y_bag, family = glmnet_family, lambda = lambda, alpha = alpha)

      # Update importance scores
      coef_matrix <- coef(model, s = lambda)
      importance_scores <- importance_scores + (coef_matrix[-1] != 0)  # Exclude intercept

      # Predict using the original X
      predictions[, i] <- predict(model, newx = X, type = "response", s = lambda)
    }
    # Average the predictions across all bags
    final_predictions <- rowMeans(predictions)
    cat("Bagging complete. Averaged predictions from", n_bags, "models.\n")
    # Report importance scores for each variable
    names(importance_scores) <- colnames(X)
    return(list(model = model, type = family, predictions = final_predictions, importance_scores = importance_scores))
  } else {

    model <- glmnet(X, y, family = glmnet_family, lambda = lambda, alpha = alpha)
    final_predictions <- predict(model, newx = X, type = "response", s = lambda)
    return(list(model = model, type = family, predictions = final_predictions))
  }
}






