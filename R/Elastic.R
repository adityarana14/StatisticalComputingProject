Elastic_net <- function(X, y, alpha = NULL, lambda = NULL, nfolds = 10, family = NULL) {
  
  # Check data integrity
  #if (!is.matrix(X)) {
   # stop("X must be a matrix.")
  #}
  #if (!is.vector(y) && !is.factor(y)) {
  #  stop("y must be a vector or factor.")
  #}
  if (any(is.na(X))) {
    stop("X contains missing values. Please handle missing data before proceeding.")
  }
  if (any(is.na(y))) {
    stop("y contains missing values. Please handle missing data before proceeding.")
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
    alph_lamb <- data.frame(matrix(ncol = 3, nrow = 0))
    alphavec = seq(0, 1, 0.1)
    
    for (i in 1:length(alphavec)) {
      fit <- cv.glmnet(X, y, alpha = alphavec[i], nfolds = 10, family = family)
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
  
  # Fit the Elastic Net model
  model.elastic <- glmnet(X, y, family = family, alpha = alpha, lambda = lambda)
  
  # Display non-zero coefficients in the final model
  fitCoefs <- coef(model.elastic)
  terms <- which(fitCoefs != 0)
  cat("\nThe predictors retained in the final model are:\n", rownames(fitCoefs)[terms], "\n")
  
  return(model.elastic)
}



set.seed(123)

# Generate sample data
X <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)  # 100 samples, 10 predictors
y <- sample(0:1, 100, replace = TRUE)

tryCatch({
  Elastic_net(X, y, alpha = 0.5)
}, error = function(e) {
  cat("Error occurred:", e$message, "\n")
})

