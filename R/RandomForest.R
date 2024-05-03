


library(randomForest)

# Function to check if input is valid
check_input <- function(y, X) {

  # Check if number of rows in X is equal to length of y
  if (nrow(X) != length(y)) {
    stop("Invalid input: Number of rows in X must be equal to length of y.")
  }

  # Check for missing values in y and X
  na_y <- sum(is.na(y))
  na_X <- sum(is.na(X))
  if (na_y > 0) {
    warning(paste("Missing values detected in y:", na_y, "rows removed."))
    na_idx <- which(is.na(y))
    y <- y[-na_idx]
    X <- X[-na_idx,]
  }

  if (na_X > 0) {
    # Remove rows with any NA values
    na_rows <- which(rowSums(is.na(X)) > 0)
    if (length(na_rows) > 0) {
      warning(paste("Rows with NA values detected:", length(na_rows), "rows removed."))
      X <- X[-na_rows,]
      y <- y[-na_rows]
    }
  }

  return(list(y = y, X = X))
}

random_forest_random_search <- function(y, X, n_iter = 10, test_size = 0.2) {
  best_accuracy <- 0
  best_params <- NULL

  set.seed(123)  # Set seed for reproducibility outside the loop

  for (i in 1:n_iter) {
    ntree <- sample(100:500, 1)
    mtry <- sample(1:ncol(X), 1)
    nodesize <- sample(1:10, 1)

    split_idx <- sample(1:nrow(X), size = floor((1 - test_size) * nrow(X)))
    X_train <- X[split_idx, ]
    y_train <- y[split_idx]
    X_test <- X[-split_idx, ]
    y_test <- y[-split_idx]

    rf <- randomForest(as.factor(y_train) ~ ., data = as.data.frame(X_train), ntree = ntree, mtry = mtry, nodesize = nodesize)
    accuracy <- sum(predict(rf, newdata = as.data.frame(X_test)) == y_test) / length(y_test)
    print(accuracy)
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_params <- list(ntree = ntree, mtry = mtry, nodesize = nodesize)
    }
  }

  return(list(best_params = best_params, best_accuracy = best_accuracy))
}


random_forest <- function(y, X, ntree = NULL, mtry = NULL, nodesize = NULL) {
  input <- check_input(y, X)
  y <- input$y
  X <- input$X

  if (is.null(ntree) || is.null(mtry) || is.null(nodesize)) {
    # If hyperparameters are not provided, use random search to find them
    tuning_result <- random_forest_random_search(y, X)
    ntree <- tuning_result$best_params$ntree
    mtry <- tuning_result$best_params$mtry
    nodesize <- tuning_result$best_params$nodesize
  }


  if (length(unique(y)) == 2) {
    rf <- randomForest(as.factor(y) ~ ., data = as.data.frame(X), ntree = ntree, mtry = mtry, nodesize = nodesize)
    accuracy <- sum(predict(rf, newdata = as.data.frame(X)) == y) / length(y)
    other_metrics <- NULL
  } else if (length(unique(y)) <= 5) {
    y_binary <- ifelse(y == mean(y), 0, 1)
    rf <- randomForest(as.factor(y_binary) ~ ., data = as.data.frame(X), ntree = ntree, mtry = mtry, nodesize = nodesize)
    accuracy <- sum(predict(rf, newdata = as.data.frame(X)) == y_binary) / length(y_binary)
    other_metrics <- NULL
    warning("The response variable has been converted to binary due to few unique values.")
  } else {
    rf <- randomForest(y ~ ., data = as.data.frame(X), ntree = ntree, mtry = mtry, nodesize = nodesize)
    accuracy <- NULL
    mae <- mean(abs(predict(rf) - y))
    mse <- mean((predict(rf) - y)^2)
    rmse <- sqrt(mean((predict(rf) - y)^2))
    r_squared <- 1 - sum((y - predict(rf))^2) / sum((y - mean(y))^2)
    other_metrics <- list(MAE = mae, MSE = mse, RMSE = rmse, R_squared = r_squared)
  }

  return(list(model = rf, accuracy = accuracy, other_metrics = other_metrics))
}

# Example usage
set.seed(123)
n <- 100
p <- 5
X <- matrix(rnorm(n*p), nrow = n, ncol = p)
y <- rnorm(n)

X[sample(1:length(X), 10)] <- NA
y[sample(1:length(y), 5)] <- NA
y
X

# example usage

data = read.csv("Price.csv")
head(data)
y <- data$price  # Select the response variable column
X <- data[, !names(data) %in% c("price")]

y
X


result <- random_forest(y, X)
model <- result$model
accuracy <- result$accuracy
other_metrics <- result$other_metrics

print(model)
print(accuracy)
print(other_metrics)







dat = read.csv("Price.csv")
y <- dat$price

# Define X as all columns except 'Group'
X <- dat[, names(dat)!="price"]
autoElasticRegression(X,y, bagging = TRUE)


dat <- read.delim('https://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/leukemiaDataSet.txt',
                  header=T,sep='\t')
y <- dat$Group
y

# Define X as all columns except 'Group'
X <- dat[, names(dat)!="Group"]
autoElasticRegression(X,y, bagging = FALSE)
