#' Ensemble Model for Multiple Predictive Models
#'
#' This function allows for the ensemble of multiple predictive models, enabling the combination
#' of their predictions for potentially improved accuracy and robustness. It supports a range of
#' models defined within the package and can combine their predictions using specified methods.
#'
#' @param y A vector representing the response variable, which can be binary or continuous.
#' @param X A matrix of predictors.
#' @param modelTypes A character vector specifying the types of models to fit.
#'        Examples include "autoRidgeRegression", "autoLassoRegression", "Elastic_net", etc.
#' @param combine A character string indicating the method to combine the model predictions.
#'        Current supported method is "average".
#'
#' @return A list containing the models' predictions and the combined predictions.
#'
#' @examples
#' \dontrun{
#'   data <- data.frame(
#'     response = rnorm(100),
#'     predictor1 = rnorm(100),
#'     predictor2 = rnorm(100),
#'     predictor3 = rnorm(100)
#'   )
#'   y <- data$response
#'   X <- data[, c("predictor1", "predictor2", "predictor3")]
#'   ensemble_results <- ensembleModel(y = y, X = X,
#'     modelTypes = c("autoRidgeRegression", "autoLassoRegression", "Elastic_net"),
#'     combine = "average"
#'   )
#'   print(ensemble_results$combined_predictions)
#' }
ensembleModel <- function(X, y, modelTypes, combine = "average") {
  # List to store model predictions
  predictions_list <- list()

  # Fit each specified model
  for (model_type in modelTypes) {
    if (model_type == "autoRidgeRegression") {
      predictions <- autoRidgeRegression(X, y)$predictions
    } else if (model_type == "autoLassoRegression") {
      predictions <- autoLassoRegression(X, y)$predictions
    } else if (model_type == "Elastic_net") {
      predictions <- autoElasticRegression(X, y)$predictions
    } else if (model_type == "autoLinearRegression") {
      predictions <- autoLinearRegression(X, y)$predictions
    } else if (model_type == "autoLogisticRegression") {
      predictions <- autoLogisticRegression(X, y)$fitted.values
    }
    else if (model_type == "dectree") {
      predictions <- dectree(X, y)$predictions
    }
    predictions_list[[model_type]] <- predictions
  }

  # Combine predictions
  combined_predictions <- NULL
  if (combine == "average") {
    combined_predictions <- Reduce("+", predictions_list) / length(predictions_list)
  }

  return(list(models = predictions_list, combined_predictions = combined_predictions))
}

y<-data$y
X<- data[, names(data)!="y"]
y
X
ensembleModel(X,y,modelTypes = c("autoRidgeRegression", "autoLassoRegression", "Elastic_net"), combine = "average" )
