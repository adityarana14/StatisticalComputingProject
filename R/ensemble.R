#' Ensemble Model for Multiple Predictive Models
#'
#' This function facilitates the ensemble of various predictive models, allowing for the aggregation
#' of their predictions to potentially enhance accuracy and robustness. It supports a diverse array
#' of models within the package and enables their predictions to be combined using specified methods.
#'
#' @param X A matrix of predictors.
#' @param y A vector representing the response variable, which can be binary or continuous.
#' @param modelTypes A character vector specifying the types of models to fit.
#'        Examples include "autoRidgeRegression", "autoLassoRegression", "Elastic_net", "autoLinearRegression",
#'        "autoLogisticRegression", and "random_forest".
#' @param combine A character string indicating the method to combine the model predictions.
#'        Currently, the supported method is "average". Future versions may include other methods such as "median" or "weighted".
#'
#' @return A list containing:
#'         \itemize{
#'           \item{models}{A list of models used in the ensemble, storing each model's predictions.}
#'           \item{combined_predictions}{The combined predictions from all models, according to the specified method.}
#'         }
#'
#' @export
#' @examples
#' data <- data.frame(
#'   response = rnorm(100),
#'   predictor1 = rnorm(100),
#'   predictor2 = rnorm(100),
#'   predictor3 = rnorm(100)
#' )
#' y <- data$response
#' X <- data[, c("predictor1", "predictor2", "predictor3")]
#'
#' # Performing an ensemble with different regression models
#' ensemble_results <- ensembleModel(X = X, y = y,
#'   modelTypes = c("autoRidgeRegression", "autoLassoRegression", "Elastic_net"),
#'   combine = "average"
#' )
#' print(ensemble_results$combined_predictions)
ensembleModel <- function(X, y, modelTypes, combine = "average") {
  # List to store model predictions
  predictions_list <- list()
  
  # Fit each specified model
  for (model_type in modelTypes) {
    if (model_type == "autoRidgeRegression") {
      predictions <- autoRidgeRegression(y, X)$predictions
    } else if (model_type == "autoLassoRegression") {
      predictions <- autoLassoRegression(y, X)$predictions
    } else if (model_type == "Elastic_net") {
      predictions <- autoElasticRegression(y, X)$predictions
    } else if (model_type == "autoLinearRegression") {
      predictions <- autoLinearRegression(y, X)$fitted.values
    } else if (model_type == "autoLogisticRegression") {
      predictions <- autoLogisticRegression(y, X)$fitted.values
    } else if (model_type == "random_forest") {
      predictions <- random_forest(y, X)$predictions
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
