#' Calculate hindcast performance of a model for a single year
#'
#' @param data (data.frame) The data to use
#' @param model (character) The model formula
#' @param year (numeric) Which year to forecast
#' @param round_method (function) A function to apply to each prediction
#'   (numeric)
#'
#' @return (data.frame) Model performance for a single year
calculate_model_performance_single <- function(data, model, year, round_method = floor) {
  training_data <- data[data$year < year,]
  training_model <- stats::lm(stats::formula(model), training_data)

  new_data <- data[data$year == year,]
  prediction <- stats::predict(training_model, newdata = new_data, se.fit = TRUE)
  prediction_fit <- round_method(prediction$fit[[1]])
  prediction_interval <- prediction_fit + c(-2, 2) * stats::qnorm(0.975) *
    prediction$se.fit[[1]]

  actual <- new_data$mdj
  in_interval <- actual >= round_method(prediction_interval[1]) &&
    actual <= round_method(prediction_interval[2])

  data.frame(
    "formula" = model,
    "year" = year,
    "predicted" = (prediction_fit),
    "observed" = actual,
    "diff" = actual - (prediction_fit),
    "predict_se" = prediction$se.fit[[1]],
    "in_interval" = in_interval,
    "int_lower" = prediction_interval[1],
    "int_upper" = prediction_interval[2],
    "int_width" = prediction_interval[2] -
      prediction_interval[1])
}

#' Calculate hindcast performance for a model
#'
#' @param data (data.frame) The data to use
#' @param model (character) The model formula
#' @param years (numeric) Which years to hindcast
#'
#' @return (data.frame) Summarized model performance
calculate_model_performance <- function(data, model, years) {
  model_performance <- do.call(rbind, lapply(years, function(year) {
    calculate_model_performance_single(data, model, year)
  }))

  data.frame(
    model = model,
    "mape" = round(mean(abs(model_performance$predicted - model_performance$observed)), 2),
    "mapesd" = round(stats::sd(abs(model_performance$predicted - model_performance$observed)), 2),
    "intwidth" = round(mean(model_performance$int_width), 2),
    "propin" = round(sum(model_performance$in_interval) / length(model_performance$in_interval), 2),
    "maxabsresid" = max(abs(model_performance$predicted - model_performance$observed)),
    "meanbias" = round(mean(model_performance$predicted - model_performance$observed), 2)
  )
}

#' Create a model performance table
#'
#' @param data (data.frame) The data to use
#' @param models (character) The model formula
#' @param years (numeric) Which years to hindcast
#'
#' @return (data.frame) A model performance table
#'
#' @export
create_performance_table <- function(data, models, years) {
  do.call(rbind, lapply(models,
                        calculate_model_performance,
                        data = data,
                        years = years))
}
