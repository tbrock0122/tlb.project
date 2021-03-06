#' Run a linear regression analysis of two chosen variables in a dataset, then print summary to screen.
#' 
#' @param variable_1: response variable (y-axis/dependent)
#' @param varible_2: numeric predictor variable (x-axis/independent)
#' @return table of summary statistics printed to screen
#' @importFrom assertthat assert_that
#' @importFrom stats lm
#' @export


linear_regression <- function(variable_1, variable_2) {
  assertthat::is.scalar(variable_2)
  model_fit <- lm(variable_1 ~ variable_2)
  summary <- summary(model_fit)
  return(summary)
}