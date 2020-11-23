#' Run a linear regression analysis of two chosen variables in a dataset, check normality, then print resulting graph to screen.
#' 
#' @param variable_1: response variable (y-axis/dependent)
#' @param varible_2: numeric predictor variable (x-axis/independent)
#' @return qqline graph printed to screen
#' @importFrom assertthat assert_that
#' @importFrom stats lm
#' @importFrom stats qqnorm
#' @importFrom stats qqline
#' @export


linear_regression_full <- function(variable_1, variable_2) {
  assertthat::is.scalar(variable_2)
  model_fit <- lm(variable_1 ~ variable_2)
  summary <- summary(model_fit)
  augmented_fit <- broom::augment(model_fit)
  qqnorm <- qqnorm(augmented_fit$.resid)
  qqline <- qqline(augmented_fit$.resid, col = "red")
  return(qqline)
}