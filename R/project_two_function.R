#' Run a linear regression analysis of two chosen variables in a dataset, then print summary to screen.
#' 
#' @param variable_1: response variable (y-axis/dependent)
#' @param varible_2: predictor variable (x-axis/independent)
#' @return table of summary statistics printed to screen 


linear_regression <- function(variable_1, variable_2) {
  model_fit <- lm(variable_1 ~ variable_2)
  summary <- summary(model_fit)
  return(summary)
}




#' Run a linear regression analysis of two chosen variables in a dataset, then print resulting graph to screen.
#' 
#' @param variable_1: response variable (y-axis/dependent)
#' @param varible_2: predictor variable (x-axis/independent)
#' @return graph printed to screen


linear_regression_full <- function(variable_1, variable_2) {
  model_fit <- lm(variable_1 ~ variable_2)
  summary <- summary(model_fit)
  augmented_fit <- broom::augment(model_fit)
  qqnorm <- qqnorm(augmented_fit$.resid)
  qqline <- qqline(augmented_fit$.resid, col = "red")
  return(qqline)
}

