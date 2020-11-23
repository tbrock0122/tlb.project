#' Run an ANOVA for chosen variables in a dataset, then print summary to screen.
#' 
#' @param variable_1: response variable (y-axis/dependent)
#' @param varible_2: categorical predictor variable (x-axis/independent)
#' @return table of summary statistics printed to screen
#' @importFrom assertthat assert_that
#' @importFrom stats aov
#' @export


simple_anova <- function(variable_1, variable_2) {
  assertthat::is.scalar(variable_1)
  model_fit <- lm(variable_1 ~ variable_2)
  anova_model_fit <- aov(model_fit)
  summary <- summary(anova_model_fit)
  return(summary)
}