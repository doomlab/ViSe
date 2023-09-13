#' Adjust coefficient for confounders
#'
#' This function calculates the adjusted effect after
#' controlling for confounding effects. You can use d values or
#' standardized regression coefficients.
#'
#' @param effect_xz Effect of x on y given z
#' @param effect_uxz Effect of u on y given x and z
#' @param effect_d Effect size difference of interest
#'
#' @return Adjusted effect size of x on y given u and z
#' @keywords effect size, dependent t-test, cohen's d, paired-sample,
#' repeated measures, correlation
#'
#' @export
#' @examples
#'
#'

adjusted_coef <- function(effect_xz, effect_uxz, effect_d){

  adjusted_effect <- effect_xz - effect_uxz*effect_d

  return(adjusted_effect)
}

#' @rdname adjusted_coef
#' @export
