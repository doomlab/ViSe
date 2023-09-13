#' Convert d to correlation coefficient
#'
#' This function allows you to convert d to Pearson's
#' correlation coefficient
#'
#' @param d the effect size to convert
#' @return correlation coefficient
#'
#' @keywords effect size, cohen's d, convert, correlation
#'
#' @export
#' @examples
#'
d_to_r <- function (d) {

  if (missing(d)){
    stop("Be sure to include d.")
  }

 return(d / sqrt((d^2+4)))

}

#' @rdname d_to_r
#' @export
