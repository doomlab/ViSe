#' Convert d to Cohen's f
#'
#' This function allows you to convert d to
#' Cohen's f and \eqn{f^2} statistics.
#'
#' @param d the effect size to convert
#' @return Both Cohen's f and f^2 statistics
#'
#' \item{f}{d values translated into f}
#' \item{f2}{d values translated into \eqn{f^2}}
#'
#' @keywords effect size, cohen's d, cohen's f, f squared,
#' convert
#'
#' @examples
#'
#' d_to_f2(.25)
#'
#' @rdname d_to_f2
#' @export

#'
d_to_f2 <- function (d) {

  if (missing(d)){
    stop("Be sure to include d.")
  }

  f <- d / 2
  f2 <- f^2

  return(list("f" = f,
              "f2" = f2))
}

