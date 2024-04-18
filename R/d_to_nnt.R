#' Convert d to Number Needed to Treat
#'
#' This function calculates the number needed to treat
#' from continuous measures (Cohen's d) using Kraemer and Kupfer
#' (2006) formula.
#'
#' @param d the effect size
#' @return nnt values from d
#'
#' @references
#'
#' Kraemer H.C., Kupfer D.J. (2006) Size of treatment effects and their
#' importance to clinical research and practice.
#' \emph{Biolological Psychiatry, 59}, 990–996.
#' https://doi.org/10.1016/j.biopsych.2005.09.014
#'
#' @examples
#'
#' d_to_nnt(d = .25)
#'
#' @rdname d_to_nnt
#' @export
#'

d_to_nnt <- function (d = NULL)
{
  if(is.null(d)){
    stop("You must include d to translate into NNT.")
  }

  return(1/((2 * pnorm(d/sqrt(2)) - 1)))

}


