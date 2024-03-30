#' Visualization for Estimating \eqn{r}
#'
#' This function displays a visualization of effect sizes.
#'
#' @param r a correlation to visualize
#' @return Returns a pretty graph
#'
#' \item{graph}{A graph of the effect size}
#'
#' @keywords effect size estimation visualization ggplot
#' @import ggplot2
#'
#' @examples
#' estimate_r(r = .4)
#'
#' @rdname estimate_r
#' @export


estimate_r <- function (r = NULL) {

  if (is.null(r)){ stop("Please include r.") }

  n <- 100
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y1 <- r*x2+sqrt(1-r*r)*x1

  graph <- ggplot(data = NULL, aes(x = x2, y = y1)) +
    geom_jitter() +
    xlab("Predictor Variable") +
    ylab("Measured Variable") +
    theme_classic()

  return(list(
    "graph" = graph
  ))

}
