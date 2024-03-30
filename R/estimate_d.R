#' Visualization for Estimating \eqn{d_s}
#'
#' This function displays a visualization of effect sizes.
#'
#' @param m1 mean from first group
#' @param m2 mean from second group
#' @param sd1 standard deviation from first group
#' @param sd2 standard deviation from second group
#' @param n1 sample size for first group
#' @param n2 sample size for the second group
#' @param d estimate of the effect size
#' @return Returns a pretty graph
#'
#' \item{d}{effect size}
#' \item{graph}{A graph of the distributions of the effect size}
#'
#' @keywords effect size estimation visualization ggplot
#' @import ggplot2
#'
#' @examples
#' estimate_d(d = .25)
#'
#' estimate_d(m1 = 10, m2 = 8, sd1 = 5, sd2 = 4,
#'  n1 = 100, n2 = 75)
#'
#' @rdname estimate_d
#' @export


estimate_d <- function (m1 = NULL, m2 = NULL,
                        sd1 = NULL, sd2 = NULL,
                        n1 = NULL, n2 = NULL, d = NULL) {

  if (!is.null(d)){

    # make a graph of d if only d defined
    m1 <- 0
    sd1 <- 1
    sd2 <- 1
    n1 <- 100
    n2 <- 100

    spooled <- sqrt( ((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / (n1 + n2 - 2))
    m2 <- (d * spooled) + m1

    graph <- ggplot() +
      stat_function(fun = ~ dnorm(.x, m1, sd1), geom = "area",
                    fill = "#88CCEE", alpha = 0.5, color = "black") +
      stat_function(fun = ~ dnorm(.x, m2, sd2), geom = "area",
                    fill = "#CC6677", alpha = 0.5, color = "black") +
      geom_vline(xintercept = m1, linetype = 3) +
      geom_vline(xintercept = m2, linetype = 3) +
      theme_classic() +
      xlim(-4,4) +
      ylab("Density") +
      xlab("Standardized Mean Scores")

      ylimits <- layer_scales(graph)$y$range$range
      yupper <- (ylimits[2] - ylimits[1])*1.1

      xright <- max(c((m2 + sd2), (m1 + sd1)))

      graph <- graph +
        annotate("segment", x = m1, xend = m2, y = yupper, yend = yupper,
                 colour = "black") +
        annotate("text", x = xright, y = yupper,
                 colour = "black",
                 label = paste0("d = ", format(d, digits = 2, nsmall = 2)))

  } else {

    if (missing(m1)){
      stop("Be sure to include m1 for the first mean.")
    }

    if (missing(m2)){
      stop("Be sure to include m2 for the second mean.")
    }

    if (missing(sd1)){
      stop("Be sure to include sd1 for the first mean.")
    }

    if (missing(sd2)){
      stop("Be sure to include sd2 for the second mean.")
    }

    if (missing(n1)){
      stop("Be sure to include the sample size n1 for the first group.")
    }

    if (missing(n2)){
      stop("Be sure to include the sample size n2 for the second group.")
    }

    spooled <- sqrt( ((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / (n1 + n2 - 2))
    d <- (m1 - m2) / spooled

    graph <- ggplot() +
      stat_function(fun = ~ dnorm(.x, m1, sd1), geom = "area",
                    fill = "#88CCEE", alpha = 0.5, color = "black") +
      stat_function(fun = ~ dnorm(.x, m2, sd2), geom = "area",
                    fill = "#CC6677", alpha = 0.5, color = "black") +
      geom_vline(xintercept = m1, linetype = 3) +
      geom_vline(xintercept = m2, linetype = 3) +
      theme_classic() +
      xlim(abs(min(m1, m2))-3*abs(max(sd1,sd2)),
           abs(min(m1, m2))+3*abs(max(sd1,sd2))) +
      ylab("Density") +
      xlab("Raw Mean Scores")

    ylimits <- layer_scales(graph)$y$range$range
    yupper <- (ylimits[2] - ylimits[1])*1.1

    xright <- max(c((m2 + sd2), (m1 + sd1)))

    graph <- graph +
      annotate("segment", x = m1, xend = m2, y = yupper, yend = yupper,
               colour = "black") +
      annotate("text", x = xright, y = yupper,
               colour = "black",
               label = paste0("d = ", format(d, digits = 2, nsmall = 2)))


  }

  return(list(
    "d" = d,
    "graph" = graph
  ))

}
