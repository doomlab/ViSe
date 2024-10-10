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
#' @param fill_1 a color code or name to fill the first distribution
#' @param fill_2 a color code or name to fill the second distribution
#' @param text_color a color code or name for the graph text
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
                        n1 = NULL, n2 = NULL, d = NULL,
                        fill_1 = "lightblue",
                        fill_2 = "pink",
                        text_color = "black") {

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
                    fill = fill_1, alpha = 0.5, color = text_color) +
      stat_function(fun = ~ dnorm(.x, m2, sd2), geom = "area",
                    fill = fill_2, alpha = 0.5, color = text_color) +
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
                 colour = text_color) +
        annotate("text", x = xright, y = yupper,
                 colour = text_color,
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
                    fill = fill_1, alpha = 0.5, color = text_color) +
      stat_function(fun = ~ dnorm(.x, m2, sd2), geom = "area",
                    fill = fill_2, alpha = 0.5, color = text_color) +
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
               colour = text_color) +
      annotate("text", x = xright, y = yupper,
               colour = text_color,
               label = paste0("d = ", format(d, digits = 2, nsmall = 2)))


  }

  return(list(
    "d" = d,
    "graph" = graph
  ))

}
