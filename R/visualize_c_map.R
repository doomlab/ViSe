#' Visualization for Estimating c Bias + Estimates
#'
#' This function displays a visualization of the possible
#' bias c that allows for a non-zero effect in sensitivity.
#' This function includes the ability to add values of
#' effect size and correlation to see how they map onto
#' the proposed c value.
#'
#' @param dlow The lower limit of the possible effect size
#' @param dvalues A vector of effect size values that are
#' possible.
#' @param rvalues A vector of correlation values that are
#' possible.
#' @return Returns a pretty graph
#'
#' \item{graph}{The graph of possible values for c}
#'
#' @keywords effect size, estimation, visualization, ggplot
#' @import ggplot2
#'
#' @examples
#'
#' visualize_c_map(dlow = .25,
#'   dvalues = c(.2, .3, .8),
#'   rvalues = c(.1, .4, .3))
#'
#' @rdname visualize_c_map
#' @export


visualize_c_map <- function (dlow,
                             dvalues,
                             rvalues) {

  if(missing(dlow) | missing(dvalues) | missing(rvalues)){
    stop("All three values must be included.")
  }

  x_df <- y_df <- ymax <- ymin <- NULL

  x <- seq(from = -2, to = 2, by = .01)
  y <- seq(from = -1, to = 1, by = .01)

  temp <- data.frame(
    x_df = rep(x, length(y)),
    y_df = rep(y, length(x))
  )

  #dlow <- d*corr
  #dlow/d <- corr

  temp$c <- ifelse(
    temp$x_df != 0,
    dlow / temp$x_df,
    0
  )

  temp <- subset(temp, abs(c) <= 1)


  if (dlow >= 0){
    temp$ymin <- ifelse(temp$x_df < 0, temp$c, -1)
    temp$ymax <- ifelse(temp$x_df > 0, temp$c, 1)
  } else {
    temp$ymin <- ifelse(temp$x_df <= 0, -1, temp$c)
    temp$ymax <- ifelse(temp$x_df >= 0, 1, temp$c)
  }

  # make the combination of d and r values
  DF_points <- expand.grid(dvalues, rvalues)
  DF_points2 <- expand.grid(svalues, rvalues)

  #rcolorbrewer to make sure color is ok
  #use shape and color to help distinguish
  #plotly for hover
  #figure out the icons package

  graph <- ggplot(temp, aes(x_df, y_df)) +
    theme_classic() +
    geom_hline(yintercept = 0, alpha = .2) +
    geom_vline(xintercept = 0, alpha = .2) +
    coord_cartesian(xlim = c(-2,2), ylim = c(-1,1)) +
    # curve start a x = i_num, y = upper y
    # ends at y = i_num, x = upper x
    xlab("Standardized Effect Size") +
    ylab("Correlation") +
    geom_point(data = DF_points, aes(Var1, Var2)) +
    geom_point(data = DF_points2, aes(Var1, Var2), shape = 2, color = "green") +
    geom_ribbon(aes(ymin = ymin, ymax = ymax),
                color = "#88CCEE", fill = "#88CCEE", alpha = .5)

  return(list("graph" = graph))


}
