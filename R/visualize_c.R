#' Visualization for Estimating c Bias
#'
#' This function displays a visualization of the possible
#' bias c that allows for a non-zero effect in sensitivity.
#'
#' @param dlow The lower limit of the possible effect size
#' @param lower Use this to indicate if you want the lower or upper bound
#' of d for one sided confidence intervals. If d is positive, you generally
#' want \code{lower = TRUE}, while negative d values should enter
#' \code{lower = FALSE} for the upper bound that is closer to zero.
#' @return Returns a pretty graph
#'
#' \item{graph}{The graph of possible values for c}
#'
#' @keywords effect size estimation visualization ggplot
#' @import ggplot2
#'
#' @examples
#'
#' visualize_c(dlow = .25, lower = TRUE)
#'
#' @rdname visualize_c
#' @export


visualize_c <- function (dlow,
                         lower = TRUE) {

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

  # if L > 0 and lower = TRUE which is d > 0
  if(dlow >= 0 && lower == TRUE){

    temp$ymin <- ifelse(temp$x_df < 0, temp$c, -1)
    temp$ymax <- ifelse(temp$x_df > 0, temp$c, 1)

    graph <- ggplot(temp, aes(x_df, y_df)) +
      theme_classic() +
      geom_hline(yintercept = 0, alpha = .2) +
      geom_vline(xintercept = 0, alpha = .2) +
      coord_cartesian(xlim = c(-2,2), ylim = c(-1,1)) +
      # curve start a x = i_num, y = upper y
      # ends at y = i_num, x = upper x
      xlab("Standardized Effect Size") +
      ylab("Correlation") +
      # geom_point(aes(x_df, c)) +
      geom_ribbon(aes(ymin = ymin, ymax = ymax),
                  color = "#88CCEE", fill = "#88CCEE", alpha = .5)

  }

  # if L < 0 and lower = TRUE which is d > 0
  if(dlow < 0 && lower == TRUE){

    temp$ymin <- ifelse(temp$x_df < 0, temp$c, -1)
    temp$ymax <- ifelse(temp$x_df > 0, temp$c, 1)

    graph <- ggplot(temp, aes(x_df, y_df)) +
      theme_classic() +
      geom_hline(yintercept = 0, alpha = .2) +
      geom_vline(xintercept = 0, alpha = .2) +
      coord_cartesian(xlim = c(-2,2), ylim = c(-1,1)) +
      # curve start a x = i_num, y = upper y
      # ends at y = i_num, x = upper x
      xlab("Standardized Effect Size") +
      ylab("Correlation") +
      # geom_point(aes(x_df, c)) +
      geom_ribbon(aes(ymin = ymin, ymax = ymax),
                  color = "#88CCEE", fill = "#88CCEE", alpha = .5)

  }

  # if L > 0 and lower = FALSE which is d < 0
  if(dlow >= 0 && lower == FALSE){

    temp$ymin <- -ifelse(temp$x_df < 0, 1, temp$c)
    temp$ymax <- -ifelse(temp$x_df > 0, -1, temp$c)

    graph <- ggplot(temp, aes(x_df, y_df)) +
      theme_classic() +
      geom_hline(yintercept = 0, alpha = .2) +
      geom_vline(xintercept = 0, alpha = .2) +
      coord_cartesian(xlim = c(-2,2), ylim = c(-1,1)) +
      # curve start a x = i_num, y = upper y
      # ends at y = i_num, x = upper x
      xlab("Standardized Effect Size") +
      ylab("Correlation") +
      # geom_point(aes(x_df, c)) +
      geom_ribbon(aes(ymin = ymin, ymax = ymax),
                  color = "#88CCEE", fill = "#88CCEE", alpha = .5)

  }

  # if L < 0 and lower = FALSE which is d < 0
  if(dlow < 0 && lower == FALSE){

    temp$ymin <- ifelse(temp$x_df < 0, temp$c, 1)
    temp$ymax <- ifelse(temp$x_df > 0, temp$c, -1)

    graph <- ggplot(temp, aes(x_df, y_df)) +
      theme_classic() +
      geom_hline(yintercept = 0, alpha = .2) +
      geom_vline(xintercept = 0, alpha = .2) +
      coord_cartesian(xlim = c(-2,2), ylim = c(-1,1)) +
      # curve start a x = i_num, y = upper y
      # ends at y = i_num, x = upper x
      xlab("Standardized Effect Size") +
      ylab("Correlation") +
      # geom_point(aes(x_df, c)) +
      geom_ribbon(aes(ymin = ymin, ymax = ymax),
                  color = "#88CCEE", fill = "#88CCEE", alpha = .5)

  }

  return(list("graph" = graph))


}
