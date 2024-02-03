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
#'   rvalues = c(.1, .4, .3),
#'   lower = TRUE)
#'
#' @rdname visualize_c_map
#' @export


visualize_c_map <- function (dlow,
                             r_values,
                             d_values = NULL,
                             f_values = NULL,
                             f2_values = NULL,
                             nnt_values = NULL,
                             prob_values = NULL,
                             propu1_values = NULL,
                             propu2_values = NULL,
                             propu3_values = NULL,
                             propover_values = NULL,
                             lower = TRUE) {

  if(missing(dlow) | missing(r_values)){
    stop("You must have dlow and r_values included.")
  }

  # make the plot
  graph <- visualize_c(dlow = dlow, lower = lower)$graph


  if(!is.null(d_values)){
    # make the combination of d and r values
    DF_points <- expand.grid(d_values, r_values)
    DF_points2 <- expand.grid(d_values, r_values)
    colnames(DF_points) <- colnames(DF_points2) <- c("d", "r")
    DF_points$label <- DF_points2$label <- paste0("d = ", DF_points$d)

  }

  if(!is.null(f_values)){
    # make the combination of d and r values
    d_values <- 1:length(f_values)
    for (i in 1:length(f_values)){
      d_values[i] <- other_to_d(f = f_values[i])
    }

    DF_points_temp <- expand.grid(d_values, r_values)
    DF_points2_temp <- expand.grid(d_values, r_values)
    colnames(DF_points_temp) <- colnames(DF_points2_temp) <- c("d", "r")
    DF_points_temp$label <- DF_points2_temp$label <- paste0("f = ", f_values)

    if (exists("DF_points")){
      DF_points <- rbind(DF_points, DF_points_temp)
      DF_points2 <- rbind(DF_points2, DF_points2_temp)
    } else {
        DF_points <- DF_points_temp
        DF_points2 <- DF_points2_temp
      }
  }

  graph2 <- graph +
    geom_point(data = DF_points, aes(d, r, color = DF_points$label), size = 2) +
    geom_point(data = DF_points2, aes(d, r, color = DF_points2$label,), shape = 2, size = 3) +
    scale_color_discrete(name = "")


  return(list("graph" = graph2))


}
