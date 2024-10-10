#' Visualization for Estimating c Bias + Estimates
#'
#' This function displays a visualization of the possible
#' bias c that allows for a non-zero effect in sensitivity.
#' This function includes the ability to add values of
#' effect size and correlation to see how they map onto
#' the proposed c value.
#'
#'
#' @param dlow The lower limit of the possible effect size (required).
#' @param r_values A vector of correlation values that are
#' possible (required).
#' @param lower Use this to indicate if you want the lower or upper bound
#' of d for one sided confidence intervals. If d is positive, you generally
#' want \code{lower = TRUE}, while negative d values should enter
#' \code{lower = FALSE} for the upper bound that is closer to zero (required).
#' @param d_values A vector of effect size values that are
#' possible.
#' @param f_values A vector of f effect size values that are
#' possible.
#' @param f2_values A vector of f2 effect size values that are
#' possible.
#' @param nnt_values A vector of number needed to treat
#' effect size values that are possible.
#' @param prob_values A vector of probability of superiority
#' effect size values that are possible.
#' @param prop_u1_values A vector of proportion of overlap u1
#' effect size values that are possible.
#' @param prop_u2_values A vector of proportion of overlap u2
#' effect size values that are possible.
#' @param prop_u3_values A vector of proportion of overlap u3
#' effect size values that are possible.
#' @param prop_overlap_values A vector of proportion of
#' distribution overlap effect size values that are possible.
#' @param point_colors A vector of color names or codes to plot
#' the effect sizes on the graph. You should use as many color
#' names/codes as you have max of an effect size (i.e, if r has 4, d has 3,
#' and prob has 5, then use 5 as the max number of colors).
#' @param size The size of the symbols on the chart.
#' @param shape_1 a numeric value of one of the ggplot2 shapes
#' @param shape_2 a numeric value of one of the ggplot2 shapes -
#' if you use different numbers, the two shapes are overlaid, as
#' we found this effect made it easier to read with many effect sizes
#' plotted on the same graph.
#' @param ribbon_color a color name or code to shade the area that
#' shows a non-zero effect in sensitivity.
#'
#'
#' @return Returns a pretty graph of the possible effect size
#' and correlation combinations with the region of effect colored in.
#' Note that all effect sizes are converted to d for the graph.
#'
#' \item{graph}{The graph of possible values for c}
#'
#' @keywords effect size estimation visualization ggplot
#' @import ggplot2
#'
#' @examples
#'
#' visualize_c_map(dlow = .25,
#'   d_values = c(.2, .3, .8),
#'   r_values = c(.1, .4, .3),
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
                             prop_u1_values = NULL,
                             prop_u2_values = NULL,
                             prop_u3_values = NULL,
                             prop_overlap_values = NULL,
                             point_colors = c("red", "green", "blue"),
                             size = 2,
                             shape_1 = 2,
                             shape_2 = 3,
                             ribbon_color = "lightblue",
                             lower = TRUE) {

  if(missing(dlow) | missing(r_values)){
    stop("You must have dlow and r_values included.")
  }

  # make the plot
  graph <- visualize_c(dlow = dlow, lower = lower,
                       ribbon_color = ribbon_color)$graph

  d <- r <- NULL

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

  if(!is.null(f2_values)){
    # make the combination of d and r values
    d_values <- 1:length(f2_values)
    for (i in 1:length(f2_values)){
      d_values[i] <- other_to_d(f2 = f2_values[i])
    }

    DF_points_temp <- expand.grid(d_values, r_values)
    DF_points2_temp <- expand.grid(d_values, r_values)
    colnames(DF_points_temp) <- colnames(DF_points2_temp) <- c("d", "r")
    DF_points_temp$label <- DF_points2_temp$label <- paste0("f2 = ", f2_values)

    if (exists("DF_points")){
      DF_points <- rbind(DF_points, DF_points_temp)
      DF_points2 <- rbind(DF_points2, DF_points2_temp)
    } else {
      DF_points <- DF_points_temp
      DF_points2 <- DF_points2_temp
    }
  }

  if(!is.null(nnt_values)){
    # make the combination of d and r values
    d_values <- 1:length(nnt_values)
    for (i in 1:length(nnt_values)){
      d_values[i] <- other_to_d(nnt = nnt_values[i])
    }

    DF_points_temp <- expand.grid(d_values, r_values)
    DF_points2_temp <- expand.grid(d_values, r_values)
    colnames(DF_points_temp) <- colnames(DF_points2_temp) <- c("d", "r")
    DF_points_temp$label <- DF_points2_temp$label <- paste0("nnt = ", nnt_values)

    if (exists("DF_points")){
      DF_points <- rbind(DF_points, DF_points_temp)
      DF_points2 <- rbind(DF_points2, DF_points2_temp)
    } else {
      DF_points <- DF_points_temp
      DF_points2 <- DF_points2_temp
    }
  }

  if(!is.null(prob_values)){
    # make the combination of d and r values
    d_values <- 1:length(prob_values)
    for (i in 1:length(prob_values)){
      d_values[i] <- other_to_d(prob = prob_values[i])
    }

    DF_points_temp <- expand.grid(d_values, r_values)
    DF_points2_temp <- expand.grid(d_values, r_values)
    colnames(DF_points_temp) <- colnames(DF_points2_temp) <- c("d", "r")
    DF_points_temp$label <- DF_points2_temp$label <- paste0("prob = ", prob_values)

    if (exists("DF_points")){
      DF_points <- rbind(DF_points, DF_points_temp)
      DF_points2 <- rbind(DF_points2, DF_points2_temp)
    } else {
      DF_points <- DF_points_temp
      DF_points2 <- DF_points2_temp
    }
  }

  if(!is.null(prop_u1_values)){
    # make the combination of d and r values
    d_values <- 1:length(prop_u1_values)
    for (i in 1:length(prop_u1_values)){
      d_values[i] <- other_to_d(prop_u1 = prop_u1_values[i])
    }

    DF_points_temp <- expand.grid(d_values, r_values)
    DF_points2_temp <- expand.grid(d_values, r_values)
    colnames(DF_points_temp) <- colnames(DF_points2_temp) <- c("d", "r")
    DF_points_temp$label <- DF_points2_temp$label <- paste0("prop_u1 = ", prop_u1_values)

    if (exists("DF_points")){
      DF_points <- rbind(DF_points, DF_points_temp)
      DF_points2 <- rbind(DF_points2, DF_points2_temp)
    } else {
      DF_points <- DF_points_temp
      DF_points2 <- DF_points2_temp
    }
  }

  if(!is.null(prop_u2_values)){
    # make the combination of d and r values
    d_values <- 1:length(prop_u2_values)
    for (i in 1:length(prop_u2_values)){
      d_values[i] <- other_to_d(prop_u2 = prop_u2_values[i])
    }

    DF_points_temp <- expand.grid(d_values, r_values)
    DF_points2_temp <- expand.grid(d_values, r_values)
    colnames(DF_points_temp) <- colnames(DF_points2_temp) <- c("d", "r")
    DF_points_temp$label <- DF_points2_temp$label <- paste0("prop_u2 = ", prop_u2_values)

    if (exists("DF_points")){
      DF_points <- rbind(DF_points, DF_points_temp)
      DF_points2 <- rbind(DF_points2, DF_points2_temp)
    } else {
      DF_points <- DF_points_temp
      DF_points2 <- DF_points2_temp
    }
  }

  if(!is.null(prop_u3_values)){
    # make the combination of d and r values
    d_values <- 1:length(prop_u3_values)
    for (i in 1:length(prop_u3_values)){
      d_values[i] <- other_to_d(prop_u3 = prop_u3_values[i])
    }

    DF_points_temp <- expand.grid(d_values, r_values)
    DF_points2_temp <- expand.grid(d_values, r_values)
    colnames(DF_points_temp) <- colnames(DF_points2_temp) <- c("d", "r")
    DF_points_temp$label <- DF_points2_temp$label <- paste0("prop_u3 = ", prop_u3_values)

    if (exists("DF_points")){
      DF_points <- rbind(DF_points, DF_points_temp)
      DF_points2 <- rbind(DF_points2, DF_points2_temp)
    } else {
      DF_points <- DF_points_temp
      DF_points2 <- DF_points2_temp
    }
  }

  if(!is.null(prop_overlap_values)){
    # make the combination of d and r values
    d_values <- 1:length(prop_overlap_values)
    for (i in 1:length(prop_overlap_values)){
      d_values[i] <- other_to_d(prop_overlap = prop_overlap_values[i])
    }

    DF_points_temp <- expand.grid(d_values, r_values)
    DF_points2_temp <- expand.grid(d_values, r_values)
    colnames(DF_points_temp) <- colnames(DF_points2_temp) <- c("d", "r")
    DF_points_temp$label <- DF_points2_temp$label <- paste0("prop_overlap = ", prop_overlap_values)

    if (exists("DF_points")){
      DF_points <- rbind(DF_points, DF_points_temp)
      DF_points2 <- rbind(DF_points2, DF_points2_temp)
    } else {
      DF_points <- DF_points_temp
      DF_points2 <- DF_points2_temp
    }
  }

  length_colors <- length(unique(DF_points$label))
  if (length(point_colors) != length_colors){
    stop(paste0("You must provide ",
                length(length_colors), " color names or numbers."))
  }

  graph2 <- graph +
    geom_point(data = DF_points, aes(d, r, color = DF_points$label),
               shape = shape_1,
               size = size) +
    geom_point(data = DF_points2, aes(d, r, color = DF_points2$label),
               shape = shape_2, size = size+1) +
    scale_color_manual(name = "",
                         values = point_colors)


  return(list("graph" = graph2))


}
