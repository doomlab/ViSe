#' Convert other statistics to d
#'
#' This function allows you to convert other effect sizes
#' to d including f, f squared, number needed to treat,
#' correlation coefficient, probability of superiority,
#' proportion overlap (u1, u2, u3, and proportion distribution
#' overlap). Please note these are approximations.
#'
#' @param f Cohen's f
#' @param f2 Cohen's f squared
#' @param nnt Number needed to treat
#' @param r Correlation coefficient
#' @param prob Probability superiority
#' @param prop_u1 Proportion Overlap U1
#' @param prop_u2 Proportion Overlap U2
#' @param prop_u3 Proportion Overlap U3
#' @param prop_overlap Proportion Overlap of Distributions
#' @return d effect size
#'
#' @keywords effect size cohen's d convert
#'
#' @examples
#' other_to_d(f = .1)
#'
#' @rdname other_to_d
#' @export
#'
other_to_d <- function (f = NULL,
                        f2 = NULL,
                        nnt = NULL,
                        r = NULL,
                        prob = NULL,
                        prop_u1 = NULL,
                        prop_u2 = NULL,
                        prop_u3 = NULL,
                        prop_overlap = NULL) {

  if(!is.null(f)) { d <- f*2 }

  if(!is.null(f2)) { d <- sqrt(f2)*2 }

  if(!is.null(nnt)) {

    save_d <- data.frame(
      d_value = seq(from = -3.00, to = 3.00, by = .001),
      NNT_value = 1:length(seq(from = -3.00, to = 3.00, by = .001))
    )

    for (i in 1:nrow(save_d)) {
      save_d$NNT_value[i] <- d_to_nnt(d = save_d$d_value[i])
    }

    save_d$NNT_diff <- save_d$NNT_value - nnt

    d <- save_d$d_value[which.min(abs(save_d$NNT_diff))]

  }

  if(!is.null(r)) { d <- (sqrt(2)*r)/sqrt(1-r^2) }

  if(!is.null(prob)) {

    save_d <- data.frame(
      d_value = seq(from = -3.00, to = 3.00, by = .001),
      prob_value = 1:length(seq(from = -3.00, to = 3.00, by = .001))
    )

    for (i in 1:nrow(save_d)) {
      save_d$prob_value[i] <- probability_superiority(d = save_d$d_value[i])
    }

    save_d$prob_diff <- save_d$prob_value - prob

    d <- save_d$d_value[which.min(abs(save_d$prob_diff))]

  }

  if(!is.null(prop_u1) | !is.null(prop_u2) | !is.null(prop_u3) | !is.null(prop_overlap)) {

    save_d <- data.frame(
      d_value = seq(from = -3.00, to = 3.00, by = .001),
      u1 = 1:length(seq(from = -3.00, to = 3.00, by = .001)),
      u2 = 1:length(seq(from = -3.00, to = 3.00, by = .001)),
      u3 = 1:length(seq(from = -3.00, to = 3.00, by = .001)),
      prop_over = 1:length(seq(from = -3.00, to = 3.00, by = .001))
    )

    for (i in 1:nrow(save_d)) {
      temp <- proportion_overlap(d = save_d$d_value[i])
      save_d$u1[i] <- temp$u1
      save_d$u2[i] <- temp$u2
      save_d$u3[i] <- temp$u3
      save_d$prop_over[i] <- temp$p_o
    }

    if(!is.null(prop_u1)){

      save_d$u1_value <- save_d$u1 - prop_u1
      d <- save_d$d_value[which.min(abs(save_d$u1_value))]

    }

    if(!is.null(prop_u2)){

      save_d$u2_value <- save_d$u2 - prop_u2
      d <- save_d$d_value[which.min(abs(save_d$u2_value))]

    }

    if(!is.null(prop_u3)){

      save_d$u3_value <- save_d$u3 - prop_u3
      d <- save_d$d_value[which.min(abs(save_d$u3_value))]

    }

    if(!is.null(prop_overlap)){

      save_d$prop_value <- save_d$prop_over - prop_overlap
      d <- save_d$d_value[which.min(abs(save_d$prop_value))]

    }

  }

    return(d)

  }



