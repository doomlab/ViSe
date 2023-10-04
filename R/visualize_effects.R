#' Visualization for Conversions of Effect Sizes
#'
#' This function displays a visualization the same effect in
#' various effect sizes including d, f, \eqn{f^2}, proportion
#' overlap, correlation, number needed to treat, and more.
#'
#' @param d d effect size to convert to other numbers
#' @return Returns a pretty graph of all the effects
#'
#' \item{graph}{ggplot object of converted effect sizes}
#'
#' @keywords effect size, estimation, visualization, ggplot
#' @import ggplot2 cowplot
#'
#' @examples
#'
#' visualize_effects(d = .25)
#'
#' @rdname visualize_effects
#' @export


visualize_effects <- function (d) {

  f <- d_to_f2(d)
  nnt <- d_to_nnt(d)
  nnt_upper <- d_to_nnt(d = .001)
  r <- d_to_r(d)
  prob <- probability_superiority(d)
  prop <- proportion_overlap(d = d)

  d_graph <- big_number_donut_raw(
    value = abs(d),
    font_family = "Times",
    highlight_color = "blue",
    upper = 3
  ) +
    geom_text(data=data.frame(), aes(label = "d", x = -Inf,
                                     y = Inf, family = "Times",
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = "blue")


  f_graph <- big_number_donut_raw(
    value = f$f2,
    font_family = "Times",
    highlight_color = "blue",
    upper = d
  ) +
    geom_text(data=data.frame(), aes(label = "f", x = -Inf,
                                     y = Inf, family = "Times",
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = "blue")

  nnt_graph <- big_number_donut_raw(
    value = nnt,
    font_family = "Times",
    highlight_color = "blue",
    upper = nnt_upper
  ) +
    geom_text(data=data.frame(), aes(label = "NNT", x = -Inf,
                                     y = Inf, family = "Times",
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = "blue")

  r_graph <- big_number_donut_raw(
    value = r,
    font_family = "Times",
    highlight_color = "blue",
    upper = 1
  ) +
    geom_text(data=data.frame(), aes(label = "r", x = -Inf,
                                     y = Inf, family = "Times",
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = "blue")

  prob_graph <- big_number_donut_percent(
    value = prob,
    font_family = "Times",
    highlight_color = "blue"
  ) +
    geom_text(data=data.frame(), aes(label = "Superiority", x = -Inf,
                                     y = Inf, family = "Times",
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = "blue")

  u1_graph <- big_number_donut_percent(
    value = prop$u1,
    font_family = "Times",
    highlight_color = "blue"
  ) +
    geom_text(data=data.frame(), aes(label = "U1", x = -Inf,
                                     y = Inf, family = "Times",
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = "blue")

  u2_graph <- big_number_donut_percent(
    value = prop$u2,
    font_family = "Times",
    highlight_color = "blue"
  ) +
    geom_text(data=data.frame(), aes(label = "U2", x = -Inf,
                                     y = Inf, family = "Times",
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = "blue")

  u3_graph <- big_number_donut_percent(
    value = prop$u3,
    font_family = "Times",
    highlight_color = "blue"
  ) +
    geom_text(data=data.frame(), aes(label = "U3", x = -Inf,
                                     y = Inf, family = "Times",
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = "blue")

  po_graph <- big_number_donut_percent(
    value = prop$p_o,
    font_family = "Times",
    highlight_color = "blue"
  ) +
    geom_text(data=data.frame(), aes(label = "Proportional", x = -Inf,
                                     y = Inf, family = "Times",
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = "blue")

  graph <- plot_grid(
    d_graph, r_graph, f_graph,
    nnt_graph, prob_graph, po_graph,
    u1_graph, u2_graph, u3_graph,
    nrow = 3
  )


  return(list("graph" = graph))

}
