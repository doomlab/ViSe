#' Visualization for Conversions of Effect Sizes
#'
#' This function displays a visualization the same effect in
#' various effect sizes including d, f, \eqn{f^2}, proportion
#' overlap, correlation, number needed to treat, and more.
#'
#' @param d d effect size to convert to other numbers
#' @param circle_color a color name or code for the highlighted part of the
#' donut circle
#' @param circle_fill a color name or code for the rest of the circle
#' @param percent_color a color name or code for the text of the
#' effect size
#' @param percent_size a numeric value representing the font size of the
#' larger effect size text inside the circle
#' @param text_color a color name or code that changes the color of the
#' effect size text label
#' @param font_family A font family name for the font of the effect
#' size text label
#'
#'
#' @return Returns a pretty graph of all the effects
#'
#' \item{graph}{ggplot object of converted effect sizes}
#'
#' @keywords effect size estimation visualization ggplot
#' @import ggplot2 cowplot
#'
#' @examples
#'
#' visualize_effects(d = .25)
#'
#' @rdname visualize_effects
#' @export


visualize_effects <- function (d,
                               circle_color = "lightblue",
                               circle_fill = "grey",
                               percent_color = "black",
                               percent_size = 12,
                               text_color = "black",
                               font_family = "Times") {

  f <- d_to_f2(d)
  nnt <- d_to_nnt(d)
  nnt_upper <- d_to_nnt(d = .001)
  r <- d_to_r(d)
  prob <- probability_superiority(d)
  prop <- proportion_overlap(d = d)

  d_graph <- big_number_donut_raw(
    value = d,
    font_family = font_family,
    highlight_color = circle_color,
    upper = 3,
    circle_fill = circle_fill,
    percent_color = percent_color,
    percent_size = percent_size
  ) +
    geom_text(data=data.frame(), aes(label = "d", x = -Inf,
                                     y = Inf, family = font_family,
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = text_color)


  f_graph <- big_number_donut_raw(
    value = f$f2,
    font_family = font_family,
    highlight_color = circle_color,
    upper = d,
    circle_fill = circle_fill,
    percent_color = percent_color,
    percent_size = percent_size
  ) +
    geom_text(data=data.frame(), aes(label = "f", x = -Inf,
                                     y = Inf, family = font_family,
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = text_color)

  nnt_graph <- big_number_donut_raw(
    value = nnt,
    font_family = font_family,
    highlight_color = circle_color,
    upper = nnt_upper,
    circle_fill = circle_fill,
    percent_color = percent_color,
    percent_size = percent_size
  ) +
    geom_text(data=data.frame(), aes(label = "NNT", x = -Inf,
                                     y = Inf, family = font_family,
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = text_color)

  r_graph <- big_number_donut_raw(
    value = r,
    font_family = font_family,
    highlight_color = circle_color,
    upper = 1,
    circle_fill = circle_fill,
    percent_color = percent_color,
    percent_size = percent_size
  ) +
    geom_text(data=data.frame(), aes(label = "r", x = -Inf,
                                     y = Inf, family = font_family,
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = text_color)

  prob_graph <- big_number_donut_percent(
    value = prob,
    font_family = font_family,
    highlight_color = circle_color,
    circle_fill = circle_fill,
    percent_color = percent_color,
    percent_size = percent_size
  ) +
    geom_text(data=data.frame(), aes(label = "Superiority", x = -Inf,
                                     y = Inf, family = font_family,
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = text_color)

  u1_graph <- big_number_donut_percent(
    value = prop$u1,
    font_family = font_family,
    highlight_color = circle_color,
    circle_fill = circle_fill,
    percent_color = percent_color,
    percent_size = percent_size
  ) +
    geom_text(data=data.frame(), aes(label = "U1", x = -Inf,
                                     y = Inf, family = font_family,
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = text_color)

  u2_graph <- big_number_donut_percent(
    value = prop$u2,
    font_family = font_family,
    highlight_color = circle_color,
    circle_fill = circle_fill,
    percent_color = percent_color,
    percent_size = percent_size
  ) +
    geom_text(data=data.frame(), aes(label = "U2", x = -Inf,
                                     y = Inf, family = font_family,
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = text_color)

  u3_graph <- big_number_donut_percent(
    value = prop$u3,
    font_family = font_family,
    highlight_color = circle_color,
    circle_fill = circle_fill,
    percent_color = percent_color,
    percent_size = percent_size
  ) +
    geom_text(data=data.frame(), aes(label = "U3", x = -Inf,
                                     y = Inf, family = font_family,
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = text_color)

  po_graph <- big_number_donut_percent(
    value = prop$p_o,
    font_family = font_family,
    highlight_color = circle_color,
    circle_fill = circle_fill,
    percent_color = percent_color,
    percent_size = percent_size
  ) +
    geom_text(data=data.frame(), aes(label = "Proportional", x = -Inf,
                                     y = Inf, family = font_family,
                                     fontface = "italic"),
              hjust = .5, vjust = 4, inherit.aes = FALSE, color = text_color)

  graph <- plot_grid(
    d_graph, r_graph, f_graph,
    nnt_graph, prob_graph, po_graph,
    u1_graph, u2_graph, u3_graph,
    nrow = 3
  )


  return(list("graph" = graph))

}
