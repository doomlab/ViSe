# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  print("Hello, world!")
}

# L = "estimated dyx – Φ(1− α) ∗ SE"
# Φ = cumulative of standard normal distribution, with "α" = .05, Φ(0.95)= 1.64
# x axis is d (-2 to 2)
# y axis is correlation (-1 to 1)
# change this to L

calculate_I <- function(effect, alpha, se){
  return(effect - qnorm((1-alpha))*se)
}

i_num <- calculate_I(.70, .05, .3162278)

i_num <- 0.09

library(ggplot2)
x <- seq(from = -2, to = 2, by = .10)
y <- seq(from = -1, to = 1, by = .10)

temp <- data.frame(
  x_df = rep(x, length(y)),
  y_df = rep(y, length(x))
)

test <- ggplot(temp, aes(x_df, y_df)) +
  theme_bw() +
  geom_hline(yintercept = 0, alpha = .2) +
  geom_vline(xintercept = 0, alpha = .2) +
  coord_cartesian(xlim = c(-2,2), ylim = c(-1,1)) +
  # curve start a x = i_num, y = upper y
  # ends at y = i_num, x = upper x
  geom_curve(x = i_num, y = 1, xend = 2, yend = i_num) +
  geom_curve(x = -i_num, y = -1, xend = -2, yend = -i_num) +
  xlab("d value") +
  ylab("corr value")



# get curve points
library(ggplot2)
library(grid)
library(stringr)

# df <- data.frame(x = 1:3, y = 1:3)
# df2 <- data.frame(x = c(1,3), y = c(1,3),
#                   xend = c(2,2), yend = c(2,2))
# g <- ggplot(df, aes(x, y)) +
#   geom_point() +
#   geom_curve(aes(x = x ,y = y,
#                  xend = xend, yend = yend),
#              data = df2,
#              color = c("red", "blue"))
# g

getCurve_controlPoints <- function(ggplotObject) {
  len_layers <- length(ggplotObject$layers)
  layerNames <- lapply(seq_len(len_layers),
                       function(j) {
                         className <- class(ggplotObject$layers[[j]]$geom)
                         className[-which(className %in% c("ggproto"  ,"gg", "Geom"))]
                       })
  curveLayerId <- which(sapply(layerNames,
                               function(layerName){
                                 "GeomCurve" %in% layerName
                               }) == TRUE
  )
  gg_build <- ggplot_build(ggplotObject)
  # you can also add yes or no in your code
  # answer <- utils::menu(c("y", "n"), title="Do you want to draw the ggplot?")
  grid.draw(ggplotObject)
  grid.force()
  gridList <- grid.ls(print = FALSE)
  gridList.name <- gridList$name
  xspline.name <- gridList.name[which(str_detect(gridList.name, "curve") == TRUE)]
  xspline.len <- length(xspline.name)

  controlPoints <- lapply(seq_len(length(curveLayerId)),
                          function (j) {
                            # curve data
                            curve_data <- gg_build$data[[curveLayerId[j]]]
                            # avoid duplicated rows
                            curve_data <- curve_data[!duplicated(curve_data), ]
                            n <- dim(curve_data)[1]
                            # here we go! But wait a second, it seems like the starting and ending position do not match
                            xsplinePoints <- xsplinePoints(grid.get("xspline", grep=TRUE))[[1]]

                            # mapping data to our coordinates
                            control_data <- lapply(seq_len(n),
                                                   function(i){
                                                     if (n == 1) {
                                                       xy <- lapply(xsplinePoints,
                                                                    function(coord){
                                                                      as.numeric(coord)
                                                                    })
                                                     } else {
                                                       xy <- lapply(xsplinePoints[[i]],
                                                                    function(coord){
                                                                      as.numeric(coord)
                                                                    })
                                                     }
                                                     x.start <- curve_data[i, ]$x
                                                     x.end <- curve_data[i, ]$xend
                                                     y.start <- curve_data[i, ]$y
                                                     y.end <- curve_data[i, ]$yend
                                                     # mapping to ggplot coordinates
                                                     xy_x.diff <- xy$x[length(xy$x)] - xy$x[1]
                                                     xy_y.diff <- xy$y[length(xy$y)] - xy$y[1]
                                                     # maybe there is a better way?
                                                     if(xy_x.diff == 0){
                                                       xy_x.diff <- 1e-16
                                                     }
                                                     if(xy_y.diff == 0){
                                                       xy_y.diff <- 1e-16
                                                     }
                                                     x <- (x.end - x.start) / (xy_x.diff) * (xy$x - xy$x[1]) + x.start
                                                     y <- (y.end - y.start) / (xy_y.diff) * (xy$y - xy$y[1]) + y.start
                                                     list(x = x, y = y)
                                                   })
                            # grid remove
                            grid.remove(xspline.name[j], redraw = FALSE)
                            control_data
                          })

  controlPoints
}
points_x <- getCurve_controlPoints(test)
points_df <- data.frame(
  x_top = points_x[[1]][[1]]$x,
  y_top = points_x[[1]][[1]]$y)

points_df2 <- data.frame(
  x_bottom = points_x[[2]][[1]]$x,
  y_bottom = points_x[[2]][[1]]$y
)

points_df <-
  cbind(points_df,
        points_df2[order(points_df2$x_bottom, decreasing = FALSE), ])

# for the left side
  # y min = x bottom, y max = max y value
# for the right side
  # y min = min y value, y max = x top

inside_df <- data.frame(
  y_min = c(points_df$y_bottom, rep(-1, nrow(points_df))),
  y_max = c(rep(1, nrow(points_df)), points_df$y_top),
  x = c(points_df$x_bottom, points_df$x_top),
  y = c(points_df$y_bottom, points_df$y_top)
  )

ggplot(inside_df, aes(x, y)) +
  geom_point() +
  theme_bw() +
  geom_hline(yintercept = 0, alpha = .2) +
  geom_vline(xintercept = 0, alpha = .2) +
  coord_cartesian(xlim = c(-2,2), ylim = c(-1,1)) +
  xlab("d value") +
  ylab("corr value") +
  geom_ribbon(aes(ymin = y_min, ymax = y_max))






