#' Run Shiny App
#'
#' This function is a convenience function to help
#' you easily run the shiny app for the package.
#'
#' @rdname runExample
#' @return Opens the shiny app version of the package to
#' use interactively.
#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "ViSe", package = "ViSe")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ViSe`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
