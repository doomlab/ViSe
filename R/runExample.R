#' @rdname runExample
#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "ViSe", package = "ViSe")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ViSe`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
