#' iskay
#'
#' @description Data analisys app for research experiments.
#' @family sapiens
#' @importFrom shiny runApp
#' @export

iskayapp <- function() {
  appDir <- system.file("iskay", package = "iskay")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `iskay`.",
         call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
