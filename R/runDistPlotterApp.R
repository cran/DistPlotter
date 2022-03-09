#' Run the DistPlotter Shiny application
#'
#' @description Runs the DistPlotter Shiny application.
#'
#' @return There is no return value.
#'
#' @author
#' Christopher Casement \cr
#' Department of Mathematics \cr
#' Fairfield University \cr
#' \email{casementc@@gmail.com}
#'
#' Grant Innerst \cr
#' Department of Mathematics \cr
#' Shippensburg University
#'
#' Melissa Innerst \cr
#' Department of Mathematics \cr
#' Juniata College
#'
#' @examples
#' ## only run the app in an interactive R session
#' if (interactive()) {runDistPlotterApp()}
#'
#' @export
runDistPlotterApp <- function() {

  # find and launch app
  run_app_R_script <- '
    appDir <- system.file("DistPlotter", package = "DistPlotter")

    if (appDir == "") {
      stop("Could not find the DistPlotter directory. Try re-installing
        the `DistPlotter` package.", call. = FALSE
      )
    }

    shiny::runApp(appDir, launch.browser = TRUE, display.mode = "normal")
  '

  eval(parse(text = run_app_R_script))
}
