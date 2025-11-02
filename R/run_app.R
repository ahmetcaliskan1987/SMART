#' Launch SMART Shiny application
#'
#' Launches the packaged Shiny app located in `inst/shinyapp`.
#' If the app files are not found, a minimal placeholder app is launched instead.
#'
#' This function is exported so users can run \code{SMART::run_app()}.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effects.
#' @export
#' @examples
#' \dontrun{
#' SMART::run_app()
#' }
#' @importFrom shiny runApp fluidPage titlePanel p actionButton shinyApp
run_app <- function() {
  app_dir  <- system.file("shinyapp", package = "SMART")
  app_file <- file.path(app_dir, "app.R")

  if (nzchar(app_dir) && file.exists(app_file)) {
    shiny::runApp(appDir = app_dir, display.mode = "normal")
    return(invisible(NULL))
  }

  # Fallback placeholder
  ui <- shiny::fluidPage(
    shiny::titlePanel("SMART - Statistical Metrics and Reporting Tool"),
    shiny::p("Placeholder app. Put your app.R into inst/shinyapp/app.R to launch the full application."),
    shiny::actionButton("ok", "OK")
  )
  server <- function(input, output, session) {}

  shiny::runApp(shiny::shinyApp(ui = ui, server = server))
  invisible(NULL)
}
