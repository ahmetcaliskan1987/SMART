#' Launch examly Shiny application
#'
#' Launches the packaged Shiny app located in `inst/shinyapp`.
#' If the app files are not found, a minimal placeholder app is launched instead.
#'
#' This function is exported so users can run \code{examly::run_app()}.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effects.
#' @export
#' @examples
#' system.file("shinyapp", package = "examly")
#' if(interactive()){
#' examly::run_app()
#' }
#' @importFrom shiny runApp fluidPage titlePanel p actionButton shinyApp
run_app <- function() {
  app_dir  <- system.file("shinyapp", package = "examly")
  app_file <- file.path(app_dir, "app.R")

  if (nzchar(app_dir) && file.exists(app_file)) {
    # YENİ EKLENEN KOD:
    # R oturumunun kendisini UTF-8 kullanmaya zorlar.
    # Bu, 'parse' hatasını (unable to translate) önler.
    old_opts <- options(encoding = "UTF-8")
    on.exit(options(old_opts), add = TRUE) # Fonksiyon bitince eski ayara döner

    # DÜZELTME: 'encoding' parametresi, eski shiny versiyonuyla
    # uyumluluk için buradan kaldırıldı.
    shiny::runApp(appDir = app_dir, display.mode = "normal")
    return(invisible(NULL))
  }

  # Fallback placeholder
  ui <- shiny::fluidPage(
    shiny::titlePanel("examly - Statistical Metrics and Reporting Tool"),
    shiny::p("Placeholder app. Put your app.R into inst/shinyapp/app.R to launch the full application."),
    shiny::actionButton("ok", "OK")
  )
  server <- function(input, output, session) {}

  shiny::runApp(shiny::shinyApp(ui = ui, server = server))
  invisible(NULL)
}
