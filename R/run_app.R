

#' Run LinguiPhyR App
#'
#' @description Runs LinguiPhyR Shiny App
#'
#' @export
run_app <- function() {
  shiny::shinyApp(app_ui,
                  app_server)
}
