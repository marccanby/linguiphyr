#' Shiny UI for LinguiPhyR
#'
#' @param request Request variable
app_ui <- function(request) {
  shinyjs::useShinyjs()
  shinyUI(
    conditionalPanel("true",
      tags$div(
        class = "navbar",
        style = paste0("position: fixed; top: 0; width: 100%; background:",
                       " linear-gradient(to bottom, #5DA9E4, #337EC1);",
                       " padding: 10px;"),
        fluidRow(
          column(6,
            h4("LinguiPhyR", style = "color: white;")
          ),
          column(6,
            HTML(paste0("<div style=\"float:right;\"><div ",
                        "style=\"color:white; font-weight: normal;",
                        " font-size: 8px; height: 100%;",
                        " line-height:41px;\">Version ",
                        packageVersion("linguiphyr"),
                        "</div></div>")
            )
          )
        )
      ),
      tags$style(type = "text/css",
        paste0("body {padding-top: 70px;} #analysis_tree_radio .radio label ",
               "{ font-size: 13px; } #analysis_treeupload_radio .radio ",
               "label { font-size: 13px; }")
      ),
      fluidPage(
        theme = shinythemes::shinytheme("cerulean"),
        shinyjs::useShinyjs(),
        sidebarPanel(style = "height: 90vh; overflow-y: auto;",
          width = 2,
          conditionalPanel("input.inferenceTabsetPanel == \"Data Upload\"",
            conditionalPanel("output.data_is_loaded",
              data_upload_ui_sidebar("data_upload")
            )
          ),
          conditionalPanel("input.inferenceTabsetPanel == \"Tree Search\"",
            conditionalPanel("output.data_is_loaded",
              paup_ui_sidebar("paup")
            )
          ),
          conditionalPanel("input.inferenceTabsetPanel == \"Analysis\"",
            analysis_ui_sidebar("analysis_computation"),
            analysis_tree_lists_ui("analysis_tree_lists")
          )
        ),
        mainPanel(width = 10,
          style = "height: 90vh; overflow-y: auto;",
          tabsetPanel(
            tabPanel("Data Upload",
              data_upload_ui("data_upload")
            ),
            tabPanel("Tree Search",
              paup_ui("paup")
            ),
            tabPanel("Analysis",
              fluidPage(
                conditionalPanel(paste0("output.paup_finished ||",
                                        " output.analysis_upload_finished"),
                  analysis_tree_ui("analysis_computation"),
                  wellPanel(
                    tabsetPanel(
                      tabPanel("Incompatible Characters",
                        analyisis_incompat_chars_ui("analysis_computation",
                                                    "analysis_tree_lists")
                      ),
                      tabPanel("Enforcing Characters",
                        analysis_enforcing_chars_ui("analysis_computation",
                                                    "analysis_tree_lists")
                      ),
                      tabPanel("All Characters",
                        analysis_all_chars_ui("analysis_computation",
                                              "analysis_tree_lists")
                      ),
                      tabPanel("Relative Chronology",
                        analysis_rel_chron_ui("analysis_rel_chron",
                                              "analysis_tree_lists")
                      ),
                      id = "analysisTabsetPanel"
                    )
                  )
                )
              )
            ),
            id = "inferenceTabsetPanel"
          )
        ),
        id = "mainNavbar"
      )
    )
  )
}
