#' Shiny Server for LinguiPhyR
#'
#' @param input Input for server
#' @param output Output for server
#' @param session Session variable
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024^2)

  # For now, suppress all warnings. It's not ideal, but plotly spews a bunch
  #   of warnings that aren't even a problem. Ideally I'd just suppress those,
  #   but that does not seem easy.......
  options(warn = -1)

  # Reactive variables
  my_vals <- reactiveValues()
  my_vals$data_is_loaded <- FALSE
  my_vals$paup_finished <- FALSE
  my_vals$paup_generated <- FALSE
  my_vals$analysis_upload_finished <- FALSE

  cache <- reactiveValues()
  cache[["cache"]] <- list()

  # Put them into output so they can be used in UI
  output$data_is_loaded <- reactive(my_vals$data_is_loaded)
  outputOptions(output,
                "data_is_loaded",
                suspendWhenHidden = FALSE)
  output$paup_finished <- reactive(my_vals$paup_finished)
  outputOptions(output,
                "paup_finished",
                suspendWhenHidden = FALSE)
  output$paup_generated <- reactive(my_vals$paup_generated)
  outputOptions(output,
                "paup_generated",
                suspendWhenHidden = FALSE)
  output$analysis_upload_finished <- reactive(my_vals$analysis_upload_finished)
  outputOptions(output,
                "analysis_upload_finished",
                suspendWhenHidden = FALSE)

  # Additional server files - some of these return reactive values (and some
  #   take in reactive values)
  data_upload <- data_upload_server("data_upload", my_vals)
  paup <- paup_server("paup",
    my_vals,
    cache,
    data_upload$root_drop
  )

  tree_to_listen <- analysis_tree_lists_server("analysis_tree_lists",
    my_vals,
    cache,
    data_upload$root_drop,
    paup$paup_is_weighted,
    analysis_computation$analysis_metrics
  )
  analysis_computation <- analysis_server("analysis_computation",
                                          my_vals,
                                          cache,
                                          data_upload$root_drop,
                                          reactive(input$table_button),
                                          tree_to_listen,
                                          paup$paup_is_weighted)
  analysis_rel_chron_server("analysis_rel_chron",
                            my_vals,
                            cache,
                            analysis_computation$analysis_root_drop,
                            tree_to_listen,
                            data_upload$root_drop,
                            paup$paup_is_weighted,
                            analysis_computation$analysis_metrics)
}
