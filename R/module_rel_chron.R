#' UI for Relative Chronology
#'
#' @param id Id of module
#' @param id_lists Id of tree_lists module
#'
#' @return A shiny tagList for the relative chronology UI
analysis_rel_chron_ui <- function(id, id_lists) {
  ns <- NS(id)
  ns_lists <- NS(id_lists)
  style <- "height:500px; overflow-y: scroll;overflow-x: scroll;"

  tagList(
    h4("Relative Chronology"),
    conditionalPanel(
      paste0("input[\'",
             ns_lists("analysis_tree_radio"), "\'] != 'Strict Consensus' && ",
             "input[\'",
             ns_lists("analysis_tree_radio"), "\'] != 'Majority Consensus'"),
      HTML(paste0("Below you can select a clade (or language) and see the"
                  , " ordering of state changes from the root to that clade.
                                   Characters that have multiple possible",
                  ", states at internal nodes along this path",
                  " are not included in the analysis.")),
      htmlOutput(
                 ns("analysis_relative_chronology_lang_checkbox_html")),
      fluidRow(column(12,
                      DT::dataTableOutput(
                                          ns("analysis_relative_chronology")),
                      style = style)),
    ),
    conditionalPanel(
                     paste0("input[\'",
                            ns_lists("analysis_tree_radio"),
                            "\'] == 'Strict Consensus' || ",
                            "input[\'",
                            ns_lists("analysis_tree_radio"),
                            "\'] == 'Majority Consensus'"),
                     HTML(paste0("Relative chronology is not supported",
                                 " for consensus trees.")))
  )
}





#' Server for Relative Chronology
#'
#' @param id Id of module
#' @param my_vals Reactive variables
#' @param cache Cache object
#' @param analysis_root_drop Root from analysis page
#' @param tree_to_listen Radio box selection
#' @param data_upload_root_drop Root from data upload page
#' @param paup_is_weighted Whether or not PAUP* was run with weight
#' @param analysis_metrics Metrics to include in analysis
#'
#' @return Nothing
analysis_rel_chron_server <- function(id,
                                      my_vals,
                                      cache,
                                      analysis_root_drop,
                                      tree_to_listen,
                                      data_upload_root_drop,
                                      paup_is_weighted,
                                      analysis_metrics) {
  moduleServer(id, function(input, output, session) {

    col_start <- 5

    observe({
      choices <- names(my_vals[["data"]])
      if (is.null(choices) || is.null(analysis_root_drop())) return()

      choices <- choices[col_start:length(choices)]
      choices <- choices[choices != analysis_root_drop()]
      output$analysis_relative_chronology_lang_checkbox_html <- renderUI({
        ns <- session$ns
        checkboxGroupInput(inputId =
                             ns("analysis_relative_chronology_lang_checkbox"),
                           choices = choices,
                           label = NULL,
                           inline = TRUE)
      })
    })

    observe({
      clade <- input[["analysis_relative_chronology_lang_checkbox"]]
      if (is.null(clade)) {
        output$analysis_relative_chronology <- NULL
        return()
      }
      if (!(my_vals$paup_finished || my_vals$analysis_upload_finished)) return()


      ttl <- tree_to_listen()
      tree_to_use <- get_tree_to_use(ttl,
                                     my_vals,
                                     cache,
                                     isolate(data_upload_root_drop()),
                                     isolate(paup_is_weighted()),
                                     analysis_metrics())
      if (is.null(tree_to_use)) return()
      tree_to_do <- tree_to_use$tree_to_do
      tree <- tree_to_use$tree
      set <- tree_to_use$set

      root_of_tree <- analysis_root_drop()
      cache_id <- paste0(ttl$set, "_", tree_to_do)

      char_reps <- cache[["cache"]][["char_reps"]]
      parsimony_cache <- cache_wrapper(cache,
                                       paste0(cache_id, "_parsimony"),
                                       {
                                         run_parsimony_on_each_char(tree,
                                                                    char_reps)
                                       })
      res <- make_chronology(tree,
                             clade,
                             char_reps,
                             parsimony_cache,
                             analysis_root_drop())
      if (is.character(res)) {
        # Should render the error message, but currently just makes blank
        output$analysis_relative_chronology <- NULL
      } else {
        res <-  add_buttons_to_table(res, "relchronchars", "char_id")
        min_rank <- min(res$rank)
        max_rank <- max(res$rank)
        info <- RColorBrewer::brewer.pal.info["Pastel1", "maxcolors"]
        colors <- RColorBrewer::brewer.pal(info, "Pastel1")
        buttons <- c("copy", "csv", "excel", "pdf", "print")
        range <- min_rank:max_rank
        output$analysis_relative_chronology <- DT::renderDataTable({
          DT::datatable(res,
                        editable = FALSE,
                        rownames = FALSE,
                        escape = FALSE,
                        selection = "none",
                        extensions = "Buttons",
                        options = list(paging = FALSE, dom = "Bfrtip",
                                       buttons = buttons)) %>%
            DT::formatStyle(
                            "rank",
                            target = "row",
                            backgroundColor = DT::styleEqual(range,
                                                             colors[range]))
        })
      }
    })
  })
}
