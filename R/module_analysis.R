#' UI Sidebar for Analysis
#'
#' @param id Id of module
#'
#' @return A shiny tagList for the analysis UI sidebar
analysis_ui_sidebar <- function(id) {
  ns <- NS(id)
  choices <- c("Parsimony (\u21E9)",
               "Weighted Parsimony (\u21E9)",
               "Incompatibility (\u21E9)",
               "Weighted Incompatibility (\u21E9)",
               "Total Edge Support (\u21E7)",
               "Weighted Total Edge Support (\u21E7)",
               "Minimum Edge Support (\u21E7)",
               "Weighted Minimum Edge Support (\u21E7)")

  tagList(
    conditionalPanel("output.analysis_upload_finished || output.paup_finished",
      h4("Settings"),
      htmlOutput(ns("analysis_root_drop_html")),
      fluidRow(column(12,
        style = "margin-bottom:-20px;",
        sliderInput(ns("analysis_height"),
                    "Height:",
                    min = 20,
                    max = 1200,
                    value = 400,
                    step = 20)
      )
      ),
      fluidRow(column(12,
        style = "margin-bottom:-20px;",
        sliderInput(ns("analysis_width"),
                    "Width:",
                    min = 20,
                    max = 1200,
                    value = 800,
                    step = 20)
      )
      ),
      fluidRow(column(12,
                      style = "margin-bottom:-20px;",
                      checkboxInput(ns("analysis_root_on_left"),
                                    "Display as unrooted", TRUE))),
      fluidRow(column(12,
                      style = "margin-bottom:-20px;",
                      checkboxInput(ns("analysis_show_edge_ids"),
                                    "Display edge ids", TRUE))),
      fluidRow(column(12,
                      style = "margin-bottom:-20px;",
                      checkboxInput(ns("analysis_show_edge_support"),
                                    "Display edge support", TRUE))),
      fluidRow(column(12,
                      checkboxInput(ns("analysis_show_weighted_edge_support"),
                                    "Display weighted edge support",
                                    FALSE))),
      fluidRow(column(12,
                      checkboxGroupInput(ns("analysis_metrics"),
                                         label = "Metrics:",
                                         choices = choices,
                                         selected = c(paste0("Weighted ",
                                                             "Parsimony ",
                                                             "(\u21E9)")
                                         )))),
    )
  )
}

#' UI for Analysis Tree Diagram
#'
#' @param id Id of module
#'
#' @return UI for analysis page
analysis_tree_ui <- function(id) {
  ns <- NS(id)
  wellPanel(htmlOutput(ns("analysis_tree_title")),
    HTML(paste0("The black numbers in parentheses below edges are edge ",
                "id's; they are used in the 'Enforcing Characters' tab. ",
                "The <span style='color:blue;'>blue</span> numbers ",
                "above the edges are the number of characters (and ",
                "weighted number of characters) that support a given edge.")),
    br(),
    br(),
    fluidRow(
      column(
        12,
        uiOutput(ns("analysis_tree_plotly"))
      )
    )
  )
}

#' UI for Analysis Incompatible Characters
#'
#' @param id Id of module
#' @param id_lists Id of tree_lists module
#'
#' @return A shiny tagList for the analysis incompatible characters UI
analyisis_incompat_chars_ui <- function(id,
                                        id_lists) {
  ns <- NS(id)
  ns_lists <- NS(id_lists)
  style <- "height:500px; overflow-y: scroll;overflow-x: scroll;"
  table_name <- "analysis_incompat_on_any_char_table"
  tagList(
    conditionalPanel(paste0("input[\'",
                            ns_lists("analysis_tree_radio"),
                            "\'] != 'Strict Consensus' && input[\'",
                            ns_lists("analysis_tree_radio"),
                            "\'] != 'Majority Consensus'"),
      h4("Incompatible Characters"),
      fluidRow(column(12,
                      htmlOutput(ns("analysis_incompat_char_text_html")))),
      fluidRow(column(12,
                      DT::dataTableOutput(ns("analysis_incompat_char_table")),
                      style = style))
    ),
    conditionalPanel(paste0("input[\'",
                            ns_lists("analysis_tree_radio"),
                            "\'] == 'Strict Consensus' || input[\'",
                            ns_lists("analysis_tree_radio"),
                            "\'] == 'Majority Consensus'"),
      h4("Characters Incompatible on At Least One Tree"),
      fluidRow(column(12,
        htmlOutput(ns("analysis_incompat_on_any_char_text_html"))
      )),
      br(),
      fluidRow(column(12,
                      DT::dataTableOutput(ns(table_name)),
                      style = style))
    )
  )
}

#' UI for Analysis Enforcing Characters
#'
#' @param id Id of module
#' @param id_lists Id of tree_lists module
#'
#' @return A shiny tagList for the analysis enforcing characters UI
analysis_enforcing_chars_ui <- function(id,
                                        id_lists) {
  ns <- NS(id)
  ns_lists <- NS(id_lists)
  style <- "height:500px; overflow-y: scroll;overflow-x: scroll;"
  tagList(
    h4("Characters that Enforce Each Edge"),
    conditionalPanel(paste0("input[\'",
                            ns_lists("analysis_tree_radio"),
                            "\'] != 'Strict Consensus' && input[\'",
                            ns_lists("analysis_tree_radio"),
                            "\'] != 'Majority Consensus'"),
      HTML(paste0("Each non-trivial edge is in the tree because ",
                  "certain characters support the split it creates. ",
                  "The table below indicates which characters enforce each ",
                  "of these edges, which are referenced by their edge_id.",
                  "<br>A character is said to enforce an edge if the edge's ",
                  "collapse would increase the parsimony score of the ",
                  "character. This <em>does</em> account for custom transition",
                  " costs (if provided).<br>You can annotate the tree with",
                  " a particular character by using the 'Select' button.")),
      fluidRow(column(12,
                      DT::dataTableOutput(ns("analysis_enforcing_chars")),
                      style = style)),
    ),
    conditionalPanel(paste0("input[\'",
                            ns_lists("analysis_tree_radio"),
                            "\'] == 'Strict Consensus' || input[\'",
                            ns_lists("analysis_tree_radio"),
                            "\'] == 'Majority Consensus'"),
                     HTML(paste0("This is currently not ",
                                 "supported for consensus trees.")))
  )
}

#' UI for Analysis All Characters
#'
#' @param id Id of module
#' @param id_lists Id of tree_lists module
#'
#' @return A shiny tagList for the analysis all characters UI
analysis_all_chars_ui <- function(id, id_lists) {
  ns <- NS(id)
  ns_lists <- NS(id_lists)
  style <- "height:500px; overflow-y: scroll;overflow-x: scroll;"
  table_name <- "analysis_consensus_all_chars"
  tagList(
    h4("All Characters"),
    conditionalPanel(paste0("input[\'",
                            ns_lists("analysis_tree_radio"),
                            "\'] != 'Strict Consensus' && input[\'",
                            ns_lists("analysis_tree_radio"),
                            "\'] != 'Majority Consensus'"),
      HTML(paste0("All characters from the dataset are listed here.",
                  " Note that the sum of the \"ps_score\" (or",
                  " \"ps_score_weighted\") column is the ",
                  "overall (weighted) parsimony score.<br>You ",
                  "can annotate the tree with a particular character",
                  " by using the 'Select' button.")),
      fluidRow(column(12,
                      DT::dataTableOutput(ns("analysis_all_chars")),
                      style = style)),
    ),
    conditionalPanel(paste0("input[\'",
                            ns_lists("analysis_tree_radio"),
                            "\'] == 'Strict Consensus' || input[\'",
                            ns_lists("analysis_tree_radio"),
                            "\'] == 'Majority Consensus'"),
                     HTML(paste0("All characters from the dataset ",
                                 "are listed here. Annotation is not",
                                 " supported for consensus trees.")),
                     fluidRow(column(12,
                                     DT::dataTableOutput(ns(table_name)),
                                     style = style)))
  )
}

#' Server for Analysis
#'
#' @param id Id of module
#' @param my_vals Reactive values
#' @param cache Cache object
#' @param data_upload_root_drop Root from data upload page
#' @param table_button Table button reactive value
#' @param tree_to_listen Tree to do analysis on
#' @param paup_is_weighted Whether or not PAUP* is weighted
#'
#' @return List of reactive values
analysis_server <- function(id,
                            my_vals,
                            cache,
                            data_upload_root_drop,
                            table_button,
                            tree_to_listen,
                            paup_is_weighted) {
  moduleServer(id, function(input, output, session) {

    col_start <- 5

    # Set analysis root
    observe({
      if (my_vals$data_is_loaded) {
        leaves <- names(my_vals[["data"]])[col_start:ncol(my_vals[["data"]])]
        leaves <- leaves[order(leaves)]

        output$analysis_root_drop_html <- renderUI({
          ns <- session$ns
          fluidRow(column(12, style = "margin-bottom:-20px;",
                          selectInput(ns("analysis_root_drop"),
                                      "Root:",
                                      choices = leaves,
                                      selected = data_upload_root_drop())))
        })

      }
    })

    # Disallow root selection if a root was required for data input
    observe({
      if (my_vals$is_on_analysis_page && (my_vals$paup_finished ||
                                            my_vals$analysis_upload_finished)) {
        to_listen <- tree_to_listen() # Not totally sure why need this
        isolate({
          ns <- session$ns
          if ("chartype" %in% names(my_vals[["data"]]) &&
                "irreversible" %in% my_vals[["data"]][["chartype"]]) {
            updateTextInput(session, ns("analysis_root_drop"),
                            value = data_upload_root_drop())
            shinyjs::disable("analysis_root_drop")
          }
        })
      }

    })


    # Observe table buttons
    observeEvent(table_button(), {
      # Identify button
      splt <- strsplit(table_button(), "_")[[1]]
      char_to_put <- splt[3]

      # Get tree to use
      tree_to_use <- get_tree_to_use(isolate(tree_to_listen()),
                                     my_vals,
                                     cache,
                                     isolate(data_upload_root_drop()),
                                     isolate(paup_is_weighted()),
                                     input$analysis_metrics)
      if (is.null(tree_to_use)) return()

      p3 <- make_tree_plot_wrapper(tree_to_use,
                                   cache,
                                   input$analysis_root_drop,
                                   input$analysis_show_edge_ids,
                                   input$analysis_show_edge_support,
                                   input$analysis_show_weighted_edge_support,
                                   input$analysis_root_on_left,
                                   input$analysis_height,
                                   input$analysis_width,
                                   char_to_put)

      render_plotly <- plotly::renderPlotly({
        # To suppress benign warning, follow instructions at
        #   https://stackoverflow.com/questions/70986061/suppress-warning-
        #   heatmap-objects-dont-have-these-attributes-mode
        store_warn <- getOption("warn")
        options(warn = -1)

        # Restore warnings, delayed once plot renders
        shinyjs::delay(expr = ({
          options(warn = store_warn)
        }), ms = 200)
        p3
      })
      attr(render_plotly, "outputArgs") <- list(height = input$analysis_height)
      output[["analysis_tree_plotly"]] <- renderUI(render_plotly)

    })

    # Analysis tree computation
    observe({
      t <- tree_to_listen()
      if (t$set == "none") return()

      # Get tree to use
      tree_to_use <- get_tree_to_use(tree_to_listen(),
                                     my_vals,
                                     cache,
                                     isolate(data_upload_root_drop()),
                                     isolate(paup_is_weighted()),
                                     input$analysis_metrics)
      if (is.null(tree_to_use)) return()
      tree_to_do <- tree_to_use$tree_to_do
      tree <- tree_to_use$tree
      set <- tree_to_use$set

      # Make plot
      p3 <- make_tree_plot_wrapper(tree_to_use,
                                   cache,
                                   input$analysis_root_drop,
                                   input$analysis_show_edge_ids,
                                   input$analysis_show_edge_support,
                                   input$analysis_show_weighted_edge_support,
                                   input$analysis_root_on_left,
                                   input$analysis_height,
                                   input$analysis_width,
                                   NULL)


      render_plotly <- plotly::renderPlotly({
        # To suppress benign warning, follow instructions at
        #   https://stackoverflow.com/questions/70986061/suppress-warning-
        #   heatmap-objects-dont-have-these-attributes-mode
        store_warn <- getOption("warn")
        options(warn = -1)

        # Restore warnings, delayed once plot renders
        shinyjs::delay(expr = ({
          options(warn = store_warn)
        }), ms = 200)
        p3
      })
      attr(render_plotly, "outputArgs") <- list(height = input$analysis_height)
      output[["analysis_tree_plotly"]] <- renderUI(render_plotly)
      output$analysis_tree_title <- renderUI(h4(tree_to_do))

      is_consensus <- tree_to_do %in% c("Strict Consensus",
                                        "Majority Consensus")
      if (is_consensus) cache_id <- NULL # Haven't tested for this
      else cache_id <- paste0(set, "_", tree_to_do)


      code <- {
        run_parsimony_on_each_char(tree, cache[["cache"]][["char_reps"]])
      }
      parsimony_cache <- cache_wrapper(cache,
                                       paste0(cache_id, "_parsimony"), code)

      if (!is_consensus) {

        # Do incompatible characters
        annotation_info <- paste0("<br>You can annotate the tree ",
                                  "with a particular character by ",
                                  "using the 'Select' button.")
        incompat_chars <- find_incompat_chars(cache[["cache"]][["char_reps"]],
                                              parsimony_cache)
        if (!isolate(paup_is_weighted()))
          str <- paste0("There are ",
                        nrow(incompat_chars),
                        " incompatible characters on this tree.",
                        annotation_info)
        else
          str <- paste0("There are ",
                        nrow(incompat_chars),
                        " incompatible characters (",
                        sum(incompat_chars$weight),
                        " accounting for weighting) on this tree.",
                        annotation_info)
        output$analysis_incompat_char_text_html <- renderUI(HTML(str))
        incompat_chars <- add_buttons_to_table(incompat_chars,
                                               "incompatchars",
                                               "id")

        output$analysis_incompat_char_table <-
          DT::renderDataTable(make_datatable(incompat_chars))

        # Find enforcing characters
        if (!ape::is.binary(tree)) enforcing_chars <-
          data.frame() # Not computed for now
        else enforcing_chars <-
          cache_wrapper(cache,
                        paste0(cache_id, "_enf_char"),
                        {find_enforcing_chars(tree, # Do before rooting tree
                                              cache[["cache"]][["char_reps"]],
                                              parsimony_cache)})

        enforcing_chars <-
          enforcing_chars[order(as.integer(enforcing_chars$edge_id)), ]
        enforcing_chars <- add_buttons_to_table(enforcing_chars,
                                                "enforcingchars",
                                                "char_id")
        output$analysis_enforcing_chars <- DT::renderDataTable({
          make_datatable(enforcing_chars)
        })

        # Make all characters
        tabtouse <-
          my_vals[["data_viz"]][, !grepl("span",
                                         names(my_vals[["data_viz"]]))]
        ps_scores <- unlist(lapply(parsimony_cache, function(x) x$score_iq))
        ps_scores_w <- unlist(lapply(parsimony_cache, function(x) x$score_iq_w))
        tabtouse <- cbind(tabtouse[, 1:(col_start - 1)],
                          data.frame(ps_score = ps_scores,
                                     ps_score_weighted = ps_scores_w),
                          tabtouse[, col_start:ncol(tabtouse)])
        all_chars <- add_buttons_to_table(tabtouse, "allchars", "id")

        output$analysis_all_chars <-
          DT::renderDataTable(make_datatable(all_chars))

      } else {

        # Characters incompatible on any tree
        parsimony_caches <- lapply(cache[["cache"]][["trees"]][[4]],
                                   function(x) x$parsimony_cache)
        res <- find_chars_incompat_any_tree(cache[["cache"]][["char_reps"]],
                                            parsimony_caches)
        output$analysis_incompat_on_any_char_text_html <- renderUI({
          HTML(paste0("There are ",
                      nrow(res),
                      " characters that are incompatible",
                      " on at least one of the trees:"))
        })
        output$analysis_incompat_on_any_char_table <-
          DT::renderDataTable(make_datatable(res))

        # All characters
        all_chars <- my_vals[["data_viz"]]
        all_chars <- all_chars[, !grepl("span", names(all_chars))]
        output$analysis_consensus_all_chars <-
          DT::renderDataTable(make_datatable(all_chars))
      }

    })

    return(list(analysis_metrics = reactive(input$analysis_metrics),
                analysis_root_drop = reactive(input$analysis_root_drop)))
  })
}
