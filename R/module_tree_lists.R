#' UI for Tree Lists
#'
#' @param id Id of module
#'
#' @return Shiny tagList for Tree Lists UI
analysis_tree_lists_ui <- function(id) {
  ns <- NS(id)

  tagList(
    conditionalPanel("output.paup_finished",
                     h4("PAUP* Trees"),
                     htmlOutput(ns("analysis_tree_radio_html"))),

    h4("Reference Trees"),
    fluidRow(column(12,
                    style = "margin-bottom:-20px;",
                    fileInput(ns("analysis_input"),
                              label = NULL,
                              multiple = FALSE,
                              accept = NULL))),
    conditionalPanel("output.analysis_upload_finished",
                     htmlOutput(ns("analysis_treeupload_radio_html")))
  )

}

#' Server for Tree Lists
#'
#' @param id Id of module
#' @param my_vals Reactive variables
#' @param cache Cache object
#' @param data_upload_root_drop Root from data upload page
#' @param paup_is_weighted Whether or not PAUP* is weighted
#' @param analysis_metrics List of analysis metrics
#'
#' @return Tree to be rendered
analysis_tree_lists_server <- function(id,
                                       my_vals,
                                       cache,
                                       data_upload_root_drop,
                                       paup_is_weighted,
                                       analysis_metrics) {
  moduleServer(id, function(input, output, session) {

    col_start <- 5

    # Upload file for analysis trees
    observeEvent(input$analysis_input, {
      showModal(modalDialog(fluidPage("Loading....."), footer = NULL))
      my_vals$analysis_upload_finished <- FALSE
      leafset <- names(my_vals[["data"]])[col_start:ncol(my_vals[["data"]])]

      # First get character representations - if there is an error in
      #   generating these, then we won't be able to analyze anything.
      #   It's possible they were already made in PAUP*, if that was run
      #   first, but if going straight to upload, it may not be.
      root <- data_upload_root_drop()
      char_reps <- cache_wrapper(cache, "char_reps", {
        make_character_representations(root,
                                       my_vals[["data"]],
                                       neutralize_custom = FALSE,
                                       overwrite_custom_weight = FALSE)
      })
      if (inherits(char_reps, "data.frame")) { # Error
        generate_error("The following errors were identified:", char_reps)
        cache[["cache"]][["char_reps"]] <- NULL
        return()
      } else {
        assertthat::assert_that(inherits(char_reps, "list"))
      }

      code <- {
        read_and_name_trees_file(input$analysis_input$datapath,
                                 cache[["cache"]][["char_reps"]],
                                 leafset = leafset,
                                 is_weighted = paup_is_weighted(),
                                 do_unroot = TRUE)
      }
      trees <- read_and_name_trees_cache(cache,
                                         "trees_upload",
                                         code,
                                         names_to_do = analysis_metrics())
      if (typeof(trees) == "character") {
        generate_error(trees)
        return()
      }
      my_vals$analysis_upload_finished <- TRUE

      removeModal()

    })

    # Set upload radio buttons
    observe({
      x <- my_vals$analysis_upload_finished
      if (!x) return()

      names_to_do <- analysis_metrics()

      isolate({
        leafset <- names(my_vals[["data"]])[col_start:ncol(my_vals[["data"]])]

        code <- {
          read_and_name_trees_file(input$analysis_input$datapath,
                                   cache[["cache"]][["char_reps"]],
                                   leafset = leafset,
                                   is_weighted = paup_is_weighted(),
                                   do_unroot = TRUE)
        }
        trees <- read_and_name_trees_cache(cache,
                                           "trees_upload",
                                           code,
                                           names_to_do = names_to_do)

        cache[["cache"]][["analysis_treeupload_radio_choices"]] <- trees$names
        output$analysis_treeupload_radio_html <- renderUI({
          ns <- session$ns
          radioButtons(ns("analysis_treeupload_radio"),
                       NULL,
                       choices = trees$names,
                       selected = trees$names[1],
                       inline = FALSE)
        })
      })
    })

    # Set PAUP radio buttons
    observe({
      x <- my_vals$paup_finished
      y <- my_vals$is_on_analysis_page
      if (!(x && y)) return()

      showModal(modalDialog(fluidPage("Loading....."), footer = NULL))

      names_to_do <- analysis_metrics()

      isolate({
        char_reps <- cache[["cache"]][["char_reps"]]

        insert_root <- data_upload_root_drop()

        code <- {
          read_and_name_trees_paup(char_reps,
                                   paup_is_weighted(),
                                   do_unroot = TRUE,
                                   insert_root = insert_root)
        }
        trees <- read_and_name_trees_cache(cache,
                                           "trees",
                                           code,
                                           names_to_do)
        if (length(trees$trees) > 1) {
          consensus_trees <- make_consensus_trees(trees)
          tree_names <- make_tree_names(trees, c("Parsimony",
                                                 "Weighted Parsimony",
                                                 "Incompatibility",
                                                 "Weighted Incompatibility"))
          choices <- c(consensus_trees$names, trees$names)
        } else {
          choices <- trees$names
        }

        cache[["cache"]][["analysis_tree_radio_choices"]] <- choices
        output$analysis_tree_radio_html <- renderUI({
          ns <- session$ns
          radioButtons(ns("analysis_tree_radio"),
                       NULL,
                       choices = choices,
                       selected = trees$names[1],
                       inline = FALSE)
        })
      })

      removeModal()

    })

    # Set selected tree as reactive value
    tree_to_listen <- reactiveVal(list(set = "none", tree_to_do = NULL))

    # Listen to PAUP* trees
    observe({
      tree_to_do <- input$analysis_tree_radio
      if (!is.null(tree_to_do)) {
        ns <- session$ns
        choices <- cache[["cache"]][["analysis_treeupload_radio_choices"]]
        if (!is.null(choices))
          updateRadioButtons(session,
                             ns("analysis_treeupload_radio"),
                             choices = choices,
                             selected = character(0))
        tree_to_listen(list(set = "paup", tree_to_do = tree_to_do))
      }
    })

    # Listen to upload trees
    observe({
      tree_to_do <- input$analysis_treeupload_radio
      if (!is.null(tree_to_do)) {
        choices <- cache[["cache"]][["analysis_tree_radio_choices"]]
        ns <- session$ns
        if (!is.null(choices)) updateRadioButtons(session,
                                                  ns("analysis_tree_radio"),
                                                  choices = choices,
                                                  selected = character(0))
        tree_to_listen(list(set = "ref",
                            tree_to_do = tree_to_do))

      }

    })

    return(tree_to_listen)

  })
}
