#' UI Sidebar for PAUP*
#'
#' @param id Id of module
#'
#' @return A shiny tagList for the PAUP* UI sidebar
paup_ui_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    h4("PAUP* Options"),

    fluidRow(column(12,
                    style = "margin-bottom:-20px;",
                    checkboxInput(ns("paup_is_weighted"),
                                  "Use Weights",
                                  TRUE))),
    fluidRow(column(12,
                    checkboxInput(ns("paup_is_exhaustive"),
                                  paste0("Run in exhaustive mode ",
                                         "(requires \u2264 12 taxa)"),
                                  TRUE))),
    fluidRow(column(12,
                    radioButtons(ns("paup_keep"),
                                 NULL,
                                 choices = c("Retain Single Best Tree",
                                             "Retain All Trees Below Score..."),
                                 inline = FALSE))),
    conditionalPanel(paste0("input[\'",
                            ns("paup_keep"),
                            "\'] === \"Retain All Trees Below Score...\""),
                     fluidRow(column(6,
                                     textInput(ns("paup_keep_score"),
                                               NULL,
                                               428)))),
    conditionalPanel("!output.paup_generated",
                     fluidRow(column(6,
                                     actionButton(ns("paup_generate_nexus"),
                                                  "Generate Nexus")))),
    conditionalPanel("output.paup_generated",
                     downloadButton(ns("paup_download_nexus"),
                                    "Download Nexus")),
    conditionalPanel("!output.paup_finished",
                     fluidRow(column(6, actionButton(ns("paup_run"),
                                                     "Run PAUP*")))),
    conditionalPanel("output.paup_finished",
                     downloadButton(ns("paup_download"),
                                    "Download PAUP* Trees"))
  )
}

#' UI for PAUP*
#'
#' @param id Id of module
#'
#' @return UI for PAUP*
paup_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    conditionalPanel("output.paup_finished",
                     wellPanel(h4("PAUP* Output"),
                               htmlOutput(ns("paup_output")))),
    conditionalPanel("output.paup_generated",
                     wellPanel(h4("Nexus File (PAUP* Input)"),
                               htmlOutput(ns("paup_input"))))
  )
}

#' Server for PAUP*
#'
#' @param id Id of module
#' @param my_vals Reactive variables
#' @param cache Cache object
#' @param data_upload_root_drop Root from data upload page
#'
#' @return List of reactive variables
paup_server <- function(id,
                        my_vals,
                        cache,
                        data_upload_root_drop) {
  moduleServer(id, function(input, output, session) {

    col_start <- 5

    generateNexusString <- function() {
      keep <- NULL
      is_exhaustive <- NULL
      is_weighted <- NULL

      # Get inputs
      if (input$paup_keep != "Retain Single Best Tree")
        keep <- input$paup_keep_score
      is_exhaustive <- input$paup_is_exhaustive
      is_weighted <- input$paup_is_weighted
      root <- data_upload_root_drop()
      paup_is_exhaustive <- input$paup_is_exhaustive

      # Get character representations
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

      # Obtain Nexus string
      has_ancestral <- !is.null(root)
      taxa <- names(my_vals[["data"]])[col_start:ncol(my_vals[["data"]])]
      if (has_ancestral) taxa <- taxa[taxa != root]
      nexus_str <- write_nexus(char_reps,
                               taxa,
                               is_weighted,
                               is_exhaustive,
                               keep,
                               root = root)
      nexus_str
    }

    observeEvent(input$paup_generate_nexus, {
      nexus_str <- generateNexusString()
      if (substr(nexus_str[1], 1, 7) == "ERROR: ") {
        assertthat::assert_that(length(nexus_str) == 1)
        generate_error(substr(nexus_str, 8, nchar(nexus_str[1])))
        return()
      }
      my_vals$paup_generated <- TRUE
      paup_input <- gsub("\n", "<br>", nexus_str)
      paup_input <- gsub("\t", "&emsp;", paup_input)
      output$paup_input <- renderUI({
        HTML(paup_input)
      })
    })

    observeEvent(input$paup_run, {
      is_exhaustive <- input$paup_is_exhaustive
      nexus_str <- generateNexusString()
      if (substr(nexus_str[1], 1, 7) == "ERROR: ") {
        assertthat::assert_that(length(nexus_str) == 1)
        generate_error(substr(nexus_str, 8, nchar(nexus_str[1])))
        return()
      }
      my_vals$paup_generated <- TRUE

      # Run PAUP*
      run_paup(nexus_str)
      show_loading_for_paup(is_exhaustive)

      # Show input/output files for PAUP*
      paup_strings <- get_paup_output_strings()
      output$paup_input <- renderUI({
        HTML(paup_strings$paup_input)
      })
      output$paup_output <- renderUI({
        HTML(paup_strings$paup_output)
      })

      # Mark PAUP* as finished
      my_vals$paup_finished <- TRUE

    })

    # Download trees
    output$paup_download <- downloadHandler(
      filename = function() {
        paste("paup_trees_", Sys.Date(), ".trees", sep = "")
      },
      content = function(file) {
        tmp <- tempdir()
        paup_out_trees <- file.path(tmp, "paup_out.trees")
        file.copy(paup_out_trees, file)
      }
    )

    # Download Nexus file
    output$paup_download_nexus <- downloadHandler(
      filename = function() {
        "paup_nexus.nex"
      },
      content = function(file) {
        nexus_str <- generateNexusString()
        assertthat::assert_that(substr(nexus_str[1], 1, 7) != "ERROR: ")
        sink(file)
        cat(nexus_str)
        sink()
      }
    )



    # Enable/disable whether PAUP can run
    observe({
      if (!my_vals$data_is_loaded) {
        shinyjs::disable("paup_run")
      } else {
        shinyjs::enable("paup_run")
      }
    })

    # Grey out PAUP* exhaustive option box
    observe({
      if (my_vals$data_is_loaded && ncol(my_vals[["data"]])
          - col_start + 1 > 12) {
        updateCheckboxInput(session, "paup_is_exhaustive", value = FALSE)
        shinyjs::disable("paup_is_exhaustive")
      } else {
        shinyjs::enable("paup_is_exhaustive")
      }
    })

    # If data changes, set PAUP* and upload to not finished (reset it!)
    observe({
      q <- my_vals[["data"]]
      my_vals$paup_finished <- FALSE
      my_vals$paup_generated <- FALSE
      my_vals$analysis_upload_finished <- FALSE
      cache[["cache"]] <- list() # Reset cache
    })

    # Reset PAUP* if options change
    observeEvent(list(data_upload_root_drop(),
                      input$paup_is_exhaustive,
                      input$paup_is_weighted,
                      input$paup_keep,
                      input$paup_keep_score),
                 {
                   my_vals$paup_finished <- FALSE
                   my_vals$paup_generated <- FALSE
                   my_vals$analysis_upload_finished <- FALSE
                   cache[["cache"]] <- list()
                 })


    return(list(paup_is_weighted = reactive(input$paup_is_weighted)))
  })
}
