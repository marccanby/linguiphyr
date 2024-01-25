#' UI Sidebar for Data Upload
#'
#' @param id Id of module
#'
#' @return A shiny tagList for the data upload UI sidebar
data_upload_ui_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Data Options"),
    fluidRow(column(12,  htmlOutput(ns("data_root_drop_html")))),
    fluidRow(column(12, radioButtons(ns("data_poly_options"),
                                     "How to handle polymorphic states?",
                                     choices = c("Replace all with ?",
                                                 "Replace unique with ?",
                                                 "Replace with majority"))),
    ),
    h4("Data View Options"),
    fluidRow(column(12, checkboxInput(ns("data_show_poly"),
                                      "Show polymorphic states",
                                      TRUE))),
    fluidRow(column(12, checkboxInput(ns("data_show_analytics"),
                                      "Show character analytics",
                                      TRUE)))
  )

}

#' UI for Data Upload
#'
#' @param id Id of module
#'
#' @return UI for Data Upload
data_upload_ui <- function(id) {
  ns <- NS(id)
  style <- "height:500px; overflow-y: scroll;overflow-x: scroll;"
  dc <- "data_clade_table"
  fluidPage(
    wellPanel(
              tabsetPanel(
                tabPanel("Data Input",
                  h4("Data Input"),
                  HTML(paste0("You can upload a CSV file containing",
                              " the character data below.
                              The first row should contain column/language ",
                              "names, and the following rows should ",
                              "contain individual characters.
                              The columns may be in any order, but there are 4",
                              " special columns (2 are required and 2 are ",
                              "optional):<br>
                              <ul>
                              <li> <b><tt>id</tt> (required):</b> The id",
                              " for a character (can be an integer, or ",
                              "string such as <tt>p01</tt>)
                              <li> <b><tt>feature</tt> (required):</b>",
                              " The name of the character (string value)
                              <li> <b><tt>chartype</tt> (optional):</b>",
                              " The type of character, which must be one",
                              " of {<tt>standard</tt>, <tt>irreversible</tt>,",
                              " or <tt>custom:...</tt>}. If not provided, all",
                              " characters are presumed <tt>standard</tt>.",
                              " See the \'Notes\' tab for information",
                              " on these options.
                              <li> <b><tt>weight</tt> (optional):</b>",
                              " The weight of the character (integer value).",
                              " If not provided, all characters are given ",
                              "weight <tt>1</tt>.
                              </ul>Unknown values should be represented with",
                              " a <tt>?</tt>. There can be no empty ",
                              "cells in the dataset.")),
                  br(),
                  br(),
                  fluidRow(column(6, fileInput(ns("data_input"),
                                               label = NULL,
                                               multiple = FALSE,
                                               accept = NULL))),
                  HTML(paste0("After uploading the data, you can adjust",
                              " preprocessing options in the left sidebar."))
                ),
                tabPanel("Notes",
                  h4("Notes"),
                  h5("Character Types"),
                  HTML(
                       paste0(
                              "<ul>
                    <li><b>Standard:</b> A character marked",
                              " as <tt>standard</tt> enforces no ",
                              "directionality or weighting on transitions",
                              " between character states.
                    <li><b>Irreversible:</b> A character marked",
                              " as <tt>irreversible</tt> does not permit ",
                              "a back-transition to state <tt>0</tt>. If ",
                              "this type is selected, then a root language ",
                              "must be specified in the drop-down menu, and ",
                              "the root must exhibit the ancestral state ",
                              "(<tt>0</tt>) for each irreversible character.
                    <li><b>Custom:</b> A <tt>custom</tt> character ",
                              "allows you to specify each possible transition ",
                              "and its cost. In this case, every allowed ",
                              "transition must be declared; any transition ",
                              "not declared is presumed impossible. ",
                              "To declare a custom character, the ",
                              "<tt>chartype</tt> column must begin with ",
                              "<tt>custom:</tt> followed by each transition ",
                              "(separated by <tt>,</tt> or <tt>;</tt>), which ",
                              "must be of the form <tt>a>b(c:1)</tt> (the ",
                              "<tt>c</tt>, for <b>c</b>ost, may be replaced ",
                              "with <tt>w</tt>, for <b>w</b>eight). Spacing ",
                              "does not matter, and if no weight is provided ",
                              "for a transition it is presumed to have weight ",
                              "<tt>1</tt>. Examples of custom character ",
                              "declarations are:
                    <ul>
                    <li><tt>custom:0>1(c:3); 0>2(c:1); 0>3(c:3);1>2(c:3);",
                              " 1>3(c:5); 3>2(c:1)</tt>
                    <li><tt>custom:0>1,1>0,0>2</tt>
                    <li><tt>custom:0>1(c:5), 1>0</tt>
                    </ul>
                    </ul>"))
                ),
                id = ns("dataInputTabsetPanel0")
              )),

    conditionalPanel(
                     paste0("output.data_is_loaded && input[\'",
                            ns("data_show_analytics"),
                            "\']"),
                     wellPanel(style = "overflow-y:scroll; max-height: 400px",
                       tabsetPanel(
                         tabPanel("Parsimony-Uninformative Characters",
                           h4("Parsimony-Uninformative Characters"),
                           fluidRow(
                                    column(12,
                                           htmlOutput(ns("data_uninf_char_html")
                                           )))
                         ),
                         tabPanel("Clade Analysis",
                                  h4("Clade Analysis"),
                                  HTML(
                                       paste0("The following checkbox ",
                                              "set allows you to select a ",
                                              "particular clade. The table ",
                                              "that follows shows each ",
                                              "character that supports ",
                                              "that clade.
                                              These are characters that have",
                                              " the same state for each of ",
                                              "the languages selected and ",
                                              "different states for each ",
                                              "language not selected.")),
                                  br(),
                                  br(),
                                  htmlOutput(ns("data_lang_checkbox_html")),
                                  fluidRow(column(12,
                                                  DT::dataTableOutput(ns(dc)),
                                                  style = style))),
                         id = ns("dataInputTabsetPanel")
                       )
                     )),

    conditionalPanel("true",
      shinydashboard::box(width = 12,
        column(12,
               DT::dataTableOutput(ns("data_table")),
               style = style),
      )
    )
  )
}

#' Server for Data Upload Module
#'
#' @param id Id of module
#' @param my_vals Reactive values
#'
#' @return List with reactive values
data_upload_server <-  function(id, my_vals) {
  moduleServer(id, function(input, output, session) {

    col_start <- 5

    # Upload dataset
    observeEvent(input$data_input, {
      data <- read_and_validate_data(input$data_input$datapath)

      if (typeof(data) == "list") {
        if ("irreversible" %in% data[["chartype"]] ||
              any(grepl("^custom:.+$", data[["chartype"]]))) {
          leaves <- names(data)
          leaves <- leaves[!(leaves %in% c("id",
                                           "chartype",
                                           "weight",
                                           "feature"))]
          leaves <- leaves[order(leaves)]
          if ("PIE" %in% leaves) selected <- "PIE"
          else selected <- leaves[1]
          output$data_root_drop_html <- renderUI({
            ns <- session$ns
            selectInput(ns("data_root_drop"),
                        "Select Root:",
                        choices = leaves,
                        selected = selected)
          })
        }

        my_vals[["original_data"]] <- data
        my_vals$data_is_loaded <- TRUE

      } else {
        my_vals$data_is_loaded <- FALSE
        generate_error(data)
      }

    })

    # Update characters that support a clade
    observe({
      checkbox_selection <- input$data_lang_checkbox
      if (!is.null(checkbox_selection) && length(checkbox_selection) > 1) {
        df <- find_chars_supporting_clade(checkbox_selection, my_vals[["data"]])
      } else {
        df <- NULL
      }

      output$data_clade_table <- DT::renderDataTable({
        DT::datatable(df,
                      editable = FALSE,
                      rownames = FALSE,
                      escape = FALSE,
                      selection = "none",
                      options = list(paging = FALSE))
      })
    })

    # Update data frame & calculate uninformative characters
    observe({
      if (my_vals$data_is_loaded) {
        df <- my_vals[["original_data"]]
        doubledf <- preprocess_data(df,  input$data_poly_options)
        df <- doubledf$df
        df_with_poly <- doubledf$df_with_poly
        my_vals[["data"]] <- df

        output[["data_lang_checkbox_html"]] <- renderUI({
          ns <- session$ns
          checkboxGroupInput(inputId = ns("data_lang_checkbox"),
                             choices = names(df)[col_start:ncol(df)],
                             label = NULL,
                             inline = TRUE)
        })

        if (input$data_show_poly) df_viz <- df_with_poly
        else df_viz <- df

        if (input$data_show_analytics) {
          lst <- find_uninformative_characters(df)
          uninf_chars <- lst$uninf
          numbigstates <- lst$numbigstates
          output[["data_uninf_char_html"]] <- renderUI({
            HTML(get_uninf_char_text(uninf_chars, df))
          })

          is_inf <- !is.element(seq_len(nrow(df_viz)), uninf_chars)
          df_viz[["<span style=\"color:red\">is_informative</span>"]] <-
            paste0("<span style=\"color:red\">", is_inf, "</span>")
          df_viz[["<span style=\"color:red\">num_big_states</span>"]] <-
            paste0("<span style=\"color:red\">", numbigstates, "</span>")
          df_viz[["<span style=\"color:red\">num_poly_states</span>"]] <-
            paste0("<span style=\"color:red\">", doubledf$num_poly, "</span>")
          df_viz[["<span style=\"color:red\">max_poly_width</span>"]] <-
            paste0("<span style=\"color:red\">", doubledf$polywid, "</span>")
          num_special <- 4 # number of analytics columns
          nms <- names(df_viz)
          nms <- c(nms[1:(col_start - 1)],
                   nms[(length(nms) - num_special + 1):length(nms)],
                   nms[col_start:(length(nms) - num_special)])

          df_viz <- df_viz[, nms]
        }

        my_vals[["data_viz"]] <- df_viz
      }
    })

    output$data_table <- DT::renderDataTable({
      DT::datatable(my_vals[["data_viz"]],
                    editable = FALSE,
                    rownames = FALSE,
                    escape = FALSE,
                    selection = "none",
                    extensions = "Buttons",
                    options = list(paging = FALSE, dom = "Bfrtip",
                                   buttons = c("copy",
                                               "csv",
                                               "excel",
                                               "pdf",
                                               "print")))
    })

    return(list(root_drop = reactive(input$data_root_drop)))
  })
}
