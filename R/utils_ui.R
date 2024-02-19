#' Add buttons to data frame
#'
#' @param df Data frame
#' @param table_id Unique id for data frame
#' @param id_col Unique key column in data frame
#' @param label Label to put on button, default is "Select"
#'
#' @return Data frame with buttons added
add_buttons_to_table <- function(df, table_id, id_col, label = "Select") {
  if (nrow(df) == 0) return(df)

  buttons <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    onclick_action <- "Shiny.onInputChange(\"table_button\",  this.id)"
    buttons[i] <- paste(
      as.character(
                   shiny::actionButton(paste0(table_id,
                                              "_select_",
                                              df[[id_col]][i]),
                                       label = label,
                                       onclick = onclick_action))
    )
  }

  data.frame(Select = buttons, df)
}





#' Make DT::datatable from data frame
#'
#' @param data Data frame
#'
#' @return datatable
make_datatable <- function(data) {
  DT::datatable(data, editable = FALSE, rownames = FALSE, escape = FALSE,
                selection = "none",
                extensions = "Buttons",
                options = list(paging = FALSE, dom = "Bfrtip",
                               buttons = c("copy",
                                           "csv",
                                           "excel",
                                           "pdf",
                                           "print")))
}



#' Generate error message in Shiny
#'
#' @param msg Message
#' @param frame Data frame (optional)
#'
#' @return Nothing
#' @import shiny
generate_error <- function(msg, frame = NULL) {
  if (is.null(frame)) {
    showModal(modalDialog(fluidPage(h3(strong("Error!"), align = "center"),
                                    hr(),
                                    HTML(paste0(msg)))))
  } else {
    showModal(modalDialog(fluidPage(h3(strong("Error!"), align = "center"),
                                    hr(),
                                    paste0(msg),
                                    hr(),
                                    DT::renderDataTable({
                                      DT::datatable(
                                        frame,
                                        editable = FALSE,
                                        rownames = FALSE,
                                        escape = FALSE,
                                        selection = "none",
                                        options = list(paging = FALSE)
                                      )
                                    }))))
  }
}







#' Get text for parsimony-uninformative characters
#'
#' @param uninf_chars Parsimony-uninformative characters
#' @param df Data frame with character information
#'
#' @return Text to display regarding parsimony-uninformative characters
get_uninf_char_text <- function(uninf_chars, df) {
  if (length(uninf_chars) == 0) return(paste0("There are no parsimony-",
                                              "uninformative characters in",
                                              " the dataset. Great!"))

  str <- paste0("There are ",
                length(uninf_chars),
                " parsimony-uninformative characters in the dataset.",
                " They are:<br>")
  str <- paste0(str, "<ul>")
  for (char in uninf_chars) {
    str <- paste0(str, "<li>", df[char, 1], "&emsp;", df[char, 2], "</li>")
  }
  str <- paste0(str, "</ul>You can sort the table below by the column",
                " 'is_informative' to see the states for these characters.")

  str
}





#' Show PAUP* loading message and progress bar
#'
#' @param is_exhaustive Whether or not PAUP* is being run in exhaustive mode
#'
#' @return Nothing
show_loading_for_paup <- function(is_exhaustive) {
  if (is_exhaustive) {
    showModal(modalDialog(fluidPage("Running....."), footer = NULL))
    Sys.sleep(4)
    withProgress(message = "Running PAUP*", min = 0, max = 1, value = 0, {
      mx <- 0
      while (mx != 1) {
        Sys.sleep(1)
        tmp <- tempdir()
        paup_run <- file.path(tmp, "paup_run.txt")
        prog <- readChar(paup_run, file.info(paup_run)$size)
        nums <- stringr::str_extract_all(prog, "\\d*\\.\\d*%")[[1]]
        if (length(nums) == 0) break
        final <- nums[length(nums)]
        final <- as.numeric(substr(final, 1, nchar(final) - 1))
        final <- final / 100
        diff <- final - mx
        incProgress(diff)
        mx <- mx + diff
      }
    })
    removeModal()
  } else {
    showModal(modalDialog(fluidPage("Running....."), footer = NULL))
    error_msg <- "Execution terminated due to errors."
    success_msg <- "Heuristic search completed"
    success_msg2 <- "Search terminated prematurely"
    paup_not_found <- FALSE
    while (TRUE) {
      Sys.sleep(1)
      paup_strings <- get_paup_output_strings()
      paup_output <- paup_strings$paup_output
      paup_run <- paup_strings$paup_run
      if (grepl(error_msg, paup_output) ||
            grepl(success_msg, paup_output) ||
            grepl(success_msg2, paup_output)) break
      if (grepl("command not found", paup_run)) {
        paup_not_found <- TRUE
        break
      }
    }
    removeModal()
    if (paup_not_found) {

      markdown <- paste0("<br><br><ol><li>Go to <a href=
      \"http://phylosolutions.com/paup-test/\">
      http://phylosolutions.com/paup-test/</a>.<br>
      <li>For Mac:<br>
      <ol><li>Click on <code>paup4a168_osx.gz</code>, which will download a
      zipped file. Unzip it, and rename the resulting <code>paup4a168_osx</code>
      file to <code>paup</code>.<br>
      <li>Move this file to a new directory
      (e.g. <code>~/Documents/paup/</code>) on your computer.<br>
      <li>Navigate to this directory, and run <code>chmod a+x paup</code>
      to enable executable permissions on the file.<br>
      <li>To make this binary available to R, run the following
      command in R:<br>
      <code>
      Sys.setenv(PATH = paste(\"~/Documents/paup/:\",
      Sys.getenv(\"PATH\"), sep=\"\"))
      </code><br>
      Replace <code>~/Documents/paup/</code> with the directory
      where you put the PAUP* binary.
      This will only make the binary available for your current R session. If
      you want to make it permanently available, you can add this line to your
      R profile file,
      which is typically located at <code>~/.Rprofile</code> or
      <code>~/.Rprofile.site</code>.<br>
      </ol>
      <li>For Windows:<br>
      <ol><li>Click on <code>paup4-setup.msi</code>, follow the directions to
      install it.<br></ol></ol>")

      github_link <- "https://github.com/marccanby/linguiphyr/tree/main"

      showModal(modalDialog(fluidPage(HTML(paste0("PAUP* binary not found. ",
                                                  "Make sure you have followed",
                                                  " the installation",
                                                  " instructions on the",
                                                  " <a href=\"", github_link,
                                                  "\">Github page</a>:")),
                                      HTML(markdown)),
                            title = "Error!", size = "l"))
      return(FALSE)
    }
  }

  return(TRUE)
}





#' Make tree plot
#'
#' @param orig_tree Tree to display
#' @param root Root node to display tree at
#' @param do_edge_label Whether or not to label edges
#' @param node_states States to display at nodes if available, else NULL
#' @param enforcing_chars Characters that enforce edges if available, else NULL
#' @param show_root_at_left Whether or not to show root on left
#' @param message Message to display on plot
#' @param precomputed_edge Edge information
#' @param show_edge_support Whether or not to show edge support
#' @param show_weighted_edge_support Whether or not to show weighted edge
#'   support
#' @param height Height of plot
#' @param width Width of plot
#'
#' @return Plotly plot
make_treeplot <- function(orig_tree,
                          root,
                          do_edge_label,
                          node_states = NULL,
                          enforcing_chars = NULL,
                          show_root_at_left = FALSE,
                          message = NULL,
                          precomputed_edge = NULL,
                          show_edge_support = FALSE,
                          show_weighted_edge_support = FALSE,
                          height = 400,
                          width = 800) {
  # Assertions
  # Not sure we allow this, so just check for correctness if so
  assertthat::assert_that(!is.null(root))
  if (show_root_at_left) assertthat::assert_that(length(root) == 1)
  if (show_edge_support || show_weighted_edge_support) {
    assertthat::assert_that(!is.null(enforcing_chars))
  }

  # Prepare tree for plot
  tree <- ape::root(orig_tree, root, resolve.root = TRUE)
  orig_tree <- tree
  if (show_root_at_left) {
    tree <- ape::drop.tip(tree, tip = root)
    tree$root.edge <- 1
  }
  .data <- rlang::.data
  tree$edge.length <- rep(1, nrow(tree$edge))

  # Make tree
  if (show_root_at_left) p1 <- ggtree::ggtree(tree) + ggtree::geom_rootedge()
  else p1 <- ggtree::ggtree(tree)

  # Labels
  labels <- (paste0(" ", p1$data$label))[!is.na(p1$data$label)]
  p1$data$label[!is.na(p1$data$label)] <- labels

  if (!is.null(node_states)) {
    # So node_states has the states by bipartition basically.
    # We need to turn this into a vector in the order of "node": the
    #   label at "node".
    # This is not that hard.
    if (show_root_at_left) {
      root_id <- which(orig_tree$tip.label == root)
      root_state <- node_states$label[node_states$node == root_id]

      # Now save the state below the state above the root.
      root_parent <- node_states$parent[node_states$node == root_id]
      assertthat::assert_that(!(root_parent %in% node_states$node))
      below_root_parents <- node_states[node_states$parent == root_parent, ]
      assertthat::assert_that(nrow(below_root_parents) == 2)
      bool <- below_root_parents$node != root_id
      state_below_root_parent <- below_root_parents$label[bool]

      # Get node states
      edge_num <- node_states$edge_num[2:(nrow(node_states) - 1)]
      label <- node_states$label[2:(nrow(node_states) - 1)]
      node_states <- cbind(tree$edge,
                           data.frame(edge_num = edge_num,
                                      label = label))
      names(node_states) <- c("parent", "node", "edge_num", "label")
    }

    vec <- c()
    node_ids <- unique(c(node_states$parent, node_states$node))
    node_ids <- node_ids[order(node_ids)]
    # Believe this is based on it being rooted, which is currenltly an assertion
    assertthat::assert_that(length(node_ids) == nrow(node_states) + 1)
    for (i in node_ids) {
      if (!(i %in% node_states$node)) {
        # This had better be the root. Let's check. (But if we've removed
        #   the root by showing on left, don't need to do this.)
        if (!show_root_at_left) {
          rows_where_this_is_parent <- node_states[node_states$parent == i, ]
          assertthat::assert_that(nrow(rows_where_this_is_parent) == 2)

          # If this is not the case, would have to recursively check that one
          #   side has all the leaves in root...leave for future work...not
          #   sure would be an actual issue though.
          assertthat::assert_that(length(root) == 1)
          labsh <- tree$tip.label[rows_where_this_is_parent$node]

          # root must be one of these! the other is the rest of the tree.
          #   again, only works if root is a leaf, otherwise need to check
          # recursively.
          assertthat::assert_that(any(labsh == root))
          vec <- c(vec, "")
        } else {
          vec <- c(vec, state_below_root_parent)
        }
      } else {
        row_id <- which(node_states$node == i)
        annot <- node_states[row_id, "label"]
        vec <- c(vec, annot)
      }
    }
    node <- data.frame(as.data.frame(p1$data[, c("parent", "node")]),
                       state = vec)
    colnames(node) <- c("parent", "node", "state")
    is_leaf <- !(node[, 2] %in% node[, 1])
    node_leaf <- node
    node_leaf$state[!is_leaf] <- ""
    p1 <- p1 %<+% node_leaf +
      ggplot2::geom_text(ggplot2::aes(x = x,
                                      label = .data$state),
                         nudge_y = 0.25,
                         colour = "red")

    node <- data.frame(as.data.frame(p1$data[, c("parent", "node")]),
                       state = vec)
    colnames(node) <- c("parent", "node", "state2")
    node_nonleaf <- node
    node_nonleaf$state2[is_leaf] <- ""
    p1 <- p1 %<+% node_nonleaf +
      ggplot2::geom_text(ggplot2::aes(x = x,
                                      label = .data$state2),
                         nudge_x = 0.15,
                         colour = "red")
  }
  if (do_edge_label) {
    if (is.null(precomputed_edge)) {
      # Invariant no matter root
      edge <- make_edges(orig_tree)$edge
    } else {
      edge <- precomputed_edge
    }
    if (show_root_at_left) {
      edge <- cbind(tree$edge,
                    data.frame(edge_num = edge$edge_num[2:(nrow(edge) - 1)]))
    }
    names(edge) <- c("parent", "node", "edge_num")
    edge2 <- edge
    val <- edge2$edge_num[edge2$edge_num != " "]
    edge2$edge_num[edge2$edge_num != " "] <- paste0("(", val, ")")
    p1 <- p1 %<+% edge2 + ggplot2::geom_text(ggplot2::aes(x = .data$branch,
                                                          label = edge_num),
                                             nudge_y = -0.35,
                                             size = 3)
  } else {
    edge <- NULL
  }
  if (ape::is.binary(tree) &&
        (show_edge_support || show_weighted_edge_support)) {
    if (is.null(edge)) {
      if (is.null(precomputed_edge)) {
        # Invariant no matter root - so the edge ids are guaranteed to match
        #   up with the enforcing_chars
        edge <- make_edges(orig_tree)$edge
      } else {
        edge <- precomputed_edge
      }
      if (show_root_at_left) {
        edge <- cbind(tree$edge,
                      data.frame(edge_num = edge$edge_num[2:(nrow(edge) - 1)]))
      }
    }
    joined <- join_enf_chars_with_edge_table(enforcing_chars, edge)


    if (show_edge_support && show_weighted_edge_support) {
      joined$n <- paste0(joined$n, " | ", joined$sum)
      joined$n[joined$n == "  |  "] <- " " # Reset to empty
    }  else if (show_edge_support && !show_weighted_edge_support) {
      joined$n <- joined$n
    } else if (!show_edge_support && show_weighted_edge_support) {
      joined$n <- joined$sum
    } else {
      stop()
    }
    joined <- joined[, -4]
    colnames(joined) <- c("parent", "node", "num_support")

    p1 <- p1 %<+% joined +
      ggplot2::geom_text(ggplot2::aes(x = .data$branch,
                                      label = .data$num_support),
                         nudge_y = .25,
                         colour = "blue",
                         size = 3)
  }

  p3 <- plotly::ggplotly(p1,
                         height = height,
                         width = width) %>% plotly::layout(margin = list(
    l = 80,
    r = 80,
    b = 0,
    t = 0,
    pad = 0
  ))
  p3 <- p3 %>% plotly::add_trace(x = p1$data$x,
                                 y = p1$data$y,
                                 text = p1$data$label,
                                 mode = "text",
                                 cliponaxis = FALSE,
                                 textposition = "right")
  if (show_root_at_left) {
    x <- -1
    y <- p1$data$y[p1$data$x == 0]
    p3 <- p3 %>% plotly::add_trace(x = x,
                                   y = y,
                                   text = paste0(root, " "),
                                   mode = "text",
                                   cliponaxis = FALSE,
                                   textposition = "left")
    if (!is.null(node_states)) {
      p3 <- p3 %>% plotly::add_trace(x = x,
                                     y = y + 0.25,
                                     text = root_state,
                                     mode = "text",
                                     cliponaxis = FALSE,
                                     textposition = "right",
                                     textfont = list(color = "red"))
    }
  }
  p3 <- p3 %>% plotly::layout(showlegend = FALSE)
  if (!is.null(message)) {
    p3 <- p3 %>% plotly::add_annotations(
      x = -1,
      y = 12,
      xref = "x",
      yref = "y",
      text = message,
      xanchor = "left",
      font = list(color = "red"),
      showarrow = FALSE
    )
  }
  p3
}





#' UI Wrapper for make_treeplot
#'
#' @param tree_to_use Tree to plot
#' @param cache Cache object
#' @param root_drop Leaf to root tree at
#' @param show_edge_ids Whether or not to show edge ids
#' @param show_edge_support Whether or not to show edge support
#' @param show_weighted_edge_support Whether or not to show weighted
#'   edge support
#' @param root_on_left Whether or not root should show on left (i.e.
#'   as unrooted)
#' @param height Height of plot
#' @param width Width of plot
#' @param char_to_put Optionally, a character with which to annotate tree
#'
#' @return Tree plot
make_tree_plot_wrapper <- function(tree_to_use,
                                   cache,
                                   root_drop,
                                   show_edge_ids,
                                   show_edge_support,
                                   show_weighted_edge_support,
                                   root_on_left,
                                   height,
                                   width,
                                   char_to_put = NULL) {

  tree_to_do <- tree_to_use$tree_to_do
  tree  <-  tree_to_use$tree
  set  <-  tree_to_use$set

  is_consensus <- tree_to_do %in% c("Strict Consensus", "Majority Consensus")
  cache_id <- paste0(set, "_", tree_to_do)

  root_of_tree <- root_drop
  parsimony_cache <- cache_wrapper(cache, paste0(cache_id, "_parsimony"), {
    run_parsimony_on_each_char(tree, cache[["cache"]][["char_reps"]])
  })
  annotations <- NULL
  if (!is.null(char_to_put)) {
    # No annotations for consensus trees for now
    assertthat::assert_that(!is_consensus)
    idxh <- which(unlist(lapply(cache[["cache"]][["char_reps"]],
                                function(x) x$id == char_to_put)))
    char_rep <- cache[["cache"]][["char_reps"]][[idxh]]
    parsimony_rep <- parsimony_cache[[idxh]]

    # Get original edge table, which will be relevant if the root gets
    #   is to be switched
    if (("anc_taxon" %in% names(char_rep))) {
      edge_orig <- NULL
    } else {
      r <- tree$tip.label[order(tree$tip.label)][1]
      edge_orig <- make_edges(ape::root(tree,
                                        r,
                                        resolve.root = TRUE))$edge
    }

    annotations <- annotate_tree(ape::root(tree,
                                           root_of_tree,
                                           resolve.root = TRUE),
                                 char_rep,
                                 parsimony_rep,
                                 edge_orig)$edge

  }

  if (!ape::is.binary(tree)) {
    enforcing_chars <- data.frame() # not computed for now
  } else {
    code <- {
      find_enforcing_chars(ape::unroot(tree),
                           cache[["cache"]][["char_reps"]],
                           parsimony_cache = parsimony_cache)
    }
    enforcing_chars <- cache_wrapper(cache,
                                     paste0(cache_id, "_enf_char"),
                                     code) # Do this before rooting the tree
  }
  display_edge_ids <- show_edge_ids
  display_edge_support <- show_edge_support
  display_weighted_edge_support <- show_weighted_edge_support
  if (display_edge_support ||
        display_weighted_edge_support) echars <- enforcing_chars
  else echars <- NULL

  if (!is_consensus) {
    # Not sure whether necessary to deal with root, and don't actually think
    # result should be different by changing root, but doing just in case to
    # keep consistent with make_treeplot
    code <- {
      make_edges(ape::root(tree,
                           root_of_tree,
                           resolve.root = TRUE))$edge
    }
    precomputed_edge <- cache_wrapper(cache,
                                      paste0(cache_id,
                                             paste0("_edge_", root_of_tree)),
                                      code)
  } else {
    precomputed_edge <- NULL
  }

  p3 <- make_treeplot(tree,
    root_of_tree,
    !is_consensus && display_edge_ids,
    annotations,
    echars,
    root_on_left,
    message = char_to_put,
    precomputed_edge = precomputed_edge,
    show_edge_support = display_edge_support,
    show_weighted_edge_support = display_weighted_edge_support,
    height = height,
    width = width
  )
  p3
}
