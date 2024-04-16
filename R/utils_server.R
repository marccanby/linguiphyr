
#' cache wrapper
#'
#' @param cache cache object
#' @param cache_id Id of key in cache
#' @param code_block Code to execute
#'
#' @return Result of code
cache_wrapper <- function(cache, cache_id, code_block) {
  # Return result if it's cached
  if (!is.null(cache_id)) {
    in_cache <- isolate(cache_id %in% names(cache[["cache"]]))
    if (in_cache) {
      return(cache[["cache"]][[cache_id]])
    }
  }

  # Execute code block and cache result
  res <- code_block
  if (!is.null(cache_id)) cache[["cache"]][[cache_id]] <- res

  res
}


#' Read and name trees using cache
#'
#' @param cache cache object
#' @param cache_id Id of key in cache
#' @param cache_code Code to execute
#' @param names_to_do Names to label the trees
#'
#' @return Trees with names
read_and_name_trees_cache <- function(cache,
                                      cache_id,
                                      cache_code,
                                      names_to_do) {
  res <- cache_wrapper(cache, cache_id, cache_code)
  if (is.character(res)) { # Error
    cache[["cache"]][[cache_id]] <- NULL
    return(res)
  }
  res$names <- make_tree_names(res, names_to_do)
  res
}




#' Get tree to use based on radio box
#'
#' @param tree_to_listen Radio box selection
#' @param my_vals Reactive variables
#' @param cache Cache object
#' @param data_upload_root_drop Root from data upload page
#' @param paup_is_weighted Whether or not PAUP* was run with weight
#' @param analysis_metrics Metrics to include in analysis
#'
#' @return Tree corresponding to selection
get_tree_to_use <- function(tree_to_listen,
                            my_vals,
                            cache,
                            data_upload_root_drop,
                            paup_is_weighted,
                            analysis_metrics) {
  colstart <- 5
  if (!isolate(my_vals$is_on_analysis_page &&
                 (my_vals$paup_finished ||
                    my_vals$analysis_upload_finished))) return()
  to_listen <- tree_to_listen
  if (to_listen$set == "none") return()
  else if (to_listen$set %in% c("paup", "ref"))
    tree_to_do <- to_listen$tree_to_do
  else stop()
  assertthat::assert_that(!is.null(tree_to_do))

  # Get trees
  if (to_listen$set == "paup") {
    trees <- read_and_name_trees_cache(cache, "trees", {
      read_and_name_trees_paup(my_vals[["data"]],
                               paup_is_weighted,
                               do_unroot = TRUE,
                               insert_root = data_upload_root_drop)
    },
    analysis_metrics)
  } else if (to_listen$set == "ref") {
    leafset <- isolate({
      names(my_vals[["data"]])[colstart:ncol(my_vals[["data"]])]
    })

    trees <- read_and_name_trees_cache(cache,
                                       "trees_upload",
                                       NULL, # NULL means it should be in cache
                                       names_to_do = analysis_metrics)
  }

  # Get tree that was selected
  if (tree_to_do %in% c("Strict Consensus", "Majority Consensus")) {
    consensus_trees <- make_consensus_trees(trees)
    idx <- which(consensus_trees$names == tree_to_do)
    tree <- consensus_trees$trees[[idx]]
  } else {
    idx <- which(trees$names == tree_to_do)
    if (length(idx) == 0) return(NULL)
    else tree <- trees$trees[[idx]]
  }

  list(tree = tree, tree_to_do = tree_to_do, set = to_listen$set)
}
