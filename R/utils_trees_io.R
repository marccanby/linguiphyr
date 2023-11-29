

#' Make tree names
#'
#' @param trees List of trees
#' @param names_to_do Names of score types to include
#'
#' @return Character vector of names corresponding to trees
make_tree_names <- function(trees, names_to_do) {
  all_scores <- trees$all_scores
  names_pre <- trees$names_pre
  res <- c()
  for (i in seq_along(names_pre)) {
    asi <- all_scores[[i]]
    if (length(names_to_do) > 0) {
      ministr <- " ("
      if ("Parsimony (\u21E9)" %in% names_to_do) {
        ministr <- paste0(ministr, asi$mp, " | ")
      }
      if ("Weighted Parsimony (\u21E9)" %in% names_to_do) {
        ministr <- paste0(ministr, asi$wmp, " | ")
      }
      if ("Incompatibility (\u21E9)" %in% names_to_do) {
        ministr <- paste0(ministr, asi$mc, " | ")
      }
      if ("Weighted Incompatibility (\u21E9)" %in% names_to_do) {
        ministr <- paste0(ministr, asi$wmc, " | ")
      }
      if ("Total Edge Support (\u21E7)" %in% names_to_do) {
        ministr <- paste0(ministr, asi$tcs, " | ")
      }
      if ("Weighted Total Edge Support (\u21E7)" %in% names_to_do) {
        ministr <- paste0(ministr, asi$wtcs, " | ")
      }
      if ("Minimum Edge Support (\u21E7)" %in% names_to_do) {
        ministr <- paste0(ministr, asi$mcs, " | ")
      }
      if ("Weighted Minimum Edge Support (\u21E7)" %in% names_to_do) {
        ministr <- paste0(ministr, asi$wmcs, " | ")
      }
      ministr <- paste0(substr(ministr, 1, nchar(ministr) - 3), ")")
    } else {
      ministr <- ""
    }
    res <- c(res, paste0(names_pre[i], ministr))

  }
  res
}






#' Read trees from PAUP* file
#'
#' @param do_unroot Whether or not to unroot the trees
#' @param insert_root Whether or not to insert root into trees
#'
#' @return List of trees read from PAUP* file
read_trees_from_paup <- function(do_unroot,
                                 insert_root = NULL) {

  # Read from PAUP* outputs
  tmp <- tempdir()
  paup_out_trees <- file.path(tmp, "paup_out.trees")
  paup_out_scores <- file.path(tmp, "paup_out.scores")
  trees <- ape::read.nexus(paup_out_trees,
                           tree.names = NULL,
                           force.multi = TRUE)
  scores <- utils::read.csv(paup_out_scores, sep = "\t")$Length

  # Insert root into tree
  if (!is.null(insert_root)) {
    newtrees <- list()
    for (idx in seq_along(trees)) {
      assertthat::assert_that({
        ape::is.rooted(trees[[idx]])
      })
      newtrees[[idx]] <- TreeTools::AddTip(trees[[idx]],
                                           where = 0,
                                           label = insert_root)
    }
    trees <- newtrees
  }

  # Remove duplicates, TODO figure out if this is still necessary
  if (do_unroot) {
    trees <- ape::unroot.multiPhylo(trees)
    uq <- ape::unique.multiPhylo(trees)
    old_index <- attr(uq, "old.index")

    idxes <- match(seq_along(uq), old_index)
    scores <- scores[idxes]

    trees <- uq
  }

  trees

}






#' Sort and name trees
#'
#' @param trees List of phylogenetic trees
#' @param char_reps Character representations
#' @param sort_by Metric to sort by, either 'wmp' for weighted maximum
#'   parsimony or 'mp' for maximum parsimony
#' @param names Names of trees if pre-specified, else NULL
#'
#' @return List of trees with names and scores
sort_and_name_trees <- function(trees,
                                char_reps,
                                sort_by = "wmp",
                                names = NULL) {

  assertthat::assert_that({
    sort_by %in% c("wmp", "mp") # Could also do mc/wmc
  })
  names_orig <- names

  sorters <- c() # To sort the trees
  all_scores <- list() # Keep track of all the scores

  for (idx in seq_along(trees)) {
    tree <- trees[[idx]]
    assertthat::assert_that({
      !ape::is.rooted(tree)
    })

    # TODO: Consider looking at cache here
    pcache <- run_parsimony_on_each_char(tree, char_reps)
    ret <- find_incompat_chars(char_reps, pcache)

    if (!ape::is.binary(tree)) {
      enforcing_chars <- data.frame() # not computed for now
    } else {
      enforcing_chars <- find_enforcing_chars(ape::unroot(tree),
                                              char_reps,
                                              parsimony_cache = pcache)
    }

    # Invariant no matter root - so the edge ids are guaranteed
    #   to match up with the enforcing_chars
    edge <- make_edges(tree)$edge
    ret2 <- join_enf_chars_with_edge_table(enforcing_chars, edge)

    ps_score_unweighted <- sum(unlist(lapply(pcache, function(x) x$score_iq)))
    ps_score_weighted <- sum(unlist(lapply(pcache, function(x) x$score_iq_w)))
    incompat_score_unweighted <- nrow(ret)
    incompat_score_weighted <- sum(ret$weight)

    tcs_unweighted <- sum(as.numeric(ret2$n[ret2$n != " "]))
    tcs_weighted <- sum(as.numeric(ret2$sum[ret2$sum != " "]))
    mcs_unweighted <- min(as.numeric(ret2$n[ret2$n != " "]))
    mcs_weighted <- min(as.numeric(ret2$sum[ret2$sum != " "]))

    if (sort_by == "wmp") s <- ps_score_weighted
    else if (sort_by == "mp") s <- ps_score_unweighted
    else stop()
    sorters <- c(sorters, s)

    minilist <- list(mp = ps_score_unweighted,
                     wmp = ps_score_weighted,
                     mc = incompat_score_unweighted,
                     wmc = incompat_score_weighted,
                     tcs = tcs_unweighted,
                     wtcs = tcs_weighted,
                     mcs = mcs_unweighted,
                     wmcs = mcs_weighted,
                     parsimony_cache = pcache)

    all_scores[[idx]] <- minilist

  }

  new_trees <- list()
  new_all_scores <- list()
  ordering <- order(sorters)
  nms_final <- c()

  for (i in seq_along(ordering)) {
    idx <- ordering[i]
    tree <- trees[[idx]]
    new_trees[[i]] <- tree
    new_all_scores[[i]] <- all_scores[[idx]]

    if (is.null(names_orig)) nms_final <- c(nms_final, paste0("Tree ", i))
    else nms_final <- c(nms_final, paste0(names_orig[idx]))
  }
  res <- list(trees = new_trees,
              names_pre = nms_final,
              scores = sorters[ordering],
              all_scores = new_all_scores)

  res
}






#' Read and name trees from PAUP*
#'
#' @param char_reps Character representations
#' @param is_weighted Whether or not to use weighted parsimony for sorting
#' @param do_unroot Whether or not to unroot trees
#' @param insert_root Root leaf to insert
#'
#' @return List of trees along with their names to display
read_and_name_trees_paup <- function(char_reps,
                                     is_weighted,
                                     do_unroot,
                                     insert_root) {
  trees <- read_trees_from_paup(do_unroot, insert_root)
  if (is_weighted) sort_by <- "wmp"
  else sort_by <- "mp"
  res <- sort_and_name_trees(trees, char_reps, sort_by = sort_by)

  res
}








#' Read and name trees from file
#'
#' @param file File path
#' @param char_reps Character representations
#' @param leafset Leafs to include
#' @param is_weighted Whether or not to use weighted parsimony for sorting
#' @param do_unroot Whether or not to unroot trees
#'
#' @return List of trees along with their names to display
read_and_name_trees_file <- function(file,
                                     char_reps,
                                     leafset,
                                     is_weighted,
                                     do_unroot = TRUE) {
  trees <- ape::read.nexus(file, tree.names = NULL, force.multi = TRUE)

  nms <- names(trees)
  error <- c()
  for (i in seq_along(nms)) {
    tree <- trees[[i]]
    if (substr(nms[i], 1, 1) == "'" &&
          substr(nms[i], nchar(nms[i]), nchar(nms[i])) == "'") {
      nms[i] <- substr(nms[i], 2, nchar(nms[i]) - 1)
    }
    tips <- tree$tip.label
    if (!all(tips[order(tips)] == leafset[order(leafset)])) {
      error <- c(error,
                 paste0(nms[i],
                        " does not have the same leafset as  the data you",
                        " have uploaded."))
    }
  }
  if (!is.null(error)) return(paste0(error, collapse = "\n"))

  if (do_unroot) trees <- ape::unroot(trees)
  if (is_weighted) sort_by <- "wmp"
  else sort_by <- "mp"
  res <- sort_and_name_trees(trees, char_reps, names = nms, sort_by = sort_by)

  res
}
