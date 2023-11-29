

#' Make relative chronology for this character
#'
#' @param tree Tree on which to compute relative chronology
#' @param clade Clade for which to compyute relative chronology
#' @param char_rep Character represntation
#' @param parsimony_rep Parsimony representation for this character
#' @param root Root of the tree
#'
#' @return List with relative chronology information, or NULL if clade is not
#'   a clade of the tree
make_chronology_for_char <- function(tree,
                                     clade,
                                     char_rep,
                                     parsimony_rep,
                                     root) {
  resh <- annotate_tree(ape::root(tree, root, resolve.root = TRUE),
                        char_rep,
                        parsimony_rep)
  annotations <- resh$edge
  mapping <- resh$mapping

  # Find the root of the rest of the tree (under the real root) - this is the
  #   clade that's the top of the search
  node_states <- annotations
  root_id <- which(tree$tip.label == root)
  root_state <- node_states$label[node_states$node == root_id]

  # Now save the state below the state above the root.
  root_parent <- node_states$parent[node_states$node == root_id]
  assertthat::assert_that({
    !(root_parent %in% node_states$node)
  })
  below_root_parents <- node_states[node_states$parent == root_parent, ]
  assertthat::assert_that({
    nrow(below_root_parents) == 2
  })
  state_below_root_parent <- below_root_parents$label[below_root_parents$node !=
                                                        root_id]
  # The root of the rest of the tree
  node_below_root_parent <- below_root_parents$node[below_root_parents$node !=
                                                      root_id]

  # Find which clade to go to (bottom of the search)
  if (length(clade) > 1) {
    clade_ids <- which(tree$tip.label %in% clade)
    clade_ids <- clade_ids[order(clade_ids)]
    clade_id <- NULL
    for (n in names(mapping)) {
      idsh <- mapping[[n]]
      if (length(clade_ids) == length(idsh) && all(clade_ids == idsh)) {
        clade_id <- as.integer(n)
        break
      }
    }
    if (is.null(clade_id)) return(NULL) # It is not a clade!
  } else {
    clade_id <- which(tree$tip.label == clade)
  }

  # Now actually get the path
  res <- c()
  edge_nums <- c()
  start <- clade_id
  while (start != node_below_root_parent) {
    row <- annotations[annotations$node == start, ]
    assertthat::assert_that({
      nrow(row) == 1
    })
    res <- c(res, row$label)
    if (length(clade) == 1 &&
          start == clade_id) edge_nums <- c(edge_nums, paste0(clade, " Edge"))
    else edge_nums <- c(edge_nums, row$edge_num)
    start <- row$parent
  }

  res <- c(res, state_below_root_parent, root_state)
  edge_nums <- rev(c(edge_nums, paste0(root, " Edge")))
  res <- rev(res)
  list(res = res, edge_nums = edge_nums)

}



#' Make relative chronology for tree
#'
#' @param tree Tree on which to compute relative chronology
#' @param clade Clade for which to compute relative chronology
#' @param char_reps Characters to include in the relative chronology
#' @param parsimony_cache Parsimony results on each character on this tree
#' @param root Root of tree
#'
#' @return Relative chronology data frame
make_chronology <- function(tree, clade, char_reps, parsimony_cache, root) {
  char_names <- unlist(lapply(char_reps, function(x) x$id))
  char_features <- unlist(lapply(char_reps, function(x) x$name))
  res <- list()
  for (n in char_names) {
    idxh <- which(unlist(lapply(char_reps, function(x) x$id == n)))
    char_rep <- char_reps[[idxh]]
    parsimony_rep <- parsimony_cache[[idxh]]
    here <- make_chronology_for_char(tree, clade, char_rep, parsimony_rep, root)
    if (is.null(here)) return("The selected leaves do not form a clade.")
    res[[n]] <- here$res
    leng <- length(here$res)
    edge_nums <- here$edge_nums
  }

  # Filter out ones that are irrelevant for the study - ones where there's no
  #   change, or where there are multiple options
  for (n in char_names) {
    rep <- res[[n]]
    if (length(unique(rep)) == 1 ||
          any(grepl(pattern = "{", rep, fixed = TRUE))) res[[n]] <- NULL
  }

  final <- data.frame()
  for (rank in 1:(leng - 1)) {
    edge_id <- edge_nums[rank]
    for (n in names(res)) {
      rep <- res[[n]]
      char_state_bf <- rep[rank]
      char_state_af <- rep[rank + 1]
      if (char_state_bf != char_state_af) { # Found a change!
        dframe <- data.frame(rank = rank,
                             edge_id = edge_id,
                             char_id = n,
                             char_name = char_features[which(char_names == n)],
                             state_from = char_state_bf,
                             state_to = char_state_af)
        final <- rbind(final, dframe)
      }
    }
  }
  final <- final[!(final$rank == 1 & final$state_from == "?"), ]
  if (min(final$rank) == 2) final$rank <- final$rank - 1
  final <- final[!(final$rank == max(final$rank) & final$state_to == "?"), ]
  final
}
