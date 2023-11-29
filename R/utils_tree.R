

#' Collapse edge on tree
#'
#' @param tree Tree on which to collapse edge
#' @param node1 One endpoint of edge to collapse
#' @param node2 Other endpoint of edge to collapse
#'
#' @return Tree with collapsed edge
collapse_edge <- function(tree, node1, node2) {
  tree2 <- tree
  tree2$edge <- tree$edge[!(tree$edge[, 1] == node1 &
                              tree$edge[, 2] == node2), ]
  tree2$edge[tree2$edge[, 1] == node2, 1] <- node1
  tree2$Nnode <- tree2$Nnode - 1
  unodes <- unique(c(tree2$edge[, 1], tree2$edge[, 2]))
  unodes <- unodes[order(unodes)]
  for (idx in seq_len(nrow(tree2$edge))) {
    tree2$edge[idx, 1] <- which(unodes == tree2$edge[idx, 1])
    tree2$edge[idx, 2] <- which(unodes == tree2$edge[idx, 2])
  }
  tree2
}





#' Return strict and greedy consensus trees
#'
#' @param trees Trees for which to compute consensus trees
#'
#' @return List with consensus trees and names of those trees
make_consensus_trees <- function(trees) {
  strict <- ape::consensus(trees$trees,
                           p = 1,
                           check.labels = TRUE,
                           rooted = FALSE)
  majority <- ape::consensus(trees$trees,
                             p = 0.5,
                             check.labels = TRUE,
                             rooted = FALSE)
  strict$node.label <- NULL
  majority$node.label <- NULL
  nms <- c("Strict Consensus", "Majority Consensus")
  list(trees = list(strict, majority), names = nms)
}






#' Number edges in tree (invariant to root placement)
#'
#' @param tree Tree For which to produce edge numbers
#' @description Number edges in such a way that result is invariant to root
#'   placement. Edge ids are assigned by bipartition, not internal node
#'   labelings. Note that the internal nodes of tree$edge change depending on
#'   the root placement, but the edge id's produced are invariant to this.
#'   Thus, this function is useful for many downstream applications, as one
#'   can use edge id to reference parts of the tree regardless of root
#'   location.
#'
#' @return List of edges, which is invariant to the root placement
make_edges <- function(tree) {
  splits <- ape::prop.part(tree)
  labels_of_splits <- attr(splits, "labels")
  labels_of_tips <- tree$tip.label
  num_leaves <- length(tree$tip.label)
  result <- list()
  label_that_must_be_there <- which(labels_of_splits == labels_of_tips[1])
  sorting_criterion <- c()

  newsplits <- list()
  cnt <- 1

  # Get rid of trivial splits that show up for some reason
  for (i in seq_along(splits)) {
    if (length(splits[[i]]) %in% c(num_leaves, num_leaves - 1, 1)) next
    else newsplits[[cnt]] <- splits[[i]]
    cnt <- cnt + 1
  }
  splits <- newsplits
  for (i in seq_along(splits)) {
    splt <- splits[[i]]
    if (!(label_that_must_be_there %in% splt)) {
      splt <-  (1:num_leaves)[!((1:num_leaves) %in% splt)]
    }
    assertthat::assert_that({
      label_that_must_be_there %in% splt
    })
    ordered <- c()
    for (x in labels_of_tips) {
      id_in_splits <- which(labels_of_splits == x)
      if (id_in_splits %in% splt) ordered <- c(ordered, id_in_splits)
    }
    assertthat::assert_that({
      all(ordered[order(ordered)] == splt[order(splt)])
    })
    result[[i]] <- ordered
    sorting_criterion <- c(sorting_criterion, paste0(ordered, collapse = ";"))
  }
  ord <- order(sorting_criterion)
  splits <- list()
  for (i in seq_along(ord)) {
    j <- ord[i]
    splits[[i]] <- result[[j]]
  }
  splts <- splits

  # This will vary depending on the rooting of tree; however, we will add in
  #   the numbers so that it is consistent with the splits which are root
  #   invariant
  edge <- data.frame(tree$edge, edge_num = NA)
  names(edge) <- c("parent", "node", "edge_num")
  assertthat::assert_that({
    num_leaves - 3 == length(splts)
  }) # Splits additionally has the entire leafset as a bipartition
  iteration <- 1
  unreduced <- 1:num_leaves
  mapping <- list()
  fullmapping <- list()
  while (sum(!is.na(edge$edge_num)) != num_leaves - 3) {
    for (parent in unique(edge$parent)) {
      sub <- edge[edge$parent == parent, ]
      assertthat::assert_that({
        nrow(sub) %in% c(2, 3)
      })
      if (sum(sub$node %in% unreduced) == 2) {
        children <- sub$node[sub$node %in% unreduced]
        if (iteration > 1) {
          newchildren <- c()
          for (x in children) {
            if (x <= num_leaves) {
              newchildren <- c(newchildren, x)
            } else {
              newchildren <- c(newchildren, mapping[[as.character(x)]])
              fullmapping[[as.character(x)]] <- mapping[[as.character(x)]]
              mapping[[as.character(x)]] <- NULL
              unreduced <- unreduced[unreduced != x]
            }

          }
          children <- newchildren
        }

        children <- children[order(children)]
        antichildren <- (1:num_leaves)[!((1:num_leaves) %in% children)]
        antichildren <- antichildren[order(antichildren)]
        found <- FALSE
        for (jj in seq_along(splts)) {
          if ((length(splts[[jj]]) == length(children) &&
                 all(children %in% splts[[jj]])) ||
                (length(splts[[jj]]) == length(antichildren) &&
                   all(antichildren %in% splts[[jj]]))) {
            found <- TRUE
            break
          }
        }
        assertthat::assert_that({
          found
        })
        if (nrow(sub) == 3) clade_num <- sub$node[!(sub$node %in% unreduced) &
                                                    is.na(sub$edge_num)]
        else clade_num <- parent
        edge$edge_num[edge$node == clade_num] <- jj
        mapping[[as.character(clade_num)]] <- children
        unreduced <- unreduced[!(unreduced %in% children)]
        unreduced <- c(unreduced, clade_num)
      }
    }
    iteration <- iteration + 1
  }
  for (x in names(mapping)) {
    fullmapping[[x]] <- mapping[[x]]
  }
  edge$edge_num[is.na(edge$edge_num)] <- " "
  colnames(edge) <- c("parent", "node", "edge_num")
  list(edge = edge, mapping = fullmapping)
}
