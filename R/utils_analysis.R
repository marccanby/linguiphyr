#' Find parsimony-uninformative characters in data frame
#'
#' @param df Linguistic data frame
#'
#' @return List with row numbers of uninformative characters and number of
#'   big states per character
find_uninformative_characters <- function(df) {
  uninf <- c()
  numbigstates <- c()
  colstart <- 5
  df <- replace_qs_with_nums(df)
  for (char in seq_len(nrow(df))) {
    states <- as.character(df[char, colstart:ncol(df)])
    unique_states <- unique(states)
    states_with_atleast2 <- c()
    for (state in unique_states) {
      if (sum(states == state) >= 2) {
        states_with_atleast2 <- c(states_with_atleast2, state)
      }
    }
    if (length(states_with_atleast2) < 2) {
      uninf <- c(uninf, char)
    }
    numbigstates <- c(numbigstates, length(states_with_atleast2))
  }

  list(uninf = uninf, numbigstates = numbigstates)
}






#' Find characters in data frame that support clade (tree-agnostic)
#'
#' @param clade Character vector containing languages of clade
#' @param df Linguistic data frame
#'
#' @return Data frame containing only characters that support provided clade
find_chars_supporting_clade <- function(clade, df) {
  colstart <- 5
  idxes <- c()
  data_ <- replace_qs_with_nums(df)

  for (i in seq_len(nrow(data_))) {
    row <- data_[i, colstart:ncol(data_)]
    nms <- names(row) %in% clade
    row <- as.character(unlist(row))
    assertthat::assert_that({
      all(row != "?")
    })
    states <- row[nms]
    if (length(unique(states)) > 1) next
    unique_state <- unique(states)
    if (sum(row == unique_state) == length(clade)) { # Only occurs here
      idxes <- c(idxes, i)
    }
  }
  df[idxes, ]
}




#' Annotate tree with states on internal nodes
#'
#' @param tree Tree to annotate
#' @param char_rep Character representation with which tree is to be annotated
#' @param parsimony_rep Parsimony result from running maximum parsimony on
#'   this tree with this character
#'
#' @return List with annotations by edge id and mapping variable
annotate_tree <- function(tree, char_rep, parsimony_rep) {
  assertthat::assert_that({
    ape::is.rooted(tree)
  })

  res2 <- parsimony_rep
  res <- res2$res$ancestral_likelihoods
  states_in_order <- res2$states_in_order
  unique_states <- char_rep$unique_states

  final_res <- c()

  # Show question marks at leaves
  for (state in states_in_order) {
    if (state > length(unique_states)) final_res <- c(final_res, "?")
    else final_res <- c(final_res, unique_states[state])
  }

  for (i in seq_len(nrow(res))) {
    states <- which(res[i, ] > 0)
    has_multiple <- length(states) > 1
    if (!has_multiple) {
      state_here <- unique_states[states]
    } else {
      # Display as a set of states
      state_here <- "{"
      for (q in seq_along(states)) {
        state_here <- paste0(state_here, unique_states[states[q]])
        if (q != length(states)) state_here <- paste0(state_here, ",")
      }
      state_here <- paste0(state_here, "}")
    }
    final_res <- c(final_res, state_here)
  }

  # Since the tree was rooted, it had 2n-1 (rather than 2n-2) nodes - and so
  #   final_res has 2n-1 items.
  # A bit post hoc, but now match with bipartitions. Since root doesn't matter
  #   for this, we'll end up with 2n-2 items.

  resh <- make_edges(tree)
  edge <- resh$edge
  mapping <- resh$mapping
  edge$label <- NA
  for (i in seq_len(nrow(edge))) {
    edge$label[i] <- final_res[edge$node[i]]
  }

  list(edge = edge, mapping = mapping)
}





#' Join enforcing characters table with edge table
#'
#' @param enforcing_chars Enforcing characters table
#' @param edge Edge table of tree
#'
#' @return Table with enforcing character count and summed weight by
#'   edge using parent and child ids from edge table
join_enf_chars_with_edge_table <- function(enforcing_chars, edge) {
  .data <- rlang::.data
  cnts <- dplyr::count(enforcing_chars, .data$edge_id)
  sms <- as.data.frame(enforcing_chars %>%
                         dplyr::group_by(.data$edge_id) %>%
                         dplyr::summarize(sum = sum(.data$weight)))
  edge_nums <- unique(edge$edge_num)
  edge_nums <- as.numeric(edge_nums[edge_nums != " "])
  missing_ids <- edge_nums[!(edge_nums %in% cnts$edge_id)]

  if (length(missing_ids) > 0) {
    cnts <- rbind(cnts, data.frame(edge_id = missing_ids, n = 0))
    sms <- rbind(sms, data.frame(edge_id = missing_ids, sum = 0))
  }

  joined <- dplyr::left_join(edge, cnts, by = c("edge_num" = "edge_id"))
  nr <- nrow(joined)
  joined <- dplyr::left_join(joined, sms, by = c("edge_num" = "edge_id"))
  assertthat::assert_that({
    nrow(joined) == nr
  })
  non_leaf_edges <- edge[edge$edge_num != " ", ]
  is_non_leaf <- joined$edge_num %in% non_leaf_edges$edge_num
  joined$n[!is_non_leaf] <- " "
  joined$sum[!is_non_leaf] <- " "
  assertthat::assert_that({
    !all(is.na(joined))
  })

  joined <- joined[, -3]
  joined
}





#' Find incompatible characters on tree
#'
#' @param char_reps Character representations
#' @param parsimony_cache Parsimony information
#' @param return_all_chars Whether or not to return all characters or just the
#'   incompatible ones
#'
#' @return Data frame with incompatible characters
find_incompat_chars <- function(char_reps,
                                parsimony_cache,
                                return_all_chars = FALSE) {
  ids <- c()
  nms <- c()
  rcs <- c()
  chartypes <- c()
  scores <- c()
  act_scores <- c()
  act_scores_w <- c()
  xtrachange <- c()
  weights <- c()
  ps_score <- 0
  ps_score_w <- 0
  for (i in seq_along(char_reps)) {
    charep <- char_reps[[i]]
    score_iq <- parsimony_cache[[i]]$score_iq
    score_iq_w <- parsimony_cache[[i]]$score_iq_w
    score_i_standard <- parsimony_cache[[i]]$stand_score

    numq <- sum(charep$are_questions) +
      ("ancstate" %in% names(charep) && charep$anc_symbol == "?")
    ps_score <- ps_score + score_iq
    ps_score_w <- ps_score_w + score_iq_w
    rc_i <- length(charep$unique_states) + numq

    assertthat::assert_that({
      score_i_standard >= rc_i - 1
    })
    is_compat <- score_i_standard == rc_i - 1
    if (!is_compat || return_all_chars) {
      ids <- c(ids, charep$id)
      nms <- c(nms, charep$name)
      rcs <- c(rcs, rc_i)
      chartypes <- c(chartypes, charep$type_orig)
      scores <- c(scores, score_i_standard)
      xtrachange <- c(xtrachange, score_i_standard - rc_i + 1)
      act_scores <- c(act_scores, score_iq)
      act_scores_w <- c(act_scores_w, score_iq_w)
      weights <- c(weights, charep$weight)
    }
  }

  data.frame(id = ids,
             feature = nms,
             weight = weights,
             chartype = chartypes,
             rc = rcs,
             score = scores,
             extra_changes = xtrachange,
             ps_score = act_scores,
             ps_score_weighted = act_scores_w)
}







#' Find characters incompatible on any tree
#'
#' @param char_reps Character representations
#' @param parsimony_caches Parsimony cache on each tree
#' @param names Names of trees, if wish to include in character, else NULL
#'
#' @return Data frame containing characters incompatible on any tree
find_chars_incompat_any_tree <- function(char_reps,
                                         parsimony_caches,
                                         names = NULL) {
  do_names <- !is.null(names)
  if (!do_names) names <- seq_along(parsimony_caches)
  else assertthat::assert_that({
    length(names) == length(parsimony_caches)
  })

  ids <- c()
  nms <- c()
  weights <- c()
  cnts <- c()
  chartypes <- c()
  if (do_names) trees_incompat <- list()

  for (i in seq_along(parsimony_caches)) {
    pcache <- parsimony_caches[[i]]
    incompat_chars <- find_incompat_chars(char_reps, pcache)

    if (nrow(incompat_chars) > 0) {
      for (j in seq_len(nrow(incompat_chars))) {
        id <- incompat_chars[j, 1]
        nm <- incompat_chars[j, 2]
        wt <- incompat_chars[j, 3]
        chartype <- incompat_chars[j, 4]
        if (id %in% ids) {
          idx <- which(ids == id)
          cnts[idx] <- cnts[idx] + 1
          if (do_names) trees_incompat[[idx]] <- c(trees_incompat[[idx]],
                                                   names[i])
        } else {
          ids <- c(ids, id)
          nms <- c(nms, nm)
          cnts <- c(cnts, 1)
          weights <- c(weights, wt)
          chartypes <- c(chartypes, chartype)
          if (do_names) trees_incompat[[length(trees_incompat) + 1]] <- names[i]
        }
      }
    }
  }
  res <- data.frame(id = ids,
                    feature = nms,
                    weight = weights,
                    chartype = chartypes,
                    count = cnts)

  if (do_names) {
    res$names <- unlist(lapply(trees_incompat,
                               function(x) paste0(x, collapse = ", ")))
  }

  res
}






#' Find characters that enforce each edge on phylogenetic tree
#'
#' @description A character is said to enforce (or support) an edge in a
#'   phylogenetic tree if the edge's collapse would increase the parsimony
#'   score of the character on the tree.
#'
#' @param tree Binary phylogenetic tree
#' @param char_reps Character representations
#' @param parsimony_cache Parsimony cache containing parsimony information on
#'   characters
#'
#' @return List with information about enforcing characters for each edge
find_enforcing_chars <- function(tree,
                                 char_reps,
                                 parsimony_cache) {
  # Assertions
  assertthat::assert_that({
    !ape::is.rooted(tree)
  })
  assertthat::assert_that({
    ape::is.binary(tree)
  })

  edge <- make_edges(tree)$edge
  non_leaf_edges <- edge[edge$edge_num != " ", ]

  # Make variables to fill
  edge_ids <- c()
  char_ids <- c()
  char_names <- c()
  scores_iq <- c()
  scores_iq_w <- c()

  for (idx in seq_len(nrow(non_leaf_edges))) {
    # Collapse internal edge
    collapsed_tree <- collapse_edge(tree,
                                    non_leaf_edges[idx, 1],
                                    non_leaf_edges[idx, 2])
    enforcing_ids <- c()
    enforcing_names <- c()
    encorcing_scores_iq <- c()
    encorcing_scores_iq_w <- c()

    for (i in seq_along(char_reps)) {
      charep <- char_reps[[i]]

      score_i <- parsimony_cache[[i]]$score
      score_iq <- parsimony_cache[[i]]$score_iq
      score_iq_w <- parsimony_cache[[i]]$score_iq_w
      score_i2 <-  run_parsimony(collapsed_tree, char_reps[[i]])$score

      if (score_i2 > score_i) { # Character enforces this edge
        enforcing_ids <- c(enforcing_ids, charep$id)
        enforcing_names <- c(enforcing_names, charep$name)
        encorcing_scores_iq <- c(encorcing_scores_iq, score_iq)
        encorcing_scores_iq_w <- c(encorcing_scores_iq_w, score_iq_w)
      }
    }

    if (length(enforcing_ids) > 0) { # At least one enforcing character
      edge_ids <- c(edge_ids,
                    rep(non_leaf_edges[idx, 3], length(enforcing_ids)))
      char_ids <- c(char_ids, enforcing_ids)
      char_names <- c(char_names, enforcing_names)
      scores_iq <- c(scores_iq, encorcing_scores_iq)
      scores_iq_w <- c(scores_iq_w, encorcing_scores_iq_w)
    } else {
      # Here, an edge has no enforcing characters. This can happen if a tree
      # is forced to be binary (when in reality there should be a polytomy)
      # or when checking for enforcing characters on a tree that was not
      # optimized for that character set.
    }
  }
  ids <- unlist(lapply(char_reps, function(x) x$id))
  weights <- unlist(lapply(char_reps, function(x) x$weight))
  weights <- weights[match(char_ids, ids)]
  types <- unlist(lapply(char_reps, function(x) x$type_orig))
  types <- types[match(char_ids, ids)]

  data.frame(edge_id = edge_ids,
             char_id = char_ids,
             feature = char_names,
             weight = weights,
             type = types,
             ps_score = scores_iq,
             ps_score_weighted = scores_iq_w)
}
