


#' Compute parsimony for character on given tree
#'
#' @param tree Tree on which to calculate parsimony
#' @param char_rep Character representation of character for which to compute
#'   parsimony
#'
#' @return List with parsimony information.
run_parsimony <- function(tree,
                          char_rep) {
  # Root the tree - note that this WILL affect the order of the ancestral
  #   likelihoods table - so it will need to be reshuffled later if the root
  #   is different.....
  if ("anc_taxon" %in% names(char_rep)) outgroup <- char_rep$anc_taxon
  else outgroup <- tree$tip.label[order(tree$tip.label)][1]
  tree <- ape::root(tree, outgroup = outgroup, resolve.root = TRUE)

  states_in_order <- c()
  is_special <- FALSE
  for (name in tree$tip.label) {
    if (name %in% char_rep$taxa) {
      states_in_order <- c(states_in_order,
                           char_rep$integer_rep_noqs[char_rep$taxa == name])
    } else {
      assertthat::assert_that({
        "anc_taxon" %in% names(char_rep) && name == char_rep$anc_taxon
      })
      if (char_rep$type == "custom" && char_rep$anc_symbol != "?") {
        # Special case - this is a clever way to handle directionality from
        #   root in case of custom matrix where root state is known. Very
        #   nuanced.
        # Unfortunately asr_parsimony doesn't take into account the root state,
        #   so sometimes it violates the directionality preferences of the
        #   matrix.
        # E.g. if matrix is [[0,5],[1,0]] and root is state 0 then we know we
        #   definitely need a cost of 5 to get to any 1. But the algorithm
        #   will sometimes allow a bunch of 1->0 transitions (each worth 1), so
        #   the score could be better than 5. This should not be allowed.
        # Idea is to replace the root state with a special state that nothing
        #   can transition to. That way it's forced to exit this state. This
        #   state has same exit conditions as original state.
        assertthat::assert_that({
          nrow(char_rep$transition_matrix) == max(char_rep$integer_rep_noqs)
        })
        states_in_order <- c(states_in_order,
                             max(char_rep$integer_rep_noqs) + 1)
        is_special <- TRUE
        orig <- char_rep$ancstate_noq
      } else {
        states_in_order <- c(states_in_order, char_rep$ancstate_noq)
      }
    }
  }

  transition_costs <- char_rep$transition_matrix

  if (is_special) {
    n <- nrow(transition_costs)
    new_costs <- matrix(0, nrow = n + 1, ncol = n + 1)
    new_costs[1:n, 1:n] <- transition_costs
    new_costs[, ncol(new_costs)][-ncol(new_costs)] <- Inf
    new_costs[ncol(new_costs), 1:n] <- transition_costs[orig, ]
    transition_costs <- new_costs
  }

  res <- castor::asr_max_parsimony(tree,
                                   states_in_order,
                                   transition_costs = transition_costs)

  if (is_special) {
    res$scenario_counts[, orig] <- res$scenario_counts[, orig] +
      res$scenario_counts[, ncol(new_costs)]
    res$ancestral_likelihoods[, orig] <- res$ancestral_likelihoods[, orig] +
      res$ancestral_likelihoods[, ncol(new_costs)]

    res$scenario_counts <- res$scenario_counts[, -ncol(new_costs)]
    res$ancestral_likelihoods <- res$ancestral_likelihoods[, -ncol(new_costs)]
    states_in_order[tree$tip.label ==
                      char_rep$anc_taxon] <- char_rep$ancstate_noq
  }

  # Run standard pasrismony too (for incompat chars, which doesn't care about
  #   the matrix)
  res_stand <- castor::asr_max_parsimony(tree, states_in_order)

  # Compute the different numbers here, so don't have to do downstream
  #   everywhere
  numq <- sum(char_rep$are_questions) + ("ancstate" %in% names(char_rep) &&
                                           char_rep$anc_symbol == "?")
  score_iq <- res$total_cost - numq
  score_iq_w <- (score_iq * char_rep$weight)

  list(res = res,
       states_in_order = states_in_order,
       score = res$total_cost, # Includes ? changes
       stand_score = res_stand$total_cost, # Includes ? changes, with no matrix
       score_iq = score_iq, # No ? changes count
       score_iq_w = score_iq_w) # No ? changes
}







#' Compute parsimony for each character on given tree
#'
#' @param tree Tree on which to calculate parsimony
#' @param char_reps Character representations for which to compute parsimony
#'
#' @return List of parsimony results on each character on the provided tree
run_parsimony_on_each_char <- function(tree, char_reps) {
  ret <- list()
  for (idx in seq_along(char_reps)) {
    res <- run_parsimony(tree, char_reps[[idx]])
    ret <- append(ret, list(res))
  }
  ret
}








#' Write NEXUS file for PAUP* to run maximum parsimony
#'
#' @param char_reps Character representations
#' @param taxa Taxa
#' @param do_weight Whether or not to run weighted parsimony
#' @param is_exhaustive Whether or not to run exhaustive search
#' @param keep How many trees to keep
#' @param symbols Symbols to use when writing file
#' @param root Root of tree
#'
#' @return String containing text to write as NEXUS file
write_nexus <- function(char_reps,
                        taxa,
                        do_weight,
                        is_exhaustive,
                        keep,
                        symbols = paste0("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefg",
                                         "hijklmnopqrstuvwxyz"),
                        root = NULL) {

  toprint <- c()

  # Taxa block
  toprint <- c(toprint,
               "#NEXUS\n\nbegin taxa;",
               paste0("\tdimensions ntax=", length(taxa), ";"),
               "\ttaxlabels")
  for (tax in taxa) {
    toprint <- c(toprint, paste0("\t\t", tax))
  }
  toprint <- c(toprint, "\t;\nend;")

  # Char block
  line <- paste0("\nbegin characters;\n\tdimensions nchar=",
                 length(char_reps),
                 ";\n\tformat missing=? RespectCase symbols=\"",
                 symbols,
                 "\" transpose;\n\tmatrix")
  toprint <- c(toprint, line)

  types <- unlist(lapply(char_reps, function(x) x$type))
  if ("irreversible" %in% types) {
    irrev_chars <- c()
    irrev_conts <- c()
    assertthat::assert_that({
      !is.null(root)
    })
  }
  if ("custom" %in% types) {
    custom_chars <- c()
    assertthat::assert_that({
      !is.null(root)
    })
  }
  if (!is.null(root)) ancstates <- rep(NA, length(char_reps))

  for (idx in seq_along(char_reps)) {
    charep <- char_reps[[idx]]

    if (charep$type == "irreversible") {
      assertthat::assert_that({
        charep$ancstate == "0"
      })
      irrev_chars <- c(irrev_chars, idx)
      irrev_conts <- c(irrev_conts, length(charep$unique_states))
    } else if ("custom" == charep$type) {
      custom_chars <- c(custom_chars, idx)
    }

    # Now add in string
    if (!is.null(root)) ancstates[idx] <- charep$anc_symbol
    ministr <- charep$symbol_rep

    toprint <- c(toprint, paste0("\t\t", charep$id, "\t", ministr))
  }

  toprint <- c(toprint, "\t;\nend;")

  toprint <- c(toprint, "\nbegin assumptions;\n\toptions deftype=unord;")
  if ("irreversible" %in% types) {
    non2s <- unique(irrev_conts[irrev_conts != 2])
    for (x in non2s) {
      toprint <- c(toprint, paste0("\tusertype irrev",
                                   x,
                                   " stepmatrix = ",
                                   x,
                                   " ",
                                   substr(symbols, 1, x)))
      mat <- make_irreverseible_matrix(x)
      for (r in seq_len(nrow(mat))) toprint <- c(toprint,
                                                 paste0("\t\t",
                                                        paste0(mat[r, ],
                                                               collapse = " ")))
      toprint <- c(toprint, "\t\t;")
    }
  }

  if ("custom" %in% types) {
    for (x in custom_chars) {
      charep <- char_reps[[x]]
      mat <- charep$custom_matrix
      toprint <- c(toprint, paste0("\tusertype custom",
                                   x,
                                   " stepmatrix = ",
                                   nrow(mat),
                                   " ",
                                   substr(symbols, 1, nrow(mat))))
      for (r in seq_len(nrow(mat))) toprint <- c(toprint,
                                                 paste0("\t\t",
                                                        paste0(mat[r, ],
                                                               collapse = " ")))
      toprint <- c(toprint, "\t\t;")
    }
  }

  if (!is.null(root)) {
    uancstates <- unique(ancstates)
    uancstates <- uancstates[uancstates != "?"]
    ln <- paste0("ancstates *", root, " = ")
    for (x in uancstates) {
      ln <- paste0(ln, x, ":", paste0(which(ancstates == x), collapse = " "))
      if (uancstates[length(uancstates)] != x) ln <- paste0(ln, ", ")
    }
    toprint <- c(toprint, paste0("\t", ln, ";"))
  }
  toprint <- c(toprint, "end;")


  # Paup block
  toprint <- c(toprint, "\nbegin paup;")

  if (is_exhaustive) fst <- "\tset criterion=parsimony;"
  else fst <- "\tset criterion=parsimony maxtrees=100 increase=no;"

  weights <-  unlist(lapply(char_reps, function(x) x$weight))
  if (do_weight) {
    fst <- paste0(fst, "\n\tweights")
    unique_weights <- unique(weights)
    for (weight in unique_weights) {
      fst <- paste0(fst,
                    " ",
                    weight,
                    ":",
                    paste0(which(weights == weight), collapse = " "))
      if (weight != unique_weights[length(unique_weights)]) {
        fst <- paste0(fst, ",")
      }
    }
    fst <- paste0(fst, ";")
  }

  if ("irreversible" %in% types) {
    assertthat::assert_that({
      sum(irrev_conts == 2) > 0
    }) # Handle case when not
    fst <- paste0(fst,
                  "\n\tctype irrev.up:",
                  paste0(irrev_chars[irrev_conts == 2],
                         collapse = " "))
    non2s <- unique(irrev_conts[irrev_conts != 2])
    for (x in non2s) {
      fst <- paste0(fst,
                    ", irrev",
                    x,
                    ":",
                    paste0(irrev_chars[irrev_conts == x], collapse = " "))
    }
    if (! ("custom" %in% types)) fst <- paste0(fst, ";")
  }

  if ("custom" %in% types) {
    if (!("irreversible" %in% types)) {
      # Make ctype line
      fst <- paste0(fst, "\n\tctype ")
    }
    for (x in custom_chars) {
      if (substr(fst, nchar(fst), nchar(fst)) != " ") fst <- paste0(fst, ",")
      fst <- paste0(fst, " custom", x, ":", x)
    }
    fst <- paste0(fst, ";")
  }

  if (!is.null(keep)) kp <- paste0(" keep=", keep)
  else kp <- ""
  toprint <- c(toprint, fst)
  if (!is_exhaustive) {
    toprint <- c(toprint,
                 paste0("\thsearch start=stepwise addseq=random nreps=25",
                        " swap=tbr collapse=no", kp, ";"))
  } else {
    toprint <- c(toprint, paste0("\talltrees", kp, ";"))
  }

  if (is.null(keep)) toprint <- c(toprint, "\tfilter best=yes;")
  toprint <- c(toprint, "\tdescribetrees 1/diag=yes;")
  toprint <- c(toprint, paste0("\tpscores all/ ci ri rc hi scorefile",
                               "=paup_out.scores replace=yes;"))
  toprint <- c(toprint, paste0("\tsavetrees file=paup_out.trees replace",
                               "=yes format=nexus;"))
  toprint <- c(toprint, "\tquit;\nend;")

  paste0(toprint, sep = "\n")


}









#' Run PAUP*
#'
#' @param nexus_string Nexus string to run in PAUP*
#'
#' @return NULL
run_paup <- function(nexus_string) {

  # Create files
  tmp <- tempdir()
  lnexus <- file.path(tmp, "ling_nexus.nex")
  paup_out <- file.path(tmp, "paup_out.txt")
  paup_run <- file.path(tmp, "paup_run.txt")
  paup_out_trees <- file.path(tmp, "paup_out.trees")
  paup_out_scores <- file.path(tmp, "paup_out.scores")

  # Remove files from previous runs
  if (file.exists(lnexus)) file.remove(lnexus)
  if (file.exists(paup_out)) file.remove(paup_out)
  if (file.exists(paup_run)) file.remove(paup_run)
  if (file.exists(paup_out_trees)) file.remove(paup_out_trees)
  if (file.exists(paup_out_scores)) file.remove(paup_out_scores)

  # Write nexus string
  sink(lnexus)
  cat(nexus_string)
  sink()

  # Run PAUP* without holding
  system(paste0("paup -n ",
                lnexus,
                " > ",
                paup_out,
                " 2> ",
                paup_run),
         wait = FALSE)
}







#' Get output strings from PAUP*
#'
#' @return List with two elements: PAUP* input file and PAUP* output file
get_paup_output_strings <- function() {
  tmp <- tempdir()
  lnexus <- file.path(tmp, "ling_nexus.nex")
  paup_out <- file.path(tmp, "paup_out.txt")
  paup_run <- file.path(tmp, "paup_run.txt")

  paup_input <- readChar(lnexus, file.info(lnexus)$size)
  paup_input <- gsub("\n", "<br>", paup_input)
  paup_input <- gsub("\t", "&emsp;", paup_input)

  paup_output <- readChar(paup_out, file.info(paup_out)$size)
  paup_output <- gsub("\n", "<br>", paup_output)
  paup_output <- gsub("\t", "&emsp;", paup_output)

  paup_runput <- readChar(paup_run, file.info(paup_run)$size)
  paup_runput <- gsub("\n", "<br>", paup_runput)
  paup_runput <- gsub("\t", "&emsp;", paup_runput)

  list(paup_input = paup_input,
       paup_output = paup_output,
       paup_run = paup_runput)
}
