#' Make character representation
#'
#' @param row Row of linguistic data frame representing a character
#' @param root Leaf representing root if available, otherwise NULL
#' @param do_ancstate Whether or not to use ancestral state in representation
#' @param neutralize_custom Whether or not to set all custom costs to 1
#' @param overwrite_custom_weight Set character weight to max cost in custom
#'   weight matrix. If TRUE, neutralize_custom must also be TRUE
#' @param symbols Alphabetic symbols in the order with which to replace
#'   state numbers
#'
#' @return List, containing several attributes that represent various aspects
#'   of the character
make_character_representation <- function(row,
                                          root = NULL,
                                          do_ancstate = FALSE,
                                          neutralize_custom = FALSE,
                                          overwrite_custom_weight = FALSE,
                                          symbols = paste0("ABCDEFGHIJKLMNOP",
                                                           "QRSTUVWXYZabcdef",
                                                           "ghijklmnopqrstuv",
                                                           "wxyz")) {
  if (overwrite_custom_weight) assertthat::assert_that(neutralize_custom)

  colstart <- 5 # Start of data in data frame

  type <- row[["chartype"]]
  id <- row[1, "id"]
  weight <- row[1, "weight"]
  name <- row[1, "feature"]
  row <- row[1, colstart:ncol(row)]

  # Determine type
  if (type == "irreversible" && is.null(root)) {
    stop("Must specify root for irreversible characters.")
  }
  if (type == "irreversible" && row[[root]] != "0") {
    return(paste0("An irreversible character must have 0",
                  " as the root (ancestral) state."))
  }
  if (grepl("^custom:.+$", type)) typ <- "custom"
  else typ <- type

  # Get unique states
  has_question <- "?" %in% row
  r_ <- as.vector(unlist(row))
  unique_states <- unique(r_[r_ != "?"])
  unique_states <- unique_states[order(unique_states)]

  if (length(unique_states) > nchar(symbols)) {
    return(paste0("This character has ", length(unique_states),
                  " unique states, and the software currently cannot",
                  " handle more than ", nchar(symbols), "."))
  }
  if (any(grepl("/", r_, fixed = TRUE))) {
    stop("Cannot pass polymorphic characters to this method.")
  }

  # Initialize return variable
  res <- list(id = id,
              name = name,
              weight = weight,
              unique_states = unique_states,
              has_question = has_question,
              type = typ,
              type_orig = type)

  # Ancestral state
  if (do_ancstate) {
    assertthat::assert_that({
      !is.null(root)
    })
    ancstate <- row[[root]]
    res$anc_taxon <- root
    row <- row[, -which(names(row) == root)]
    res$ancstate <- ancstate
    if (ancstate == "?") {
      res$anc_symbol <- "?"
    } else {
      idx <- which(unique_states == ancstate)
      res$anc_symbol <- substr(symbols, idx, idx)
    }
    if (type == "irreversible") {
      assertthat::assert_that({
        res$anc_symbol == "A"
      }) # Otherwise it won't line up with the irrev matrices
    }
  }
  res$taxa <- names(row)
  res$are_questions <- row == "?"

  # Get symbol and integer representation
  ministr <- ""
  integer_rep_noqs <- c()
  q_start <- length(unique_states) + 1
  for (state in row) {
    if (state == "?") {
      ministr <- paste0(ministr, "?")
      integer_rep_noqs <- c(integer_rep_noqs, q_start)
      q_start <- q_start + 1
    } else {
      idx <- which(unique_states == state)
      ministr <- paste0(ministr, substr(symbols, idx, idx))
      integer_rep_noqs <- c(integer_rep_noqs, idx)
    }
  }
  res$symbol_rep <- ministr
  res$integer_rep_noqs <- integer_rep_noqs

  # Ancestral state
  if (do_ancstate) {
    if (res$anc_symbol == "?") {
      res$ancstate_noq <- q_start
      q_start <- q_start + 1
    } else {
      res$ancstate_noq <- which(unique_states == res$ancstate) # Has to be 0
    }
  }

  # Now make custom information
  mat <- NULL
  if (grepl("^custom:.+$", type)) {
    mat <- make_custom_matrix(substr(type, 8, nchar(type)), unique_states)
    if (neutralize_custom) {
      mat[mat == "i"] <- -1
      mat <- matrix(as.numeric(mat), ncol = ncol(mat))
      max_cost <- max(mat)
      mat[mat > 0] <- 1
      mat <- matrix(as.character(mat), ncol = ncol(mat))
      mat[mat == "-1"] <- "i"
      if (overwrite_custom_weight) res$weight <- max_cost
    }
    if (is.null(mat)) {
      return(paste0("The custom character specification could not be parsed.",
                    " Make sure it conforms to the specifications ",
                    "on the \'Data Input\' tab."))
    }
    res$custom_matrix <- mat
  }

  # Make cost matrix for all types
  trans <- make_cost_matrix_for_parsimony(unique_states, q_start - 1, typ, mat)
  res$transition_matrix <- trans

  return(res)
}





#' Make character representations
#'
#' @param root Root of tree if available, otherwise NULL
#' @param data Data frame
#' @param neutralize_custom Whether or not to neutralize custom weights
#' @param overwrite_custom_weight Set character weight to max cost in custom
#'   weight matrix. If TRUE, neutralize_custom must also be TRUE
#'
#' @return List of character representations, or data frame containing errors
make_character_representations <- function(root,
                                           data,
                                           neutralize_custom,
                                           overwrite_custom_weight) {
  has_ancestral <- !is.null(root)
  if (has_ancestral) {
    assertthat::assert_that("irreversible" %in% data[["chartype"]] ||
                              any(grepl("^custom:.+$", data[["chartype"]])))
  } else {
    assertthat::assert_that(unique(data[["chartype"]]) == "standard")
  }

  char_reps <- list()
  error_table <- data.frame()

  for (i in seq_len(nrow(data))) {
    char_rep <- make_character_representation(data[i, ],
                                              root,
                                              has_ancestral,
                                              neutralize_custom,
                                              overwrite_custom_weight)
    char_reps[[i]] <- char_rep
    if (typeof(char_rep) == "character") { # ERROR!
      error_table <- rbind(error_table,
                           data.frame(id = data[i, "id"],
                                      feature = data[i, "feature"],
                                      error = char_rep))
    }
  }

  if (nrow(error_table) > 0) return(error_table)
  else return(char_reps)

}




#' Make custom transition cost matrix
#'
#' @param string Custom matrix specified as a string
#' @param states_char Unique states vector (must be in order)
#'
#' @return Custom transition cost matrix
make_custom_matrix <- function(string, states_char) {

  string <- gsub(" ", "", string)
  string <- gsub(",", ";", string)
  splt <- stringr::str_split(string, ";")[[1]]

  if (length(split) == 0) return(NULL)

  mat <- 1 - diag(length(states_char))
  mat[mat == 1] <- "i"

  for (x in splt) {
    regex_with_cost <- "([^>():])>([^>():])\\([c|w]:(\\d+)\\)"
    regex_wo_cost <- "([^>():])>([^>():])"

    # May match both, in which case first should be used
    strmatch_with_cost <- stringr::str_match(x, regex_with_cost)
    strmatch_wo_cost <- stringr::str_match(x, regex_wo_cost)

    if (all(!is.na(strmatch_with_cost)) && x == strmatch_with_cost[1, 1]) {
      state0 <- strmatch_with_cost[1, 2]
      state1 <- strmatch_with_cost[1, 3]
      cost <- strmatch_with_cost[1, 4]
      idx0 <- which(states_char == state0)
      idx1 <- which(states_char == state1)
      if (length(idx0) != 1 || length(idx1) != 1) return(NULL)
      mat[idx0, idx1] <- cost
    } else if (all(!is.na(strmatch_wo_cost)) && x == strmatch_wo_cost[1, 1]) {
      state0 <- strmatch_wo_cost[1, 2]
      state1 <- strmatch_wo_cost[1, 3]
      idx0 <- which(states_char == state0)
      idx1 <- which(states_char == state1)
      if (length(idx0) != 1 || length(idx1) != 1) return(NULL)
      mat[idx0, idx1] <- "1"
    } else {
      return(NULL)
    }
  }
  mat
}





#' Make cost matrix for asr_max_parsimony.
#'
#' @param unique_states Unique states vector (must be in order)
#' @param total_num_states Total number of states (including ? states)
#' @param style One of "standard", "irreversible", or "custom"
#' @param custom_matrix Custom matrix if style == "custom" else NULL
#'
#' @return Cost matrix for asr_max_parsimony. Includes rows/columns for all ?
#'   characters.
make_cost_matrix_for_parsimony <- function(unique_states,
                                           total_num_states,
                                           style,
                                           custom_matrix = NULL) {
  # total_num_states includes questions

  # Assertions
  assertthat::assert_that({
    style %in% c("standard", "irreversible", "custom")
  })
  assertthat::assert_that({
    total_num_states >= length(unique_states)
  })

  # Create matrix that will be returned
  transition_costs <- 1 - diag(total_num_states)

  # "?" states are only allowed to be at leaves (cannot participate in
  #   reconstructions - do this by setting their transition to infinity)
  if (length(unique_states) < total_num_states) {
    for (q in (length(unique_states) + 1):total_num_states) { # the ? states
      transition_costs[q, ][-q] <- Inf
    }
  }

  # Irreversible means can't go from anything to 0 (any other transition is
  #   fine, and ?'s are as above)
  if (style == "irreversible") {
    assertthat::assert_that({
      unique_states[1] == "0"
    })
    transition_costs[2:nrow(transition_costs), 1] <- Inf
  } else if (style == "custom") {
    assertthat::assert_that({
      !is.null(custom_matrix)
    })

    transition_costs[transition_costs == 1] <- Inf
    for (i in seq_len(nrow(transition_costs))) {
      if (i > nrow(custom_matrix)) {
        transition_costs[i, ][-i] <- Inf
        transition_costs[1:(i - 1), i] <- 1
      } else {
        vec <- custom_matrix[i, ]
        vec[vec == "i"] <- "-1"
        vec <- as.integer(vec)
        vec[vec == -1] <- Inf
        transition_costs[i, seq_along(vec)] <- vec
      }
    }
  }

  transition_costs
}



#' Make cost matrix for irreversible character
#' @param num_states Number of states
#'
#' @return Cost matrix
make_irreverseible_matrix <- function(num_states) {
  mat <- matrix(NA, nrow = num_states, ncol = num_states)
  for (i in 1:num_states) {
    for (j in 1:num_states) {
      if (i == j) mat[i, j] <- 0
      else if (j > i) mat[i, j] <- 1
      else if (j < i && j == 1) mat[i, j] <- "i"
      else if (j < i && j > 1) mat[i, j] <- 1
      else stop("Case not possible logically.")
    }
  }
  mat
}
