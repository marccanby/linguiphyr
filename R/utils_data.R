#' Read linguistic data and do basic validation
#'
#' @param path A string pointing to the file path
#'
#' @return A data frame if successful, or a string containing error message
#' if not
read_and_validate_data <- function(path) {
  # Note that this just does basic verification; it
  # does not check that char types can be fully
  # parsed given the char states (given that that
  # depends on the preprocessing as well).

  data <- utils::read.csv(path)
  if (ncol(data) == 1) return("Data must be a ','-separated CSV file.")

  if (!("id" %in% names(data))) {
    return(paste0("The dataset must contain a column titled 'id'",
                  " (containing the id of each character)."))
  }
  if (!("feature" %in% names(data))) {
    return(paste0("The dataset must contain a column titled 'feature'",
                  " (containing the name of each character)."))
  }
  ids <- data[["id"]]
  features <- data[["feature"]]

  if ("weight" %in% names(data)) weights <- data[["weight"]]
  else weights <- rep(1, nrow(data))
  if ("chartype" %in% names(data)) chartypes <- data[["chartype"]]
  else chartypes <- rep("standard", nrow(data))

  if (!all(grepl("^standard$|^custom:.+$|^irreversible$", chartypes))) {
    return(paste0("The 'chartype' column must only contain values from",
                  " {'standard', 'irreversible', 'custom:...'}."))
  }

  data <- data[, -which(names(data) %in% c("id", "feature",
                                           "weight", "chartype"))]
  data <- cbind(data.frame(id = ids, feature = features,
                           weight = weights, chartype = chartypes), data)

  if (sum(is.na(data)) > 0 || sum(data == "") > 0) {
    return("There can be no blank cells in the dataset.")
  }
  if (sum(is.na(as.integer(data[, 3]))) > 0 ||
        sum(as.numeric(data[, 3]) %% 1 != 0) > 0) {
    return("The 'weight' column must have all integer values.")
  }

  return(data)
}




#' Preprocessing the linguistic data
#'
#' @param df Data frame to preprocess
#' @param poly_choice String, how to handle polymorphism
#'
#' @return List containing:
#'   df_with_poly: Data frame containing the polymorphisms
#'   df: Preprocessed data frame
#'   num_poly: Numeric vector containing number of polymorphic states per
#'     character
#'   polywid: Numeric vector containing the maximum number of polymorphic
#'     states per character
preprocess_data <- function(df,  poly_choice) {

  assertthat::assert_that({
                           poly_choice %in% c("Replace all with ?",
                                              "Replace unique with ?",
                                              "Replace with majority")})

  colstart <- 5 # Start of data in data frame

  # Count the # of poly states per language and max poly width
  numpoly <- rep(0, nrow(df))
  polywid <- rep(0, nrow(df))
  for (col in colstart:(ncol(df))) {
    bools <- grepl("/", df[, col])
    numpoly <- numpoly + as.integer(bools)
    cnts <- stringr::str_count(df[, col], "/")
    polywid <- pmax(polywid, cnts)
  }
  polywid <- polywid + 1
  polywid[polywid == 1] <- "-"

  res <- list(df_with_poly = df)

  if (poly_choice == "Replace all with ?") {
    for (col in colstart:(ncol(df))) {
      bools <- grepl("/", df[, col])
      df[bools, col] <- "?"
    }
  } else if (poly_choice == "Replace with majority") {
    for (char in seq_len(nrow(df))) {
      # First get counts of each state
      dct <- list()
      sts <- df[char, colstart:ncol(df)]
      if (!any(grepl("/", sts))) next
      for (col in colstart:ncol(df)) {
        state <- df[char, col]
        state <- as.character(state)
        if (grepl("/", state)) {
          splt <- stringr::str_split(state, "/")[[1]]
          for (st in splt) {
            if (st %in% names(dct)) dct[[st]] <- dct[[st]] + 1
            else dct[[st]] <- 1
          }
        } else {
          st <- state
          if (!is.null(names(dct)) && (st %in% names(dct))) {
            dct[[st]] <- dct[[st]] + 1
          } else {
            dct[[st]] <- 1
          }
        }
      }

      # Now, replace each polymorphic state with the one with highest counts.
      # Ties go to lowest id (so it's arbitrary and deterministic).
      for (col in colstart:ncol(df)) {
        state <- df[char, col]
        if (grepl("/", state)) {
          splt <- stringr::str_split(state, "/")[[1]]
          max_cnt <- 0
          argmax <- NULL
          for (st in splt) {
            if (dct[[st]] > max_cnt) {
              max_cnt <- dct[[st]]
              argmax <- c(argmax, st)
            }
          }
          if (length(argmax) > 1) argmax <- argmax[order(argmax)][1]
          df[char, col] <- argmax
        } else {
          next
        }
      }
    }
  } else if (poly_choice == "Replace unique with ?") {
    for (char in seq_len(nrow(df))) {
      # First get counts of each state
      sts <- unlist(df[char, colstart:ncol(df)])
      if (!any(grepl("/", sts))) next

      unique_sts <- unique(sts)
      sts_copy <- sts
      sts_copy[grepl("/", sts_copy)] <- -1
      sts_copy[sts == "?"] <- -1
      sts_copy <- as.integer(sts_copy)
      new_id <- max(sts_copy) + 1


      sts_h <- sts
      for (state in unique_sts) {
        if (!grepl("/", state)) next
        else if (sum(sts_h == state) == 1) sts_h[sts_h == state] <- "?"
        else if (sum(sts_h == state) > 1) {
          sts_h[sts_h == state] <- new_id
          new_id <- new_id + 1
        } else {
          stop("Case not possible logically.")
        }
      }
      df[char, colstart:ncol(df)] <- sts_h
      assertthat::assert_that({
        !any(is.na(df[char, ]))
      })
    }
  } else {
    stop("Case not possible logically.")
  }

  # just confirm no poly left
  for (col in colstart:ncol(df)) assertthat::assert_that({
    !any(grepl("/", df[, col]))
  })

  res$df <- df
  res$num_poly <- numpoly
  res$polywid <- polywid
  res
}






#' Replace ?'s with unique numbers in linguistic data frame
#'
#' @param df Linguistic data frame
#'
#' @return Linguistic data frame, where all ?'s are replaced with unique
#'   numbers
replace_qs_with_nums <- function(df) {
  colstart <- 5
  for (i in seq_len(nrow(df))) {
    row <- df[i, colstart:ncol(df)]
    start <- max(as.integer(row[row != "?"])) + 1
    for (q in colstart:ncol(df)) {
      if (df[i, q] == "?") {
        df[i, q] <- start
        start <- start + 1
      }
    }
  }
  df
}
