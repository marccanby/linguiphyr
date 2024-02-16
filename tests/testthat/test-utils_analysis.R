test_that("find_uninformative_characters", {
  gold <- list(uninf = c(1, 2, 3, 4),
               numbigstates = c(1, 0, 0, 0))

  input <- data.frame(id = paste0("a", 1:4),
                      feature = paste0("a", 1:4),
                      weight = 1:4,
                      chartype = rep("standard", 4),
                      A = c("0", "2", "?", "1"),
                      B = c("1", "?", "?", "2"),
                      C = c("?", "0", "?", "3"),
                      D = c("?", "?", "?", "4"),
                      E = c("1", "3", "1", "5"))

  expect_equal(find_uninformative_characters(input), gold)

  gold <- list(uninf = c(3),
               numbigstates = c(2, 2, 0, 2))

  input <- data.frame(id = paste0("a", 1:4),
                      feature = paste0("a", 1:4),
                      weight = 1:4,
                      chartype = rep("standard", 4),
                      A = c("0", "3", "?", "1"),
                      B = c("1", "3", "?", "2"),
                      C = c("?", "0", "?", "5"),
                      D = c("0", "0", "?", "1"),
                      E = c("1", "3", "1", "5"))

  expect_equal(find_uninformative_characters(input), gold)

})

test_that("find_chars_supporting_clade", {
  gold <- data.frame(id = as.character(),
                     feature = as.character(),
                     weight = as.integer(),
                     chartype = as.character(),
                     A = as.character(),
                     B = as.character(),
                     C = as.character(),
                     D = as.character(),
                     E = as.character())

  input <- data.frame(id = paste0("a", 1:4),
                      feature = paste0("a", 1:4),
                      weight = 1:4,
                      chartype = rep("standard", 4),
                      A = c("0", "2", "?", "1"),
                      B = c("1", "?", "?", "2"),
                      C = c("?", "0", "?", "3"),
                      D = c("?", "?", "?", "4"),
                      E = c("1", "3", "1", "5"))

  expect_equal(find_chars_supporting_clade(c("A", "B"), input), gold)

  gold <- data.frame(id = paste0("a", 1),
                     feature = paste0("a", 1),
                     weight = 1,
                     chartype = rep("standard", 1),
                     A = c("0"),
                     B = c("1"),
                     C = c("?"),
                     D = c("?"),
                     E = c("1"))

  input <- data.frame(id = paste0("a", 1:4),
                      feature = paste0("a", 1:4),
                      weight = 1:4,
                      chartype = rep("standard", 4),
                      A = c("0", "2", "?", "1"),
                      B = c("1", "?", "?", "2"),
                      C = c("?", "0", "?", "3"),
                      D = c("?", "?", "?", "4"),
                      E = c("1", "3", "1", "5"))

  expect_equal(find_chars_supporting_clade(c("B", "E"), input), gold)

  gold <- data.frame(id = paste0("a", 1),
                     feature = paste0("a", 1),
                     weight = 1,
                     chartype = rep("standard", 1),
                     A = c("0"),
                     B = c("1"),
                     C = c("?"),
                     D = c("?"),
                     E = c("1"))

  input <- data.frame(id = paste0("a", 1:4),
                      feature = paste0("a", 1:4),
                      weight = 1:4,
                      chartype = rep("standard", 4),
                      A = c("0", "2", "1", "1"),
                      B = c("1", "?", "1", "2"),
                      C = c("?", "0", "?", "3"),
                      D = c("?", "2", "?", "4"),
                      E = c("1", "?", "1", "5"))

  expect_equal(find_chars_supporting_clade(c("B", "E"), input), gold)

  gold <- data.frame(id = paste0("a", c(1, 4)),
                     feature = paste0("a", c(1, 4)),
                     weight = c(1, 4),
                     chartype = rep("standard", 2),
                     A = c("0", "1"),
                     B = c("1", "2"),
                     C = c("?", "3"),
                     D = c("?", "4"),
                     E = c("1", "2"))
  gold <- data.frame(gold, row.names = NULL)

  input <- data.frame(id = paste0("a", 1:4),
                      feature = paste0("a", 1:4),
                      weight = 1:4,
                      chartype = rep("standard", 4),
                      A = c("0", "2", "1", "1"),
                      B = c("1", "?", "1", "2"),
                      C = c("?", "0", "?", "3"),
                      D = c("?", "2", "?", "4"),
                      E = c("1", "?", "1", "2"))

  res <- find_chars_supporting_clade(c("B", "E"), input)
  res <- data.frame(res, row.names = NULL)
  expect_equal(res, gold)

  input <- data.frame(id = paste0("a", 1:4),
                      feature = paste0("a", 1:4),
                      weight = 1:4,
                      chartype = rep("standard", 4),
                      A = c("0", "2", "1", "1"),
                      B = c("1", "?", "1", "2"),
                      C = c("?", "0", "?", "3"),
                      D = c("?", "2", "?", "4"),
                      E = c("1", "?", "1", "2"))

  expect_error(find_chars_supporting_clade(c("A"), input),
               "Clade must contain at least two languages.")

})
