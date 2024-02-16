test_that("replace_qs_with_nums", {
  gold <- data.frame(id = paste0("a", 1:4),
                     feature = paste0("a", 1:4),
                     weight = 1:4,
                     chartype = rep("standard", 4),
                     A = c("0", "2", "2", "1"),
                     B = c("1", "4", "3", "2"),
                     C = c("2", "0", "4", "3"),
                     D = c("3", "5", "5", "4"),
                     E = c("1", "3", "1", "5"))

  input <- data.frame(id = paste0("a", 1:4),
                      feature = paste0("a", 1:4),
                      weight = 1:4,
                      chartype = rep("standard", 4),
                      A = c("0", "2", "?", "1"),
                      B = c("1", "?", "?", "2"),
                      C = c("?", "0", "?", "3"),
                      D = c("?", "?", "?", "4"),
                      E = c("1", "3", "1", "5"))

  expect_equal(replace_qs_with_nums(input), gold)

  gold <- data.frame(id = paste0("a", 1:4),
                     feature = paste0("a", 1:4),
                     weight = 1:4,
                     chartype = rep("standard", 4),
                     A = c("0", "2", "2", "1"),
                     B = c("1", "4", "3", "2"),
                     C = c("2", "0", "4", "3"),
                     D = c("3", "5", "5", "4"),
                     E = c("1", "3", "1", "5"))

  input <- gold

  expect_equal(replace_qs_with_nums(input), gold)

})
