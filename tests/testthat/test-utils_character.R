test_that("make_custom_matrix: binary irreversible", {
  gold <- matrix(
    c("0", "1",
      "i", "0"),
    nrow = 2,
    byrow = TRUE
  )

  expect_equal(make_custom_matrix("0>1", 0:1), gold)
  expect_equal(make_custom_matrix("1>2", 1:2), gold)
})

test_that("make_custom_matrix: binary assymetric", {
  gold <- matrix(
    c("0", "5",
      "1", "0"
    ),
    nrow = 2,
    byrow = TRUE
  )
  expect_equal(make_custom_matrix("0>1(c:5);1>0(c:1)", 0:1), gold)

  gold <- matrix(
    c("0", "7",
      "1", "0"
    ),
    nrow = 2,
    byrow = TRUE
  )
  expect_equal(make_custom_matrix("1>2(c:7);2>1(c:1)", 1:2), gold)
})

test_that("make_custom_matrix: multi-state irreversible", {
  gold <- matrix(
    c("0", "1", "1", "1", "1", "1", "1",
      "i", "0", "i", "i", "i", "i", "i",
      "i", "i", "0", "i", "i", "i", "i",
      "i", "i", "i", "0", "i", "i", "i",
      "i", "i", "i", "i", "0", "i", "i",
      "i", "i", "i", "i", "i", "0", "i",
      "i", "i", "i", "i", "i", "i", "0"),
    nrow = 7,
    byrow = TRUE
  )
  expect_equal(make_custom_matrix("0>1;0>2;0>3;0>4;0>5;0>6", 0:6), gold)

  gold <- matrix(
    c("0", "1", "1", "1", "1",
      "i", "0", "i", "i", "i",
      "i", "i", "0", "i", "i",
      "i", "i", "i", "0", "i",
      "i", "i", "i", "i", "0"),
    nrow = 5,
    byrow = TRUE
  )
  expect_equal(make_custom_matrix("0>1;0>2;0>3;0>4", 0:4), gold)
})

test_that("make_custom_matrix: multi-state custom", {
  gold <- matrix(
    c("0", "8", "1", "3",
      "i", "0", "3", "5",
      "i", "i", "0", "i",
      "i", "i", "1", "0"),
    nrow = 4,
    byrow = TRUE
  )
  string <- "0>1(c:8);0>2(c:1);0>3(c:3);1>2(c:3);1>3(c:5);3>2(c:1)"
  expect_equal(make_custom_matrix(string, 0:3), gold)
})

test_that("make_cost_matrix_for_parsimony: standard", {
  gold <- matrix(c(
    0, 1, 1, 1, 1,
    1, 0, 1, 1, 1,
    1, 1, 0, 1, 1,
    1, 1, 1, 0, 1,
    1, 1, 1, 1, 0
  ),
  nrow = 5,
  byrow = TRUE)

  uq_states <- c("10", "20", "2", "3", "4")
  expect_equal(make_cost_matrix_for_parsimony(unique_states = uq_states,
                                              total_num_states = 5,
                                              style = "standard",
                                              custom_matrix = NULL),
               gold)

  gold <- matrix(c(
    0, 1, 1, 1, 1, 1,
    1, 0, 1, 1, 1, 1,
    1, 1, 0, 1, 1, 1,
    1, 1, 1, 0, 1, 1,
    1, 1, 1, 1, 0, 1,
    Inf, Inf, Inf, Inf, Inf, 0
  ),
  nrow = 6,
  byrow = TRUE)

  uq_states <- c("9", "12", "2", "5", "4")
  expect_equal(make_cost_matrix_for_parsimony(unique_states = uq_states,
                                              total_num_states = 6,
                                              style = "standard",
                                              custom_matrix = NULL), gold)

  gold <- matrix(c(
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 0, Inf, Inf,
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 0, Inf,
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 0
  ),
  nrow = 11,
  byrow = TRUE)

  uq_states <- c("1", "20", "2", "3", "4", "5", "6", "8")
  expect_equal(make_cost_matrix_for_parsimony(unique_states = uq_states,
                                              total_num_states = 11,
                                              style = "standard",
                                              custom_matrix = NULL), gold)
})

test_that("make_cost_matrix_for_parsimony: irreversible", {
  gold <- matrix(c(
    0, 1,
    Inf, 0
  ),
  nrow = 2,
  byrow = TRUE)

  expect_equal(make_cost_matrix_for_parsimony(unique_states = c("0", "1"),
                                              total_num_states = 2,
                                              style = "irreversible",
                                              custom_matrix = NULL), gold)

  gold <- matrix(c(
    0, 1, 1, 1, 1, 1, 1, 1, 1,
    Inf, 0, 1, 1, 1, 1, 1, 1, 1,
    Inf, Inf, 0, Inf, Inf, Inf, Inf, Inf, Inf,
    Inf, Inf, Inf, 0, Inf, Inf, Inf, Inf, Inf,
    Inf, Inf, Inf, Inf, 0, Inf, Inf, Inf, Inf,
    Inf, Inf, Inf, Inf, Inf, 0, Inf, Inf, Inf,
    Inf, Inf, Inf, Inf, Inf, Inf, 0, Inf, Inf,
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, 0, Inf,
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 0
  ),
  nrow = 9,
  byrow = TRUE)

  expect_equal(make_cost_matrix_for_parsimony(unique_states = c("0", "1"),
                                              total_num_states = 9,
                                              style = "irreversible",
                                              custom_matrix = NULL), gold)

})

test_that("make_cost_matrix_for_parsimony: custom", {
  gold <- matrix(c(
    0, 1, 1, 1, 1, 1, 1, 1,
    Inf, 0, 1, 1, 1, 1, 1, 1,
    Inf, Inf, 0, 1, 1, 1, 1, 1,
    Inf, Inf, Inf, 0, 1, 1, 1, 1,
    Inf, Inf, Inf, Inf, 0, 1, 1, 1,
    Inf, Inf, Inf, Inf, Inf, 0, 1, 1,
    Inf, Inf, Inf, Inf, Inf, Inf, 0, 1,
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, 0
  ),
  nrow = 8,
  byrow = TRUE)

  expect_equal(make_cost_matrix_for_parsimony(unique_states = c("1", "2"),
                                              total_num_states = 8,
                                              style = "custom",
                                              custom_matrix = matrix(c(
                                                "0", "1",
                                                "i", "0"
                                              ),
                                              nrow = 2,
                                              byrow = TRUE)), gold)

  gold <- matrix(c(
    0, 3, 1,
    1, 0, 1,
    Inf, Inf, 0
  ),
  nrow = 3,
  byrow = TRUE)

  expect_equal(make_cost_matrix_for_parsimony(unique_states = c("1", "2"),
                                              total_num_states = 3,
                                              style = "custom",
                                              custom_matrix = matrix(c(
                                                "0", "3",
                                                "1", "0"
                                              ),
                                              nrow = 2,
                                              byrow = TRUE)), gold)

  gold <- matrix(c(
    0, 3, 1, 3,
    Inf, 0, 3, 5,
    Inf, Inf, 0, Inf,
    Inf, Inf, 1, 0
  ),
  nrow = 4,
  byrow = TRUE)

  uq_states <- c("0", "1", "2", "3")
  expect_equal(make_cost_matrix_for_parsimony(unique_states = uq_states,
                                              total_num_states = 4,
                                              style = "custom",
                                              custom_matrix = matrix(c(
                                                "0", "3", "1", "3",
                                                "i", "0", "3", "5",
                                                "i", "i", "0", "i",
                                                "i", "i", "1", "0"
                                              ),
                                              nrow = 4,
                                              byrow = TRUE)), gold)

})

test_that("make_irreverseible_matrix", {
  gold <- matrix(c(
    "0", "1",
    "i", "0"
  ),
  nrow = 2,
  byrow = TRUE)

  expect_equal(make_irreverseible_matrix(2), gold)

  gold <- matrix(c(
    "0", "1", "1",
    "i", "0", "1",
    "i", "1", "0"
  ),
  nrow = 3,
  byrow = TRUE)

  expect_equal(make_irreverseible_matrix(3), gold)

  gold <- matrix(c(
    "0", "1", "1", "1",
    "i", "0", "1", "1",
    "i", "1", "0", "1",
    "i", "1", "1", "0"
  ),
  nrow = 4,
  byrow = TRUE)

  expect_equal(make_irreverseible_matrix(4), gold)

  gold <- matrix(c(
    "0", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "i", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "i", "1", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "i", "1", "1", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "i", "1", "1", "1", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "i", "1", "1", "1", "1", "0", "1", "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "i", "1", "1", "1", "1", "1", "0", "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "i", "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "i", "1", "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "i", "1", "1", "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "i", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "i", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "i", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "1",
    "1", "1", "1", "1", "1", "1",
    "i", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0",
    "1", "1", "1", "1", "1", "1",
    "i", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
    "0", "1", "1", "1", "1", "1",
    "i", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
    "1", "0", "1", "1", "1", "1",
    "i", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "0", "1", "1", "1",
    "i", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "0", "1", "1",
    "i", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "0", "1",
    "i", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "0"
  ),
  nrow = 20,
  byrow = TRUE)

  expect_equal(make_irreverseible_matrix(20), gold)

})
