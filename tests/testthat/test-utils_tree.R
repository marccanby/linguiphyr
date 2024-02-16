test_that("collapse_edge: valid input", {
  tree <- ape::read.tree(text =
                           "(A,(B,(((C,D),E),(((F,G),H),((I,J),(K,L))))),M);")
  gold <- ape::read.tree(text =
                           "(A,(B,(((C,D),E),((F,G),H,((I,J),(K,L))))),M);")
  coll <- collapse_edge(tree, 19, 20)
  expect_true(ape::all.equal.phylo(coll, gold))

  tree <- ape::read.tree(text =
                           "(A,(B,(((C,D),E),(((F,G),H),((I,J),(K,L))))),M);")
  gold <- ape::read.tree(text =
                           "(A,(B,(((C,D),E),((F,G),H),((I,J),(K,L)))),M);")
  coll <- collapse_edge(tree, 16, 19)
  expect_true(ape::all.equal.phylo(coll, gold))

  gold <- ape::read.tree(text = "(A,(B,((C,D),E,((F,G),H),((I,J),(K,L)))),M);")
  coll <- collapse_edge(coll, 16, 17)
  expect_true(ape::all.equal.phylo(coll, gold))

  gold <- ape::read.tree(text = "(A,(B,(C,D),E,((F,G),H),((I,J),(K,L))),M);")
  coll <- collapse_edge(coll, 15, 16)
  expect_true(ape::all.equal.phylo(coll, gold))

  for (i in 1:6) coll <- collapse_edge(coll, 15, 16)
  gold <- ape::read.tree(text = "(A,(B,C,D,E,F,G,H,I,J,K,L),M);")
  expect_true(ape::all.equal.phylo(coll, gold))
})

test_that("collapse_edge: invalid input", {
  tree <- ape::read.tree(text =
                           "(A,(B,(((C,D),E),(((F,G),H),((I,J),(K,L))))),M);")
  expect_error(collapse_edge(tree, 19, 1), "Edge does not exist in tree.")

  tree <- ape::read.tree(text =
                           "(A,(B,(((C,D),E),(((F,G),H),((I,J),(K,L))))),M);")
  expect_error(collapse_edge(tree, 16, 20), "Edge does not exist in tree.")

  tree <- ape::read.tree(text =
                           "(A,(B,(((C,D),E),(((F,G),H),((I,J),(K,L))))),M);")
  expect_error(collapse_edge(tree, 20, 8),
               "Collapsing leaf edges is not supported.")

  tree <- ape::read.tree(text =
                           "(A,(B,(((C,D),E),(((F,G),H),((I,J),(K,L))))),M);")
  expect_error(collapse_edge(tree, 14, 1),
               "Collapsing leaf edges is not supported.")
})

test_that("make_consensus_trees", {
  tree1 <- ape::read.tree(text =
                            "(A,(B,(((C,D),E),(((F,G),H),((I,J),(K,L))))),M);")
  tree2 <- ape::read.tree(text =
                            "(A,(B,(((C,E),D),(((F,G),H),((I,J),(K,L))))),M);")
  tree3 <- ape::read.tree(text =
                            "(A,(B,(((E,D),C),(((F,G),H),((I,J),(K,L))))),M);")
  trees <- list(tree1, tree2, tree3)
  cons <- make_consensus_trees(list(trees = trees))
  expect_equal(cons$names, c("Strict Consensus", "Majority Consensus"))
  strict <- ape::read.tree(text =
                             "((((A,M),B),(C,D,E)),((I,J),(K,L)),((F,G),H));")
  majori <- ape::read.tree(text =
                             "((((A,M),B),(C,D,E)),((I,J),(K,L)),((F,G),H));")
  expect_true(ape::all.equal.phylo(cons$trees[[1]], strict))
  expect_true(ape::all.equal.phylo(cons$trees[[2]], majori))

  tree4 <- ape::read.tree(text =
                            "(A,(B,(((C,D),E),(((F,H),G),((I,J),(K,L))))),M);")
  trees <- list(tree1, tree2, tree3, tree4)
  cons <- make_consensus_trees(list(trees = trees))
  expect_equal(cons$names, c("Strict Consensus", "Majority Consensus"))
  strict <- ape::read.tree(text =
                             "((((A,M),B),(C,D,E)),((I,J),(K,L)),(F,G,H));")
  majori <- ape::read.tree(text =
                             "((((A,M),B),(C,D,E)),((I,J),(K,L)),((F,G),H));")
  expect_true(ape::all.equal.phylo(cons$trees[[1]], strict))
  expect_true(ape::all.equal.phylo(cons$trees[[2]], majori))
})
