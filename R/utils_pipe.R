#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Ggtree backward operator
#'
#' See https://bioc.ism.ac.jp/packages/3.5/bioc/vignettes/ggtree/inst/doc/
#' treeAnnotation.html#tree-annotation-with-user-specified-annotation
#'
#' @name %<+%
#' @keywords internal
#' @importFrom ggtree %<+%
#' @usage lhs \%<+\% rhs
#' @param lhs A value
#' @param rhs A value
#' @return The result of %<+%.
NULL
