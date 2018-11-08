
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a scaling matrix
#'
#' @param v 3 element vector giving scaling along each dimension
#'
#' @return 3d scaling matrix
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scaling_matrix <- function(v) {
  stopifnot(length(v) %in% 3:4)
  diag(c(v[1:3], 1))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Scale the given object
#'
#' @inheritParams scaling_matrix
#' @param x matrix, mesh3d object or numeric vector of length 3 or 4 (homogenous coordinates)
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_by <- function(x, v) {
  transform_by(x, scaling_matrix(v))
}
