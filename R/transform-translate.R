
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a translation matrix
#'
#' @param v 3 element vector giving translation along each dimension
#'
#' @return 3d translation matrix
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
translation_matrix <- function(v) {
  transform_matrix         <- diag(4)
  transform_matrix[1:3, 4] <- v[1:3]
  transform_matrix
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Translate the given object
#'
#' @inheritParams translation_matrix
#' @param x matrix, mesh3d object or numeric vector of length 3 or 4 (homogenous coordinates)
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
translate_by <- function(x, v) {
  transform_by(x, translation_matrix(v))
}

