
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an identity matrix
#'
#' @param n size. default: 4
#'
#' @return identity matrix of given size
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
identity_matrix <- function(n = 4) {
  diag(nrow=n)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Invert a matrix
#'
#' @param mat matrix to invert
#'
#' @return inverted matrix
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
invert_matrix <- function(mat) {
  solve(mat)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Actualize a transform
#'
#' Applying transformations to a mesh3d object only update the object's
#' transformation matrix, and does not actually modify any vertices.
#'
#' This function applies the transformation to the vertices.
#'
#' @param obj mesh3d object
#'
#' @return transformed object
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
actualize_transformation <- function(obj) {

  # Is there any transformation? if not, return the original object
  if (is.null(obj$transform_matrix)) {
    return(obj)
  }

  # otherwise transform the vertices
  vb     <- obj$vb
  vb     <- obj$transform_matrix %*% vb
  vb     <- t( t(vb)/vb[4,] )
  obj$vb <- vb

  obj$transform_matrix <- NULL

  # recalculate normals if already present
  if (!is.null(obj$normals) | !is.null(obj$face_normals)) {
    obj <- add_normals(obj)
  }

  obj
}
