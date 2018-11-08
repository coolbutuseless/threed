
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a simple orthographic projection matrix
#'
#' @param w,h,n,f width, height, near far frustrum boundaries
#' @return orthographic projection matrix
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
orthographic_projection_matrix <- function(w = 2, h = 2, n = 1, f = 10) {
  stopifnot(w > 0 && h > 0 && n > 0 && f > 0)

  matrix(c(
    2/w, 0  , 0       , 0,
    0  , 2/h, 0       , 0,
    0  , 0  , 2/(f-n) , -(f+n)/(f - n),
    0  , 0  , 0       , 1
  ), byrow = TRUE, ncol = 4)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' orthographic projection
#'
#' Perform orthographic projection.
#'
#' @inheritParams orthographic_projection_matrix
#' @param x matrix, mesh3d, vector or data.frame object
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
orthographic_projection <- function(x, w = 2, h = 2, n = 1, f = 10) {
  transform_by(x, orthographic_projection_matrix(w, h, n, f))
}
