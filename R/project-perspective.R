
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a perspective projection matrix (symmetric frustrum)
#'
#' @param w,h,n,f width, height, near, far boundaries
#'
#' @return perspective projection matrix
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
perspective_projection_matrix <- function(w = 2, h = 2, n = 1, f = 10) {
  stopifnot(w > 0 && h > 0 && n > 0 && f > 0)

  matrix(c(
    2 * n/w,  0      ,  0              , 0,
    0      ,  2 * n/h,  0              , 0,
    0      ,  0      , -(f + n)/(f - n), 2 * f * n/(f - n),
    0      ,  0      , -1              , 0
  ), byrow=TRUE, ncol=4)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Perspective projection (symmetric frustrum)
#'
#' Perform perspective projection
#'
#' @inheritParams perspective_projection_matrix
#' @param x matrix, mesh3d, vector or data.frame object
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
perspective_projection <- function(x, w = 2, h = 2, n = 1, f = 10) {
  transform_by(x, perspective_projection_matrix(w, h, n, f))
}
