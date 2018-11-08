
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a rotation matrix
#'
#' from https://en.wikipedia.org/wiki/Rotation_matrix
#'
#' @param angle Angle (radians)
#' @param v vector to rotate around
#'
#' @return 3d rotation matrix
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rotation_matrix <- function(angle, v) {
  if (angle == 0) {
    return(identity_matrix())
  }

  u <- vec3_normalize(v[1:3])

  x <- u[1]
  y <- u[2]
  z <- u[3]
  c <- cos(angle)
  s <- sin(angle)
  t <- 1 - c

  matrix(c(
    t*x*x + c  , t*x*y - s*z, t*x*z + s*y,   0,
    t*x*y + s*z, t*y*y + c  , t*y*z - s*x,   0,
    t*x*z - s*y, t*y*z + s*x, t*z*z + c  ,   0,
    0          ,           0,           0,   1
  ), byrow=TRUE, ncol=4)

}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Rotate the given object
#'
#' @inheritParams rotation_matrix
#' @param x matrix, mesh3d object or numeric vector of length 3 or 4 (homogenous coordinates)
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rotate_by <- function(x, angle, v) {
  transform_by(x, rotation_matrix(angle, v))
}
