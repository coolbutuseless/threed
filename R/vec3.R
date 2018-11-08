
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a vec3 object
#'
#' @param x,y,z vector. default c(0, 0, 0)
#'
#' @return vec3
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vec3 <- function(x = 0, y = 0, z = 0) {
  c(x, y, z)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calculate the vector cross-product of 2 3d vectors
#'
#' @param v1,v2 vectors of length 3
#'
#' @return vector cross-product
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vec3_crossproduct <- function(v1, v2) {
  v1[c(2L, 3L, 1L)] * v2[c(3L, 1L, 2L)] - v1[c(3L, 1L, 2L)] * v2[c(2L, 3L, 1L)]
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calculate the vector dot-product of 2 3d vectors
#'
#' @param v1,v2 vectors of length 3
#'
#' @return vector dot-product
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vec3_dotproduct <- function(v1, v2) {
  stopifnot(length(v1)==3L, length(v2)==3L)
  v1 %*% v2
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Normalize a vector to be of unit length
#'
#' @param v vector of length 3
#'
#' @return vector normalized to unit length
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vec3_normalize <- function(v) {
  v[1:3] / sqrt(sum(v[1:3]^2))
}
