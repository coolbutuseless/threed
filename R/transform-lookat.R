
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a camera 'look-at' transformation matrix i.e. camera-to-world transform
#'
#' @param eye position of camera (3-element vector)
#' @param at position in space to look at.  (3 element vector)
#'
#' @return homogenous 4x4 matrix
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
look_at_matrix <- function(eye, at) {

  forward <- vec3_normalize(eye - at)
  right   <- vec3_normalize(vec3_crossproduct(c(0, 1, 0), forward))
  up      <- vec3_crossproduct(forward, right)

  matrix(c(
    right[1], up[1], forward[1], eye[1],
    right[2], up[2], forward[2], eye[2],
    right[3], up[3], forward[3], eye[3],
    0       , 0       , 0       , 1
  ), byrow = TRUE, ncol = 4)
}
