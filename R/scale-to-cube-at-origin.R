
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Resize to standard dimension i.e. within a 2x2x2 cube centred at origin
#'
#' @param x matrix, mesh3d object or numeric vector of length 3 or 4 (homogenous coordinates)
#' @param size length of side of cube. default 2
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_to_cube_at_origin <- function(x, size = 2) {
  UseMethod("scale_to_cube_at_origin")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Resize to standard dimension i.e. within a 2x2x2 cube centred at origin
#'
#' @inheritParams scale_to_cube_at_origin
#' @param x mesh3d object
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_to_cube_at_origin.mesh3d <- function(x, size = 2) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the min/max extents of the vertices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vmin <- apply(x$vb, 1, min)[1:3]
  vmax <- apply(x$vb, 1, max)[1:3]

  if (!all(is.finite(vmin)) || !all(is.finite(vmax))) {
    stop("Non-finite vertex extents. Is this object only 1D or 2D? Must be 3D!")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Scale evenly in all dimensions to not exceed size of 2x2x2
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  v_scale  <- size / (vmax - vmin)
  v_scale  <- rep(min(v_scale), 3)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Translate centroid to origin
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  v_translate <- -(vmax + vmin) / 2

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Apply the scale and rotation
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- translate_by(x, v_translate)
  x <- scale_by(x, v_scale)

  x
}
