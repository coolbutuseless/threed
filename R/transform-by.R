
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Transform the vertex coordinates by the transformation matrix
#'
#' @param x matrix, mesh3d object, mesh3dlist object, or numeric vector of length 3 or 4 (homogenous coordinates)
#' @param transform_matrix 4x4 transformation matrix
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
transform_by <- function(x, transform_matrix) {
  UseMethod("transform_by")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname transform_by
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
transform_by.default <- function(x, transform_matrix) {
  stop("transform_by.default called on object with class: ", class(x))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname transform_by
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
transform_by.matrix <- function(x, transform_matrix) {
  if (ncol(x) == 4) {
    res <- x %*% t(transform_matrix)
    res <- res/res[,4]
  } else if (nrow(x) == 4) {
    res <- transform_matrix %*% x
    res <- t( t(res)/res[4,] )
  } else {
    stop("Non-sane dimensions: ", deparse(dim(x)))
  }

  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname transform_by
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
transform_by.mesh3d <- function(x, transform_matrix) {
  if (is.null(x$transform_matrix)) {
    x$transform_matrix <- transform_matrix
  } else {
    x$transform_matrix <- transform_matrix %*% x$transform_matrix
  }

  # In general, if you transform an object you should transform the normals
  # For my purposes, I'm just going to blank them out, and they'll have to
  # be recalcualted if needed.
  x$face_normals <- NULL
  x$normals      <- NULL

  x
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname transform_by
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
transform_by.numeric <- function(x, transform_matrix) {
  if (length(x) == 3) {
    x   <- c(x, 1)
    res <- transform_matrix %*% x
    res[-4]
  } else if (length(x) == 4) {
    as.vector(transform_matrix  %*% x)
  } else {
    stop("Bad length: ", length(x))
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname transform_by
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
transform_by.data.frame <- function(x, transform_matrix) {
  stop("transform_by.data.frame: not doing this directly anymore. Maybe replace with conversion to mesh3d object, then transform then conversion back to data.frame?")
  if (all(c('x', 'y', 'z', 'w') %in% colnames(x))) {
    cols <- c('x', 'y', 'z', 'w')
    mat <- as.matrix(x[, cols])
  } else if (all(c('x', 'y', 'z') %in% colnames(x))) {
    cols <- c('x', 'y', 'z')
    mat <- as.matrix(x[, cols])
    mat <- cbind(mat, 1)
  } else {
    stop("data.frame must have columns 'x', 'y' and 'z' (and optionally 'w')")
  }

  res <- transform_by.matrix(mat, transform_matrix)
  for (i in seq_along(cols)) {
    x[[cols[i]]] <- res[,i]
  }

  x
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname transform_by
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
transform_by.mesh3dlist <- function(x, transform_matrix) {
  l <- lapply(x, transform_by, transform_matrix = transform_matrix)
  class(l) <- 'mesh3dlist'
  l
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname transform_by
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
transform_by_inverse <- function(x, transform_matrix) {
  transform_by(x, invert_matrix(transform_matrix))
}
