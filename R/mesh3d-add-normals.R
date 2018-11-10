
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add face and vertex normals to an object
#'
#' Currently only 'mesh3d' objects are supported.
#'
#' @param x object
#' @param ... other arguments
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_normals <- function(x, ...) UseMethod("add_normals")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add face and vertex normals for mesh3d objects.
#'
#' This is a modified version of \code{rgl::addNormals()}. The \code{rgl} function
#' only saves the vertex nomarls - but it does actually calculate face normals
#' as an intermediate result.
#'
#' This function is a clone of \code{rgl::addNormals()} where these face normals
#' are assigned to the object rather than just being discarded.
#'
#' There is also some extra logic so that no attempt is made to calculate normals
#' of 'point' and 'line' objects.
#'
#' @param x mesh3d object
#' @param ... ignored
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_normals.mesh3d <- function(x, ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check: Only element_types 3 + 4 can have a normal
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  element_types <- get_element_types(x)
  if (length(intersect(element_types, 3:4)) == 0L) {
    return(x)
  }

  v <- x$vb

  # Make sure v is homogeneous with unit w
  if (nrow(v) == 3L) {
    v <- rbind(v, 1)
  } else {
    v <- t( t(v)/v[4L,] )
  }

  normals <- v*0
  v <- v[1:3,]

  if (length(x$it)) {
    it <- x$it
    face_normals <- matrix(0, ncol = ncol(it), nrow=4)

    for (i in 1:ncol(it)) {
      normal <- vec3_normalize(vec3_crossproduct( v[, it[1, i]] - v[, it[3, i]],
                                                  v[, it[2, i]] - v[, it[1, i]]))
      face_normals[, i] <- c(normal, 1)
      if (!any(is.na(normal)))
        for (j in 1:3) {
          if (sum(normals[1:3, it[j,i]]*normal) < 0)
            normals[, it[j,i]] <- normals[, it[j,i]] + c(-normal, 1)
          else
            normals[, it[j,i]] <- normals[, it[j,i]] + c(normal, 1)
        }
    }

    x$triangle_normals <- face_normals
  }

  if (length(x$ib)) {
    it <- x$ib
    face_normals <- matrix(0, ncol = ncol(it), nrow=4)

    for (i in 1:ncol(it)) {
      normal <- vec3_normalize(vec3_crossproduct( v[, it[1, i]] - v[, it[4, i]],
                                                  v[, it[2, i]] - v[, it[1, i]]))
      face_normals[, i] <- c(normal, 1)
      if (!any(is.na(normal)))
        for (j in 1:4) {
          if (sum(normals[1:3, it[j,i]]*normal) < 0)
            normals[, it[j,i]] <- normals[, it[j,i]] + c(-normal, 1)
          else
            normals[, it[j,i]] <- normals[, it[j,i]] + c(normal, 1)
        }
    }

    x$quad_normals <- face_normals
  }

  # homogenise
  normals <- t( t(normals)/normals[4,] )

  # Now make into unit normals
  lengths <- sqrt(colSums(normals[1:3,]^2))
  normals[1:3, ] <- t( t(normals[1:3,]) / lengths)

  x$normals      <- normals
  x$face_normals <- face_normals

  x
}


