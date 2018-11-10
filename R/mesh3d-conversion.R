globalVariables(c('element_type', 'element_id', 'fcz', 'vorder', 'y', 'z', 'vertex'))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add zordering
#'
#' @param obj_df an object data.frame
#'
#' @return data.frame with 'zorder' column
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_zorder <- function(obj_df) {
  missing_cols <- setdiff(c('zorder_var', 'element_id', 'vorder'), colnames(obj_df))
  if (length(missing_cols) > 0) {
    stop("data.frame is missing columns: ", deparse(missing_cols))
  }


  stopifnot(!'object_id' %in% colnames(obj_df))

  tmp        <- obj_df
  tmp        <- with(tmp, tmp[order(zorder_var, element_id, vorder),])
  tmp$zorder <- with(tmp, factor(element_id, labels=seq_along(unique(element_id)), levels = unique(element_id)))
  tmp        <- with(tmp, tmp[order(element_id, vorder),])

  tmp
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add zordering to an object with multiple object_ids
#'
#' @param obj_df an object data.frame
#'
#' @return data.frame with 'zorder' column
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_zorder_multiple_objects <- function(obj_df) {
  missing_cols <- setdiff(c('zorder_var', 'element_id', 'vorder', 'object_id'), colnames(obj_df))
  if (length(missing_cols) > 0L) {
    stop("data.frame is missing columns: ", deparse(missing_cols))
  }

  obj_df$uber_id <- paste(obj_df$object_id, obj_df$element_id)

  tmp        <- obj_df
  tmp        <- with(tmp, tmp[order(zorder_var, uber_id, vorder),])
  tmp$zorder <- with(tmp, factor(uber_id, labels=seq_along(unique(uber_id)), levels = unique(uber_id)))
  tmp        <- with(tmp, tmp[order(object_id, element_id, vorder),])

  tmp
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert all objects in mesh3d list into a single data.frame
#'
#' An \code{object_id} will also be included in the output data.frame
#'
#' @inheritParams as.data.frame.mesh3d
#' @param x mesh3dlist object
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.data.frame.mesh3dlist <- function(x, ...) {
  if (length(x) == 0L) {
    stop("Empty mesh3dlist")
  }

  res <- lapply(x, as.data.frame)
  res <- lapply(seq_along(res), function(x) {res[[x]]$object_id <- x; res[[x]]})
  res <- do.call(rbind, res)

  add_zorder_multiple_objects(res)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert mesh3d object to a data.frame representation
#'
#' @param x mesh3d object
#' @param ... ignored
#'
#' @return data.frame representation
#' \itemize{
#' \item{\code{element_id}} - {element index}
#' \item{\code{element_type}} - {an integer from 1 to 4 indicating whether the element is
#' a point, line, triangle or quad}
#' \item{\code{vorder}} - {vertex ordering within element}
#' \item{\code{x,y,z}} - {coorindates of vertex}
#' \item{\code{vertex}} - {global index of this vertex}
#' \item{\code{vnx,vny,vnz}} - {unit vector in direction of vertex normal}
#' \item{\code{fnx,fny,fnz}} - {unit vector in direction of face normal}
#' \item{\code{fcx,fcy,fcz}} - {coordinates of centroid of element}
#' \item{\code{zorder}} - {\code{zorder} is used to control ggplot draw order.
#'                       For polygons it is based upon \code{fcz}. For lines it is
#'                       the smaller of the \code{z} coordinates. For points, it is the
#'                       \code{z} coordinate of the point}
#' \item{\code{zorder_var}} - { The value used to determine zorder }
#' \item{\code{hidden}} - by assuming the camera faces along the negative z-axis,
#' any triangle or quad face which has a face normal which also points along the
#' negative z-axis faces away from the camera and is hidden. Boolean value.
#'
#' }
#'
#' @importFrom stats aggregate setNames
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.data.frame.mesh3d <- function(x, ...) {

  obj <- x

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # "Concretize" the transform of the object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  obj <- actualize_transformation(obj)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate normals if feasible
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  obj <- add_normals(obj)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # vertices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  all_vertices <- t(obj$vb)
  all_vertices <- all_vertices[,1:3]/all_vertices[,4]
  all_vertices <- cbind(all_vertices, seq(nrow(all_vertices)))
  colnames(all_vertices) <- c('x', 'y', 'z', 'vertex')

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # What element types are present in the object. Can be multiple!
  # e.g. cuboctahedron is both quads and tris
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  element_types <- get_element_types(obj)


  obj_mats <- lapply(element_types, function(x) {get_matrix_for_element_type(obj, element_type = x, all_vertices = all_vertices)})
  obj_mat  <- do.call(rbind, obj_mats)
  obj_df   <- as.data.frame(obj_mat)

  obj_df <- transform(
    obj_df,
    element_type = as.integer(element_type),
    vorder       = as.integer(vorder),
    vertex       = as.integer(vertex),
    element_id   = as.integer(as.factor(interaction(element_id, element_type)))
  )


  obj_df <- add_zorder(obj_df)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Desireded column ordering
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  column_ordering <- c('element_id', 'element_type', 'vorder', 'x', 'y', 'z', 'vertex',
                       'vnx', 'vny', 'vnz',
                       'fnx', 'fny', 'fnz',
                       'fcx', 'fcy', 'fcz', 'zorder')
  column_ordering <- intersect(column_ordering, colnames(obj_df))
  obj_df <- obj_df[, c(column_ordering, setdiff(colnames(obj_df), column_ordering))]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If the face normal points towards the negative z axis, the it's hidden
  # (in the default perspectvve proj and orthographic proj)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if ('fnz' %in% colnames(obj_df)) {
    obj_df$hidden <- obj_df$fnz < 0
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # add in any properties by element_id
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(obj$properties)) {
    cols_to_add <- setdiff(colnames(obj$properties), colnames(obj_df))

    if (!'element_id' %in% names(obj$properties)) {
      obj$properties$element_id <- seq(nrow(obj$properties))
    }
    cols_to_add <- c('element_id', cols_to_add)

    if (length(cols_to_add) > 0L) {
      obj_df <- merge(obj_df, obj$properties[,cols_to_add, drop=FALSE], all.x = TRUE)
    }
  }


  obj_df
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert the given object to a mesh3d object
#'
#' @param x object
#' @param ... arguments to particular methods
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.mesh3d <- function(x, ...) {
  UseMethod("as.mesh3d")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert data.frame into mesh3d object
#'
#' @param x data.frame with columns: x, y, z, element_id  Optional columns include:
#'          c('vnx', 'vny', 'vnz') for vertex normals.  c('fnx', 'fny', 'fnz')
#'          for face normals.
#' @param ... ignored
#'
#' @return mesh3d object
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.mesh3d.data.frame <- function(x, ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # At a minimum, the data.frame must have these columns
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  missing_cols <- setdiff(c('x', 'y', 'z', 'element_type', 'element_id', 'vorder'), colnames(x))
  if (length(missing_cols) > 0) {
    stop("data.frame is missing columns: ", deparse(missing_cols))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Keep track of all unused columns in the conversion.
  # These will be added as a 'properties' item
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  property_columns <- setdiff(colnames(x), c('x', 'y', 'z', 'element_id', 'vorder'))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initialise the mesh3d object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  obj <- list(material=list(), texcoords = NULL, normals=NULL)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add in vertices.
  # If numeric 'vertex' found, then use this for getting distinct vertices.
  # Otherwise, calculate the distince vertices and number them
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if ('vertex' %in% colnames(x) && is.numeric(x$vertex)) {
    # use the 'vertex' to include every vertex only once
    vb_df  <- x[!duplicated(x$vertex),]
    vb_df  <- vb_df[with(vb_df, order(vertex)) ,]
  } else {
    vb_df  <- x[,c('x', 'y', 'z')]
    vb_df  <- vb_df[!duplicated(vb_df),]
    vb_df$vertex <- seq(nrow(vb_df))
    x <- merge(vb_df, x, by=c('x', 'y', 'z'), sort=FALSE)
    x <- x[with(x, order(element_id, vorder)), ]
  }

  vb     <- unname(as.matrix(vb_df[,c('x', 'y', 'z')]))
  vb     <- cbind(vb, 1)
  obj$vb <- unname(t(vb))

  property_columns <- setdiff(property_columns, 'vertex')

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add in the vertex indices for each element
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (vertices_per_element in 1:4) {
    xs <- subset(x, element_type == vertices_per_element)
    if (nrow(xs) == 0) next
    if (vertices_per_element == 1) {
      obj$primitivetype <- 'point'
      ip                <- matrix(xs$vertex, nrow=1, byrow=TRUE)
      obj$ip            <- ip
    } else if (vertices_per_element == 2) {
      obj$primitivetype <- 'line'
      il                <- matrix(xs$vertex, nrow=2)
      obj$il            <- il
    } else if (vertices_per_element == 3) {
      obj$primitivetype <- 'triangle'
      obj$it            <- matrix(xs$vertex, nrow=3)
    } else {
      obj$primitivetype <- 'quad'
      obj$ib            <- matrix(xs$vertex, nrow=4)
    }
  }


  # Don't save any meta info
  property_columns <- setdiff(property_columns, c('vnx', 'vny', 'vnz'))
  property_columns <- setdiff(property_columns, c('fnx', 'fny', 'fnz'))
  property_columns <- setdiff(property_columns, c('fcx', 'fcy', 'fcz'))
  property_columns <- setdiff(property_columns, c('zorder', 'zorder_var', 'hidden'))
  property_columns <- setdiff(property_columns, c('element_type'))

  if (length(property_columns) > 0) {
    obj$properties <- x[ , property_columns, drop = FALSE]
  } else {
    obj$properties <- NULL
  }


  class(obj) <- c('mesh3d', 'shape3d')
  obj
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Fortify a mesh3d object into a data.frame for compatibility with ggplot2
#'
#' @param x object
#' @param ... unused
#'
#' @return data.frame
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fortify.mesh3d <- as.data.frame.mesh3d








