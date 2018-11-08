


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get a matrix of data to represent the given element type
#'
#' `mesh3d` objects contain a matrix of vertices and matrix of vertex indicies
#' to describe each element.
#'
#' This function maps the vertex indices to the actual vertex coordinates and
#' returns a matrix of information to represent each element of the requested
#' type.
#'
#' @param obj mesh3d object
#' @param element_type single element type to extract. specify an integer 1:4
#' @param all_vertices normalised matrix of all vertices. If NULL, then will be calculated within this call
#'
#'
#' @return numeric matrix representation of the given element type
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
#'
#' }
#'
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_matrix_for_element_type <- function(obj, element_type, all_vertices = NULL) {

  data_name            <- all_data_names[element_type]
  vertices_per_element <- element_type

  if (is.null(obj[[data_name]])) {
    return(NULL)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The vertex indicies for tihs element
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vertex_indices       <- as.vector(obj[[data_name]])
  n_elements           <- ncol(obj[[data_name]])
  n_vertices           <- length(vertex_indices)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # All vertices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(all_vertices)) {
    all_vertices <- t(obj$vb)
    all_vertices <- all_vertices[,1:3]/all_vertices[,4]
    all_vertices <- cbind(all_vertices, seq(nrow(all_vertices)))
    colnames(all_vertices) <- c('x', 'y', 'z', 'vertex')
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get the actual vertex coords based upon the vertex_indicies for the elements
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vertices <- all_vertices[vertex_indices,]
  vertices <- cbind(vertices, vorder = rep(seq(vertices_per_element), n_elements))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set to NULL by default
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vertex_normals <- NULL
  face_normals   <- NULL


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # element_id ordering
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  element_id <- rep(seq(n_elements), each = vertices_per_element)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Normals
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (vertices_per_element > 2) {
    obj <- add_normals(obj)

    face_normals_name <- ifelse(vertices_per_element == 3L, 'triangle_normals', 'quad_normals')

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # vertex normals. ensure they are unit vectors
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(obj$normals)) {
      vertex_normals <- t(obj$normals)[vertex_indices, 1:3]
      vertex_normals <- t(apply(vertex_normals, 1, vec3_normalize))
      colnames(vertex_normals) <- c('vnx', 'vny', 'vnz')
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # face normals
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(obj[[face_normals_name]])) {
      face_normals <- t(obj[[face_normals_name]])[,1:3]
      colnames(face_normals) <- c('fnx', 'fny', 'fnz')
      face_normals <- face_normals[rep(seq(n_elements), each=vertices_per_element),]
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Combine all data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  element_mat <- cbind(vertices, vertex_normals, face_normals, element_id)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the centroid for each element_id
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  face_centroid <- aggregate(element_mat[,c('x', 'y', 'z')], by=list(element_id = element_mat[,'element_id']), mean)
  face_centroid <- setNames(face_centroid, c('element_id', 'fcx', 'fcy', 'fcz'))
  element_mat        <- merge(element_mat, face_centroid, all.x = TRUE)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the zorder for each element_id based upon the z coordinates
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (vertices_per_element == 1) {
    # for points, pick the z coordinate
    element_mat[,'zorder_var'] <- element_mat[,'z']
  } else if (vertices_per_element == 2) {
    # For lines, pick the most negative z coord of each 'element_id'
    min_z <- aggregate(element_mat[,'z'], list(element_id = element_mat[,'element_id']), max)$x
    element_mat[,'zorder_var'] <- rep(min_z, each=2)
  } else {
    # for tris and quads, use the face centroid
    element_mat[,'zorder_var'] <- element_mat[,'fcz']
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # add dummy normal values if actual normals not found or not possible
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!'vnx' %in% colnames(element_mat)) { element_mat[, c('vnx', 'vny', 'vnz')] <- NA_real_ }
  if (!'fnx' %in% colnames(element_mat)) { element_mat[, c('fnx', 'fny', 'fnz')] <- NA_real_ }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Element type
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  element_mat[,'element_type'] <- vertices_per_element


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Desireded column ordering
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  column_ordering <- c('element_type', 'element_id', 'vorder', 'x', 'y', 'z', 'vertex',
                       'vnx', 'vny', 'vnz',
                       'fnx', 'fny', 'fnz',
                       'fcx', 'fcy', 'fcz', 'zorder', 'zorder_var')
  column_ordering <- intersect(column_ordering, colnames(element_mat))
  element_mat <- element_mat[, c(column_ordering, setdiff(colnames(element_mat), column_ordering))]


  element_mat
}