



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a bounding box for the given object at
#' the correct scale and location
#'
#' @param obj 3d object
#'
#' @return bounding box (mesh3d cube object) for the given 3d object
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_bbox_obj <- function(obj) {

  vertices <- t(obj$vb)
  vertices <- vertices[,1:3]/vertices[,4]

  vmax <- apply(vertices, 2, max)
  vmin <- apply(vertices, 2, min)

  scale_v     <- (vmax - vmin)/2
  translate_v <- (vmax + vmin)/2

  cube <- scale_by(cube, scale_v)
  cube <- translate_by(cube, translate_v)
  cube <- actualize_transformation(cube)

  cube
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print mesh3d object
#'
#' @param x mesh3d object
#' @param max_cols maximum number of columns to print for each matrix. default: 10
#' @param ... ignored
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.mesh3d <- function(x, max_cols=10, ...) {

  message("print.mesh3d with max_cols=", max_cols)
  pmat <- function(mat, max_cols = 10) {
    print(mat[,seq(min(max_cols, ncol(mat))), drop=FALSE])
  }


  mat_names <- c('vb', 'ib', 'it', 'normals', 'face_normals')
  for (mat_name in mat_names) {
    if (length(x[[mat_name]])) {
      cat("$", mat_name, "  -  ", deparse(dim(x[[mat_name]])),  "\n", sep="")
      pmat(x[[mat_name]], max_cols = max_cols)
      cat("\n")
    }
  }

  x[mat_names] <- NULL

  if (!is.null(x$properties)) {
    cat("$properties", "  -  ", deparse(dim(x[['properties']])),  "\n", sep="")
    print(x$properties[seq(min(max_cols, nrow(x$properties))) , , drop = FALSE])
    cat("\n")
    x['properties'] <- NULL
  }

  print.default(x)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get a list of information about a 3d object
#'
#' @param obj object
#' @param ... arguments passed to particular methods
#'
#' @return list
#' \itemize{
#' \item{element_type} - {Type of element. point, line, triangle or quad}
#' \item{vertices} - {Number of vertices}
#' \item{elements} - {Number of elements}
#' \item{bytes} - {Size of object in bytes}
#' }
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_object_info <- function(obj, ...) {
  UseMethod("get_object_info")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname get_object_info
#'
#' @importFrom utils object.size
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_object_info.mesh3d <- function(obj, ...) {

  element_types <- get_element_types(obj)
  data_names    <- all_data_names[element_types]

  element_counts        <- vapply(data_names, function(x) {ncol(obj[[x]])}, integer(1))
  names(element_counts) <- all_element_names[element_types]

  list(
    vertices = ncol(obj$vb),
    elements = element_counts,
    bytes    = as.integer(object.size(obj))
  )

}




