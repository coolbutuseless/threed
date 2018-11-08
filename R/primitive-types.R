


# all_vertices_per_element <- c(point=   1, line=   2, triangle=   3, quad=   4)


all_data_names    <- c('ip'   , 'il'  , 'it'      , 'ib'  )
all_element_names <- c('point', 'line', 'triangle', 'quad')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Determine element types present in object.
#'
#' @param x object (mesh3d object or data.frame of elements)
#'
#' @return Integer vector with values in range 1:4 representing the primitive types
#'         c('point', 'line', 'triangle' or 'quad')
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_element_types <- function(x) {
  UseMethod("get_element_types")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname get_element_types
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_element_types.data.frame <- function(x) {
  stopifnot('element_id' %in% colnames(x))

  vertices_per_element <- (aggregate(x$element_id, list(element_id = x$element_id), length))$x
  vertices_per_element <- unique(vertices_per_element)

  stopifnot(all(vertices_per_element %in% 1:4))

  vertices_per_element
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname get_element_types
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_element_types.mesh3d <- function(x) {
  # check which matrices are in object
  vertices_per_element <- which(c('ip', 'il', 'it', 'ib') %in% names(x))

  stopifnot(all(vertices_per_element %in% 1:4))

  vertices_per_element
}