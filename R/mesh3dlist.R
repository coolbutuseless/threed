


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a list of mesh3d objects
#'
#' @param ... mesh3d objects
#'
#' @return an R object of type 'mesh3dlist'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mesh3dlist <- function(...) {
  l <- list(...)
  class(l) <- 'mesh3dlist'
  l
}