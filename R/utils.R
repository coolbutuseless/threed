
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Interleave the elements of 2 vectors of the same length
#'
#' @param a,b vectors
#'
#' @return interleaved values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
interleave <- function(a, b) {
  stopifnot(length(a) == length(b) && length(a) > 0)
  as.vector(rbind(a, b))
}
