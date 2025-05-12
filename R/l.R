#' Truncated linear map
#'
#' Truncated linear map
#'
#' @param x x
#' @param k scale
#
#'
#' @returns truncated kx
#' @examples
#' l(1:20, 2)
#'
#' @export
#'

l <- function(x, k){
  if(any(x<0 | x > 10))return(-100)
  k*x
}
