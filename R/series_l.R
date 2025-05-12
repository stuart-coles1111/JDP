#' Truncated linear series
#'
#' Truncated linear map
#'
#' @param x x
#' @param k scale
#
#'
#' @returns series map
#' @examples
#' series_l(1:20, 2)
#'
#' @export
#'

series_l <- function(x, k = 1.5){
  pmin(JDP::l(x,k), JDP::l(8,k))
}
