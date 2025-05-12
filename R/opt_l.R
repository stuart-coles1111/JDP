#' Optimiser
#'
#' Optimiser
#'
#' @param x x
#' @param k scale
#
#'
#' @returns optimal values
#' @examples
#' opt_l()
#'
#' @export
#'

opt_l <- function(q, init = c(5, 5)){
  res <- optim(init, ch_l, control = list(maxit=5000), method="Nelder-Mead")
  list(res, c(res$par, 15-sum(res$par)) %>% round(2))
}
