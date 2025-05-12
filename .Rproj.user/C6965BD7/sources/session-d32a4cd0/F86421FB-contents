#' Simulate multiple matches
#'
#' Simulate multiple matches
#'
#' @param n number of matches
#' @param a1_m matrix of allocations for player 1
#' @param a2_m matrix of allocations for player 2
#' @param seed seed
#'
#' @returns simulated match info
#' @examples
#' mgame_play_l(16, dummy_data[, 2:4], dummy_data[16:1, 2:4], seed = 15)
#'
#' @export
#'
#'
mgame_play_l <- function(n, a1_m, a2_m, seed = NULL){
  if(!is.null(seed)) set.seed(seed)
  df <- lapply(1:n, function(x) game_play_l(a1_m[x,], a2_m[x,]))
  df <- do.call(rbind, df) %>% as.data.frame
  colnames(df) <- c('sarilhos_1', 'formos_1', 'series_1', 'sarilhos_2', 'formos_2', 'series_2', 'p_1', 'p_2', 'w_1', 'w_2')
  df
}
