#' Simulate multiple matches
#'
#' Simulate multiple matches
#'
#' @param n number of matches
#' @param seed seed
#'
#' @returns simulated match info
#' @examples
#' mgame_sim_l(100, seed = 15)
#'
#' @export
#'
#'
mgame_sim_l <- function(n, seed = NULL){
  if(!is.null(seed)) set.seed(seed)
  df <- lapply(1:n,function(x) game_sim_l())
  df <- do.call(rbind, df) %>% as.data.frame
  colnames(df) <- c('sarilhos_1', 'formos_1', 'series_1', 'sarilhos_2', 'formos_2', 'series_2', 'p_1', 'p_2', 'w_1', 'w_2')
  df
}
