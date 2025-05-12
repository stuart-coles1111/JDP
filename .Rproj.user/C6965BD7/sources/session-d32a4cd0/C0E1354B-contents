#' Simulate match
#'
#' Simulate match
#'
#
#'
#' @returns simulated match info
#' @examples
#' game_sim_l()
#'
#' @export
#'
game_sim_l <- function(){

  a1 <- 11
  a2 <- 11
  while(max(a1) > 10 | max(a2) > 10){
    a1 <- gtools::rdirichlet(1, c(1, 1, 1)) * 15 + runif(3, -.5, .5)
    a2 <- gtools::rdirichlet(1,c(1, 1, 1)) * 15 + runif(3, -.5, .5)
  }
  s <- strengths_l(a1, a2)[[4]][1]
  w <-  0
  l <- 0
  while(w < 5 & l < 5){
    res <- sample(c(1, 0), 1, F, c(s, 1 - s))
    if(res == 1) w <- w + 1
    else l <- l + 1
  }
  c(a1, a2, s, 1 - s, w, l) %>% round(2)
}
