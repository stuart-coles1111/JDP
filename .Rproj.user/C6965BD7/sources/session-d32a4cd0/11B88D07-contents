#' Play match
#'
#' Play match
#'
#
#'
#' @returns played match info
#' @examples
#' game_play_l()
#'
#' @export
#'
game_play_l <- function(a1 = c(5, 5, 5), a2 = c(5, 5, 5)){
  p <- strengths_l(a1, a2)[[4]][1]
  w <-  0
  l <- 0
  while(w < 5 & l < 5){
    res <- sample(c(1, 0), 1, F, c(p, 1 - p))
    if(res == 1) w <- w + 1
    else l <- l + 1
  }

  c(a1, a2, p, 1 - p, w, l) %>% as.numeric %>% round(2)
}
