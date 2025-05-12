#' Strengths
#'
#'Strengths
#'
#' @param p1 team 1 allocation
#' @param p2 team 2 allocation
#
#'
#' @returns strength info
#' @examples
#' strengths_l(c(5, 5, 5), c(2, 9, 4))
#'
#' @export
#'

strengths_l <- function(p1, p2){
  p1 <- as.numeric(p1)
  p2 <- as.numeric(p2)

  s1_1 <- sarilhos_l(p1[1])
  s2_1 <- formos_l(p1[2])
  s3_1 <- series_l(p1[3])


  s1_2 <- sarilhos_l(p2[1])
  s2_2 <- formos_l(p2[2])
  s3_2 <- series_l(p2[3])


  c_1 <- (s1_1 + s2_1 + s3_1)
  c_2 <- (s1_2 + s2_2 + s3_2)
  list(
    a_1 = c(s1_1, s2_1, s3_1),
    a_2 = c(s1_2, s2_2, s3_2),
    s = c(c_1, c_2),
    p = c(exp(c_1 - c_2)/(1 + exp(c_1 - c_2)), 1/(1 + exp(c_1 - c_2)))
  )
}
