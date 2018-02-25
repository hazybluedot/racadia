#' semester
#' @param term1 a term object
#' @param term2 another term object
#' @return an integer value of the number of semesters term2 is since term1
#' @export
#' @examples 
#' term1 <- as.term("201709")
#' term2 <- as.term("201801")
#' semester(term2, term1)
semester <- function(term2, term1) {
  stopifnot(is.term(term2) & is.term(term1))
  spy <- attr(term2, "semestersPerYear")
  spy2 <- attr(term1, "semestersPerYear")
  .args <- c(term1, term2)
  stopifnot(spy == spy2)
  if (term2 < term1) {
    .args <- rev(.args)
  }
  as.integer(ceiling((.args[2] - .args[1])*spy+1))
}
