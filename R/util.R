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
  stopifnot(is_term(term2) & is_term(term1))
  spy <- attr(term2, "semestersPerYear")
  spy2 <- attr(term1, "semestersPerYear")
  stopifnot(!is.null(spy) & spy == spy2)
  # this method of calculating number of semesters between two terms uses differences in time between the two terms.
  # Unfortunately, getting the number of semesters from a time difference is not very straight forward because of the relatively
  # long summer break relative to winter. Also, this would need to be tested for trimesters and quarters.
  # An alternative approach might be to generate a factor of semesters starting from term1 up to term2 by incrementing years and seasons, then
  # get the difference between the two terms as factors.

  # this feels like a bit of a hack, but when the start semester is spring instead of fall
  # we need to subtract off the equivelent of the summer months to make the resulting calculation
  # come out correct.

  adjust <- function(t2, t1) {
    ifelse(season(t2) == "Fall" & season(t1) == "Spring", 0.333, 0)
  }
  .semester <- ifelse(term2 >= term1, term2 - term1, term1 - term2)
  #adj <- ifelse(term2 >= term1, adjust(term2, term1), adjust(term1, term2))
  adj <- 0
  as.integer(ceiling(.semester)*2+1)
}

