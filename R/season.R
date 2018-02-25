#' Return the season of an academic term
#' @param x a term object
#' @return character string representing the season of term x. May be one of Fall, Spring, Winter, Summer1, Summer2
#' @export
#' @examples 
#' term1 <- as.term("201709")
#' season(term1)
season <- function(x) {
  # TODO: This is really slow on large vectors right now.
  if(!is.term(x)) {
    stop("Only valid on term objects.")
  }
  seasonMap <- attr(x, "seasonMap")
  seasonNames <- names(seasonMap)
  
  month <- format(as.POSIXlt(x), format = "%m")
  unlist(lapply(month, function(m) seasonNames[which(seasonMap == m)]))
}