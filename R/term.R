#' term
#' @title An Academic Term Class
#' @param x string interpreted
#' @export
#' @examples
#' term(x)
#'
#' is.term(x)
term <- function(x,
                 semestersPerYear = 2,
                 seasonMap = c(Fall = "09", Spring = "01", Winter = "12", Summer1 = "06", Summer2 = "07"),
                 format = "%Y%m") {
  .x <- zoo::as.yearmon(x, format = format)
  structure(
    .x,
    format = format,
    semestersPerYear = semestersPerYear,
    seasonMap = seasonMap,
    class = c("term", class(.x))
  )
}

#' @rdname term
#' @export
is.term <- function(x, ...) inherits(x, "term")

#' as.term
#' @title coerce string and numeric objects to a term
#' @param x a string or numeric value of an academic term
#' @return a term object
#' @export
#' @examples
#' as.term("201709", "%Y%m")
as.term <- function(x, ...) UseMethod("as.term")

#' @describeIn term coerce character string to term
#' @export
as.term.default <- function(x, ...) term(as.character(x), format = "%Y%m")

#' @describeIn term coerce integer to term
#' @export
as.term.integer <- function(x, ...) as.term(as.character(x))

#' @describeIn term coerce Date object to term
#' @export
as.term.Date <- function(x, ...) as.term(format(x, "%Y%m"))

#' @describeIn term coerce POSIXct object to term using only month and year
#' @export
as.term.POSIXct <- function(x, ...) as.term(format(x, "%Y%m"))

## other methods for class term
#' @export
format.term <- function(x, format = "%Y%m", ...)  {
  paste0(season(x), " ", format(as.POSIXlt(x), format = "%Y"))
}

#' @export
print.term <- function(x, ...) {
  #print(paste0(season(x), " ", substr(format(x), 1,4)), ...)
  cat(format(x, ...), "\n")
  invisible(x)
}
