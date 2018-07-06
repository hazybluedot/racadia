#' @rdname term
#' @param x numeric (interpreted as 4 digit year concatenated with 2 digit month, e.g. 201209 for September 2012)
#' @export
new_term <- function(x, semesters, units, subclass = NULL) {
  stopifnot(is.numeric(x))
  structure(
    as.integer(x),
    semesters = semesters,
    units = units,
    class = c(subclass, "term")
  )
}

#' @rdname term
#' @export
validate_term <- function(x) {
  values <- unclass(x)
  semesters = attr(x, "semesters")
  units = attr(x, "units")

  if(!all(nchar(values[!is.na(values)]) == 6)) {
    stop(
      "All `x` values must be 6 digits long comprised of a 4 digit year and 2 digit month, e.g. 201209",
      call. = FALSE
    )
  }

  if (length(semesters) != length(units)) {
    stop("Length of `semesters` must match length of `units`.",
         call. = FALSE)
  }

  if (!all(!is.na(term_season(x)))) {
    warning("Unmatched months in `seasons` will be reported as NA.", call. = FALSE)
  }
  x
}

#' term
#' @title An Academic Term Class
#' @param x string interpreted
#' @export
#' @examples
#' term(x)
#'
#' is.term(x)
term <- function(x,
                 semesters = c("Spring" = 1, "Fall" = 9, "Summer I" = 6, "Summer II" = 7, "Winter" = 12),
                 units = c(1, 1, .33, .33, .33)) {
    validate_term(new_term(x, semesters, units))
}

#' @export
term_season <- function(x, ...) {
  month <- cycle(x)
  semesters <- attr(x, "semesters")
  names(semesters)[match(month, semesters)]
}

#' @export
term_unit <- function(x, ...) {
  month <- cycle(x)
  semesters <- attr(x, "semesters")
  attr(x, "units")[match(month, semesters)]
}

#' @rdname term
#' @export
is_term <- function(x, ...) inherits(x, "term")

#' as.term
#' @title coerce string and numeric objects to a term
#' @param x a string or numeric value of an academic term
#' @return a term object
#' @export
#' @examples
#' as.term("201709", "%Y%m")
as_term <- function(x, ...) UseMethod("as_term")

#' @rdname term
#' @export
as_term.default <- function(x, ...) term(as.numeric(x), ...)

#' @export
as_term.term <- function(x, ...) x

#' @rdname term
#' @export
as_term.character <- function(x, format = "%Y%m", ...) {
  date_pattern <- "^[1-9][0-9]{5}$"
  season_pattern <- "^((?:[Ss]pring|[Ss]ummer|[Ff]all|[Ww]inter])(?:\\s+[1-9]+|I+)?)\\s+([0-9]{4})$"

  smatch <- stringr::str_match(x, season_pattern)
  if (!all(is.na(smatch))) {
    stop(x, " parsing season pattern is not yet implemented.")
  }

  if (is.character(format)) {
    if (format == "%Y%m") {
      return (term(as.integer(x)))
    } else {
      stop("unhandled formate")
    }
    return(term(x))
  }

}

#' @rdname term
#' @export
as_term.integer <- function(x, ...) term(x, ...)

# #' @rdname term
# #' @export
as_term.yearmon <- function(x, ...) as_term(as.integer(format(x, "%Y%m")), ...)

#' @rdname term
#' @export
as_term.Date <- function(x, ...) as_term(as.integer(format(x, "%Y%m")), ...)

#' @rdname term
#' @export
as_term.POSIXct <- function(x, ...) as.term(as.integer(format(x, "%Y%m")), ...)

reclass.term <- function(old, new) {
  class(new) <- unique(c(class(old)[[1]], class(new)))
  attr(new, "semesters") <- attr(old, "semesters")
  attr(new, "units") <- attr(old, "units")
  new
}

# Generic methods to extend the 'c' and '['
#' @rdname term
#' @export
c.term <- function(...)  {
  #message(paste0('c.term with ', paste(list(...), collapse = ", ")))
  as_term(do.call("c", lapply(list(...), as.integer)))
}


#' @rdname term
#' @export
cycle.term <- function(x, ...) as.integer(x) %% 100

#' @rdname term
#' @export
rep.term <- function(x, ...) as_term(rep(x, ...))

#' @rdname term
#' @export
`[.term` <- function(x, ..., drop = TRUE) reclass(x, NextMethod("["))

#' @rdname term
#' @export
`[[.term` <- function(x, ..., drop = TRUE) reclass(x, NextMethod("[["))

#`[.myclass` <- function (x, i)  {
#  y <- unclass(x)[i]
#  ns <- attr(x, "n")[i]
#  class(y) <- "myclass"
#  attr(y, "n") <- ns
#  return (y)
#}

#' @export
Ops.term <- function(e1, e2) {
  e1 <- as.numeric(as_term(e1))
  e2 <- as.numeric(as_term(e2))
  rval <- NextMethod(.Generic)
  if(is.numeric(rval)) rval <- term(rval)
  return(rval)
}

## other methods for class term
#' @export
format.term <- function(x, format = "%Y%m", ...)  {
  paste0(term_season(x), " ", unclass(x) %/% 100)
}

#' @export
print.term <- function(x, ...) {
  #print(paste0(season(x), " ", substr(format(x), 1,4)), ...)
  print(format(x), ...)
  invisible(x)
}

is.numeric.term <- function(x) FALSE

#' @export
range.term <- function(..., na.rm = FALSE) {
  as_term(range.default(..., na.rm = na.rm))
}

#' @export
unique.term <- function(x, incomparables = FALSE, ...) {
  as_term(unique.default(x, incomparables = incomparables, ...))
}
