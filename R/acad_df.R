#' academic
#' @export
academic <- function(.data, ...) {
  stopifnot(is.data.frame(.data))
  termNames <- unlist(list(...)) #c(first.term, current.term)
  .data[termNames] <- lapply(.data[termNames], function(x) as_term(as.character(x)))
  structure(.data,
            first.term = termNames[1],
            current.term = termNames[2],
            class = c("acad_df", class(.data))
  )
}

#' as.academic
#' @rdname acad_df
#' @param x object to be converted to academic record
#' @return academic data object
#' @export
as_academic <- function(x, ...) UseMethod("as_academic")

#' @rdname acad_df
#' @export
as_academic.default <- function(x, ...) academic(x, ...)

reclass.acad_df <- function(old, new) {
  class(new) <- unique(c(class(old)[[1]], class(new)))
  attr(new, "first.term") <- attr(old, "first.term")
  attr(new, "current.term") <- attr(old, "current.term")
  new
}

#' @importFrom dplyr mutate
#' @export
mutate.acad_df <- function(.data, ...) reclass(.data, NextMethod())

#' @importFrom dplyr arrange
#' @export
arrange.acad_df <- function(.data, ...) reclass(.data, NextMethod())

#' @importFrom dplyr filter
#' @export
filter.acad_df <- function(.data, ...) reclass(.data, NextMethod())
