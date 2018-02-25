#' academic
#' @export
academic <- function(.data, first.term, current.term) {
  stopifnot(is.data.frame(.data))
  termNames <- c(first.term, current.term)
  .data[termNames] <- lapply(.data[termNames], function(x) as.term(as.character(x)))
  structure(.data, 
            first.term = first.term, 
            current.term = current.term,
            class = c("acad_df", class(.data))
  )
}

#' as.academic
#' @rdname acad_df
#' @param x object to be converted to academic record
#' @return academic data object
#' @export
as.academic <- function(x, ...) UseMethod("as.academic")

#' @rdname acad_df
#' @export
as.academic.default <- function(x, ...) academic(x, ...)

reclass.acad_df <- function(old, new) {
  class(new) <- unique(c(class(old)[[1]], class(new)))
  attr(new, "first.term") <- attr(old, "first.term")
  attr(new, "current.term") <- attr(old, "current.term")
  new
}

mutate.acad_df <- function(.data, ...) reclass(.data, NextMethod())
arrange.acad_df <- function(.data, ...) reclass(.data, NextMethod())