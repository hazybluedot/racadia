#' reclase
reclass <- function(x, result) {
  UseMethod('reclass')
}

reclass.default <- function(x, result) {
  class(result) <- unique(c(class(x)[[1]], class(result)))
  result
}