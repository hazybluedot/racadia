
#' @title Calculate number of terms enrolled for each student
#' @param .data a tbl
#' @param idvar ID variable
#' @param termvar Term variable
#' @param drop.unit When drop.unit = FALSE, the default, the time unit column is included in the returned table. Set this value to TRUE to drop this variable.
#' @return An object of the same class as .data containing variables `idvar`, `termvar`, utime, term
#' @export
enrolled_time <- function(.data, ...) UseMethod("enrolled_time")

#' @rdname enrolled_time
#' @export
enrolled_time.data.frame <- function(.data, idvar, termvar, drop.unit = FALSE) {
  idvar <- quo(idvar)
  termvar <- quo(termvar)

  .data <- group_by(.data, !! idvar, !! termvar) %>%
    summarize(utime = first(term_unit(!! termvar))) %>%
    arrange(!! termvar) %>%
    mutate(term = cumsum(utime))
  if (drop.unit) {
    .data <- .data %>% select(-utime)
  }
  .data
}

#' @rdname enrolled_time
#' @export
enrolled_time.data.table <- function(.data, idvar, termvar, drop.unit = FALSE) {
  #dt <- data.table(.data)
  idvar_ <- deparse(substitute(idvar))
  termvar_ <- deparse(substitute(termvar))
  #print(names(.data))
  # Why doesn't this work in here, but it does when it is defined as a function in the global environment?
  setkeyv(.data, termvar_)
  .data[order(termvar_), unique(termvar_), by = idvar_
        ][, utime:=term_unit(V1)
          ][, term:=cumsum(utime), by = idvar_]
}

#' @title Add cumulative terms enrolled count to data table.
#' @export
add_enrolled_time <- function(.data, idvar, termvar) {
  idvar <- enquo(idvar)
  termvar <- enquo(termvar)
 #as.character(rlang::get_expr(idvar))
  left_join(.data, enrolled_time(.data, idvar, termvar), by = idvar)
}
