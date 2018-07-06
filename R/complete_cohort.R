as.academicYears <- function(year) {
  year - 1 + 0.33
}

#' Filter an academic dataset to include only complete cohorts, that is 
#' filter out records from students who have not been enrolled long enough to 
#' have graduated
#' 
#' @param firstTerm a date or term object. Filter cohorts who start on this term. If NULL then include all complete cohorts.
#' @return a filtered academic data set
#' @export
complete_cohort <- function(.data, firstTerm = NULL, yearsToGrad = 5) {
  firstTermName <- attr(.data, "first.term")
  currentTermName <- attr(.data, "current.term")
  stopifnot(!is_null(firstTermName) & !is_null(currentTermName))
  
  cohortFirstTerm <- max(.data[[currentTermName]]) - as.academicYears(yearsToGrad)
  if (is.null(firstTerm)) {
    message(paste0("find first term <= ", cohortFirstTerm))
    cohort <- .data[[firstTermName,]] <= cohortFirstTerm
  } else {
    cohort <- .data[[firstTermName,]] == firstTerm
    if (firstTerm > cohortFirstTerm) {
      warning(paste0("The most recent term in the data is ", max(.data[[currentTermName]]), " so filtering for firstTerm == ", firstTerm, " may not include full cohort assuming ", yearsToGrad, " years to graduation."))
    }
  }
  .data[cohort,]
}