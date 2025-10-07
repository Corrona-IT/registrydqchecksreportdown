#' printCriticalChecks
#'
#' @param .criticalChecksToPrint An r dataframe with the critical checks to print
printCriticalChecks <- function(.criticalChecksToPrint) {
  for (criticalCheckId in names(.criticalChecksToPrint)) {
    printCriticalCheck(
      .criticalCheckId = criticalCheckId,
      .criticalCheckResults = .criticalChecksToPrint[[criticalCheckId]]
    )
  }
}
