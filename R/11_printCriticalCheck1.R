#' printCriticalCheck
#'
#' @param .criticalCheckId The id of the check to be printed (i.e. criticalCheck1 - criticalCheck9)
#' @param .criticalCheckResults The listing output to be printed
#'
#' @importFrom DT datatable
#' @importFrom htmltools tagList
printCriticalCheck <- function(.criticalCheckId, .criticalCheckResults) {
  cat(paste0("******  \n"))

  cat(paste0("### ", toupper(.criticalCheckResults$checkId), " - ", .criticalCheckResults$checkShortDescription, "  \n"))
  cat(paste0("<div style='font-size:24px; font-weight:bold;'>**Results of Critical Check ", as.numeric(gsub("\\D", "", .criticalCheckId)), " - ", .criticalCheckResults$checkTitle, "**</div>  \n"))
  cat(paste0("**Description:** ", .criticalCheckResults$checkDescription, "  \n"))
  cat(paste0("**Send check to CDM/ROM:** ", .criticalCheckResults$sendCheckToRom, "  \n"))
  cat(paste0("  \n"))
  cat(paste0("**Pass:** ", .criticalCheckResults$pass, "  \n"))

  switch(.criticalCheckId,
    "criticalCheck1" = {
      cat(paste0("**Number Failed:** ", .criticalCheckResults$nDuplicateRows, "  \n"))
      if (.criticalCheckResults$nDuplicateRows > 0) {
        print(htmltools::tagList(DT::datatable(as.data.frame(.criticalCheckResults$listOfDuplicateRows),
          options = list(dom = "tp"),
          rownames = FALSE
        )))
      }
      cat(paste0("  \n"))
    },
    "criticalCheck2" = {
      cat(paste0("**Number Failed:** ", .criticalCheckResults$nExtraVars, "  \n"))
      if (.criticalCheckResults$nExtraVars > 0) {
        print(htmltools::tagList(DT::datatable(as.data.frame(.criticalCheckResults$extraVars),
          options = list(dom = "tp"),
          rownames = FALSE,
          colnames = c("Added Variables")
        )))
      }
      cat(paste0("  \n"))
    },
    "criticalCheck3" = {
      cat(paste0("**Number Failed:** ", .criticalCheckResults$nOmittedVars, "  \n"))
      if (.criticalCheckResults$nOmittedVars > 0) {
        print(htmltools::tagList(DT::datatable(as.data.frame(.criticalCheckResults$omittedVars),
          options = list(dom = "tp"),
          rownames = FALSE,
          colnames = c("Removed Vars")
        )))
      }
      cat(paste0("  \n"))
    },
    "criticalCheck4" = {
      cat(paste0("**Number Failed:** ", .criticalCheckResults$nMissingVariableLabels, "  \n"))
      if (.criticalCheckResults$nMissingVariableLabels > 0) {
        print(htmltools::tagList(DT::datatable(as.data.frame(.criticalCheckResults$listOfVarsWithMissingLabels),
          options = list(dom = "tp"),
          rownames = FALSE,
          colnames = c("Vars with Missing Labels")
        )))
      }
      cat(paste0("  \n"))
    },
    "criticalCheck5" = {
      cat(paste0("**Number of Current Rows:** ", .criticalCheckResults$nRowsThisMonth, "  \n"))
      cat(paste0("**Number of Old Rows:** ", .criticalCheckResults$nOldRows, "  \n"))

      cat(paste0("**Number Added Rows:** ", .criticalCheckResults$nAddedRows, "  \n"))
      cat(paste0("**Percent of Row Increase:** ", .criticalCheckResults$pctRowIncrease, "  \n"))
      cat(paste0("**Comparison Threshold:** ", .criticalCheckResults$threshold, "  \n"))
      cat(paste0("  \n"))
    },
    "criticalCheck6" = {
      cat(paste0("**Number of Current Rows:** ", .criticalCheckResults$nRowsThisMonth, "  \n"))
      cat(paste0("**Number of Old Rows:** ", .criticalCheckResults$nOldRows, "  \n"))

      cat(paste0("**Number of Removed Rows:** ", .criticalCheckResults$nRemovedRows, "  \n"))
      cat(paste0("**Comparison Threshold:** ", .criticalCheckResults$threshold, "  \n"))
      if (nrow(.criticalCheckResults$inOldAndNotInNew) > 0) {
        print(htmltools::tagList(DT::datatable(.criticalCheckResults$inOldAndNotInNew,
          options = list(dom = "tp"),
          rownames = FALSE
        )))
      }
      cat(paste0("  \n"))
    },
    "criticalCheck7" = {
      if (!is.null(.criticalCheckResults)) {
        if (nrow(.criticalCheckResults$essentialVariablesMissingness) > 0) {
          print(htmltools::tagList(DT::datatable(.criticalCheckResults$essentialVariablesMissingness,
            options = list(dom = "tp"),
            rownames = FALSE
          )))
        }
      }
      cat(paste0("  \n"))
    },
    "criticalCheck8" = {
      if (!is.null(.criticalCheckResults)) {
        if (nrow(.criticalCheckResults$essentialVariablesMissingness) > 0) {
          .dsToPrint <- .criticalCheckResults$essentialVariablesMissingness |>
            dplyr::select(varName, nMissingThisMonth, nRowsThisMonth, pctMissingThisMonth, pctMissingLastMonth, acceptableMissingness, skipLogic)
          print(htmltools::tagList(DT::datatable(.dsToPrint,
            options = list(dom = "tp"),
            rownames = FALSE
          )))
        }
      }
      cat(paste0("  \n"))
    },
    "criticalCheck9" = {
      if (!is.null(.criticalCheckResults)) {
        cat(paste0("**Number Failed:** ", .criticalCheckResults$nVariablesUnexpectedType, "  \n"))
        if (nrow(.criticalCheckResults$listOfVarsWithUnexpectedType) > 0) {
          .dsToPrint <- .criticalCheckResults$listOfVarsWithUnexpectedType
          print(htmltools::tagList(DT::datatable(.dsToPrint,
            options = list(dom = "tp"),
            rownames = FALSE
          )))
        }
      }
      cat(paste0("  \n"))
    },
    "criticalCheck10" = {
      if (!is.null(.criticalCheckResults)) {
        cat(paste0("**Number Failed:** ", .criticalCheckResults$values$n, "  \n"))
        if (.criticalCheckResults$values$n > 0) {
          .dsToPrint <- .criticalCheckResults$listing
          print(htmltools::tagList(DT::datatable(.dsToPrint,
            options = list(dom = "tp"),
            rownames = FALSE
          )))
        }
      }
      cat(paste0("  \n"))
    }
  )
}
