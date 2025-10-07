#' generateReport
#'
#' @param .inputDatasetUrl direct link to the .RDS file that has the check report output
#' @param .reportOutputUrl link to the folder where the report will be stored
#' @param .fileName name of the output file
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom rmarkdown render
generateReport <- function(.inputDatasetUrl, .reportOutputUrl, .fileName) {
  .checkDataset <- readRDS(glue::glue("{.inputDatasetUrl}"))

  # Render the rmarkdown file as a single HTML file
  rmarkdown::render(system.file("rmarkdown", "template.Rmd", package = "registrydqchecksreportdown"),
    output_dir = .reportOutputUrl,
    output_file = glue::glue("{.fileName}.html"),
    output_format = rmarkdown::html_document(
      toc = TRUE,
      toc_float = TRUE,
      toc_depth = 4,
      includes = rmarkdown::includes(,
        after_body = system.file("www/toc-collapse.js", package = "registrydqchecksreportdown")
      )
    ),
    params = list(checkDataset = .checkDataset)
    # ,envir = new.env(parent = globalenv())
  )

  # Remove the intermediary file
  # file.remove(glue::glue("{.reportOutputUrl}{.fileName}.md"))
}
