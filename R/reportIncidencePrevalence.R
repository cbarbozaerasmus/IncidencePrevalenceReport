#' Creates an HTML report taking the prevalence and incidence estimates from IncidencePrevalence.
#'
#' @param studyTitle A character title of the study.
#' @param studyAuthor A character name of the author.
#' @param prevalenceData A tibble from IncidencePrevalence.
#' @param prevalenceData A tibble from IncidencePrevalence.
#' @export
#' @import dplyr CDMConnector rmarkdown here ggplot2 quarto
#' @return An HTML document
reportIncidencePrevalence <- function(studyTitle,
                                      studyAuthor,
                                      prevalenceData,
                                      incidenceData,
                                      word = TRUE) {

  if (word == TRUE) {
    quarto::quarto_render(
      input = paste0(system.file(package = "IncidencePrevalenceReport"),
                     "/rmarkdown/templates/IncidencePrevalenceReport.Rmd"),
      output_format = word_document(reference_docx = paste0(system.file(package = "IncidencePrevalenceReport"),
                                                            "/rmarkdown/templates/IncidencePrevalenceReport.docx")),
      output_file = here("Reports/IncidencePrevalenceReport")
      )
  } else {
    rmarkdown::render(
      input = paste0(system.file(package = "IncidencePrevalenceReport"),
                     "/rmarkdown/templates/IncidencePrevalenceReport.Rmd"),
      output_format = "html_document",
      output_file = here("Reports/IncidencePrevalenceReport"),
      # params = list(titleParam = studyTitle,
      #               authorParam = studyAuthor,
      #               prevalenceParam = prevalenceData,
      #               incidenceParam = incidenceData),
      encoding = 'UTF-8'
    )
  }
}
