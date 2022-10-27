#' This application displays an automated and interactive report of the results from the IncidencePrevalence package.
#'
#' @param studyTitle A character title of the study.
#' @param studyAuthor A character name of the author.
#' @param prevalenceData A tibble from IncidencePrevalence.
#' @param prevalenceData A tibble from IncidencePrevalence.
#' @export
#' @import dplyr CDMConnector rmarkdown here ggplot2 quarto shiny
#' @return An HTML document
ui <- shiny::fluidPage(
  shiny::selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  shiny::verbatimTextOutput("summary"),
  shiny::tableOutput("table")
)
server <- function(input, output, session) {
  output$summary <- renderPrint({
    dataset <- get(input$dataset, "package:datasets")
    summary(dataset)
  })

  output$table <- renderTable({
    dataset <- get(input$dataset, "package:datasets")
    dataset
  })
}
shiny::shinyApp(ui, server)
