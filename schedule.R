
#Using shiny anf flytr to visualise the schedule of Air France

#devtools::install_github("jmcimula/flytr")

library(flytr)
library(shiny)
shinyApp(
  ui = miniUI::miniPage(DT::dataTableOutput('tbl')),
  server = function(input, output) {
    output$tbl = DT::renderDataTable(
      ft_schedule("air-france-af-afr")
    )
  }
)
