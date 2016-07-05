library(shiny)
library(dygraphs)

options(shiny.launch.browser = NULL)

ui <- fluidPage(
  radioButtons("mode", "Choose mode:", inline = TRUE, c(
    "Normal",
    "Null",
    "Error",
    "Validation error",
    "Silent validation error"
  )),
  dygraphOutput("dy"),
  "This is some content below the dygraph."
)

server <- function(input, output, session) {
  value <- reactive({
    switch(input$mode,
      "Normal" = dygraph(nhtemp, main = "New Haven Temperatures", ylab = "Temp (F)"),
      "Null" = NULL,
      "Error" = stop("An error has occurred"),
      "Validation error" = validate(need(FALSE, message = "A validation error has occurred")),
      "Silent validation error" = validate(FALSE)
    )
  })
  output$dy <- renderDygraph({
    value()
  })
}

shinyApp(ui, server)