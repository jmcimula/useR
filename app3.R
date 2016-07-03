library(DT)
library(shiny)
# options(shiny.reactlog=TRUE)

id <- 1:10
label <- paste0("lab","-",id)
ratio <- runif(10, 1.0, 10.0)
ratio <- round(ratio,0)
df <- ds <- data.frame(id=id, label=label,ratio=ratio, stringsAsFactors = FALSE)

ui <- fluidPage(
  fluidRow(
    column(5, uiOutput('resSli')) ,
    column(7, textOutput("resAv",inline=TRUE),
      plotOutput('pie'))
  )
)

server <- function(session, input, output) {

  output$resSli <- renderUI({
    df_  <- rawData()
    
      buttn <- 1
      
      iter <- length(df_$id) 
      
      iter <- iter / 2
      toRender <- lapply(1:iter, function(i) {
        fluidRow(
          column(2,h6(df_$label[i], 
            style = "font-family: 'Helvetica Neue', small;
            font-weight: 200; line-height: 1.0;")),
          column(3,numericInput(paste0(df_$label[i],buttn),
            label = NULL, value = df_$ratio[i],min = 1, max = 100)),
          column(2,h6(df_$label[i+iter], 
            style = "font-family: 'Helvetica Neue', small;
            font-weight: 200; line-height: 1.0;")),
          column(3,numericInput(paste0(df_$label[i+iter],buttn),
            label = NULL, value = df_$ratio[i+iter], min = 1, max = 100))
        )
      })
      return(toRender)
      
  })
  ######
  
  # The original data. Won't change in response to user input.
  rawData <- reactive({
    df
  })
  
  # Numeric vector of ratios that the user entered. Doesn't include
  # RestTo100.
  userRatios <- reactive({
    buttn <- 1
    sapply(rawData()$label, function(label) {
      validate(need(input[[paste0(label,buttn)]], message = FALSE))
      as.numeric(input[[paste0(label,buttn)]])  
    })
  })
  
  # The RestTo100 value. Will change in response to user input.
  remainder <- reactive({
    max(0, 100 - sum(as.numeric(userRatios())))
  })
  
  # The complete dataframe. Will change in response to user input.
  resRDs <- reactive({
    df_ <- rawData()
    df_$ratio <- userRatios()
    rbind(df_,
      data.frame(id = nrow(df_) + 1, label = "RestTo100", ratio = remainder())
    )
  })
  
  #########
  
  output$resAv <- renderText({ 
    return(paste0("Available resources: ",round(remainder()),"%"))
  })
  
  ##########
  
  
  
  output$pie <- renderPlot({
    df_ <- resRDs()
    df_$ratio <- round(as.numeric(df_$ratio),0)
    
    lbls <- paste(df_$label,"%",sep="") # ad % to labels 
    pie(df_$ratio,labels = lbls, col=rainbow(length(lbls)),
      main="Pie Chart of Labels")
  })
  
  
}


shinyApp(ui, server)
