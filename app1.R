library(png)

shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(width=3,
        actionButton("newimages", "New images"),
        h4("Double-click options"),
        wellPanel(
          sliderInput("dblclick_delay", "Delay", min=100, max=1000, value=400,
            step=100)
        ),
        h4("Hover options"),
        wellPanel(
          radioButtons("hover_policy", "Input rate policy",
            c("debounce", "throttle"), inline = TRUE),
          sliderInput("hover_delay", "Delay", min=100, max=1000, value=200,
            step=100)
        ),
        h4("Brush options"),
        wellPanel(
          radioButtons("brush_dir", "Direction(s)",
            c("xy", "x", "y"), inline = TRUE),
          radioButtons("brush_policy", "Input rate policy",
            c("debounce", "throttle"), inline = TRUE),
          sliderInput("brush_delay", "Delay", min=100, max=1000, value=200,
            step=100),
          checkboxInput("brush_reset", "Reset on new image")
        )
      ),
      mainPanel(width = 9,
        fluidRow(
          column(width = 6,
            h2("plotOutput (data coords)"),
            uiOutput("plotui"),
            fluidRow(
              column(width = 6,
                verbatimTextOutput("plot_clickinfo"),
                verbatimTextOutput("plot_dblclickinfo")
              ),
              column(width = 6,
                verbatimTextOutput("plot_hoverinfo"),
                verbatimTextOutput("plot_brushinfo")
              )
            )
          ),
          column(width = 6,
            h2("imageOutput (pixel coords)"),
            uiOutput("imageui"),
            fluidRow(
              column(width = 6,
                verbatimTextOutput("image_clickinfo"),
                verbatimTextOutput("image_dblclickinfo")
              ),
              column(width = 6,
                verbatimTextOutput("image_hoverinfo"),
                verbatimTextOutput("image_brushinfo")
              )
            )
          )
        )
      )
    )
  ),
  server = function(input, output, session) {
    output$plotui <- renderUI({
      plotOutput("plot", height=300,
        click = "plot_click",
        dblclick = dblclickOpts(
          id = "plot_dblclick",
          delay = input$dblclick_delay
        ),
        hover = hoverOpts(
          id = "plot_hover",
          delay = input$hover_delay,
          delayType = input$hover_policy
        ),
        brush = brushOpts(
          id = "plot_brush",
          delay = input$brush_delay,
          delayType = input$brush_policy,
          direction = input$brush_dir,
          resetOnNew = input$brush_reset
        )
      )
    })
    output$plot <- renderPlot({
      input$newimages
      cars2 <- cars + rnorm(nrow(cars))
      plot(cars2)
    })
    output$plot_clickinfo <- renderPrint({
      cat("Click:\n")
      str(input$plot_click)
    })
    output$plot_dblclickinfo <- renderPrint({
      cat("Double-click:\n")
      str(input$plot_dblclick)
    })
    output$plot_hoverinfo <- renderPrint({
      cat("Hover:\n")
      str(input$plot_hover)
    })
    output$plot_brushinfo <- renderPrint({
      cat("Brush:\n")
      str(input$plot_brush)
    })

    output$imageui <- renderUI({
      imageOutput("image", height=300,
        click="image_click",
        dblclick = dblclickOpts(
          id = "image_dblclick",
          delay = input$dblclick_delay
        ),
        hover = hoverOpts(
          id = "image_hover",
          delay = input$hover_delay,
          delayType = input$hover_policy
        ),
        brush = brushOpts(
          id = "image_brush",
          delay = input$brush_delay,
          delayType = input$brush_policy,
          direction = input$brush_dir,
          resetOnNew = input$brush_reset
        )
      )
    })

    output$image <- renderImage({
      input$newimages

      # Get width and height of image output
      width  <- session$clientData$output_image_width
      height <- session$clientData$output_image_height
      npixels <- width * height

      # Fill the pixels for R, G, B
      img <- c(
        rep(runif(1), npixels),
        rep(runif(1), npixels),
        rep(runif(1), npixels)
      )
      # Convert the vector to an array with 3 planes
      img <- array(img, dim = c(width, height, 3))

      # Write it to a temporary file
      outfile <- tempfile(fileext = ".png")
      writePNG(img, target = outfile)

      # Return a list containing information about the image
      list(
        src = outfile,
        contentType = "image/png",
        width = width,
        height = height,
        alt = "This is alternate text"
      )
    })
    output$image_clickinfo <- renderPrint({
      cat("Click:\n")
      str(input$image_click)
    })
    output$image_dblclickinfo <- renderPrint({
      cat("Double-click:\n")
      str(input$image_dblclick)
    })
    output$image_hoverinfo <- renderPrint({
      cat("Hover:\n")
      str(input$image_hover)
    })
    output$image_brushinfo <- renderPrint({
      cat("Brush:\n")
      str(input$image_brush)
    })
  }
)