library(png)
library(ggplot2)
library(Cairo)


mtc <- mtcars
mtc$cyl <- factor(mtc$cyl)
mtc$am  <- factor(mtc$am)
mtc$vs  <- factor(mtc$vs)

mtc$date <- Sys.Date() + seq_len(nrow(mtc))
mtc$datetime <- Sys.time() + 60 * seq_len(nrow(mtc))

# Data set with points on a grid
grid <- data.frame(
  x = rep(1:8, 4),
  y = rep(1:4, each = 8)
)

shinyApp(
  ui = fluidPage(
    # Smaller font for preformatted text
    tags$head(
      tags$style(HTML("
        pre, table.table {
          font-size: smaller;
        }
      "))
    ),

    sidebarLayout(
      sidebarPanel(width=3,
        actionButton("newimages", "Generate new images"),
        radioButtons("dataset", "Data set",
          choices = c("mtcars", "diamonds", "grid"), inline = TRUE),
        radioButtons("plot_type", "Plot type",
          c("base", "ggplot2"), inline = TRUE),

        conditionalPanel("input.plot_type === 'base'",
          wellPanel(
            selectInput("plot_scaletype", "Scale type",
              c("normal" = "normal",
                "log" = "log",
                "x factor" = "x_factor"
              ),
              selectize = FALSE
            )
          )
        ),

        conditionalPanel("input.plot_type === 'ggplot2'",
          wellPanel(
            selectInput("ggplot_scaletype", "Scale type",
              c("normal" = "normal",
                "reverse (scale_*_reverse())" = "reverse",
                "log10 (scale_*_log10())" = "log10",
                "log2 (scale_*_continuous( trans=log2_trans()))" = "log2",
                "log10 (coord_trans())" = "log10_trans",
                "log2 (coord_trans())" = "log2_trans",
                "coord_cartesian()" = "coord_cartesian",
                "coord_flip()" = "coord_flip",
                "coord_polar() (doesn't work)" = "coord_polar",
                "x factor" = "x_factor",
                "x datetime" = "x_datetime"
              ),
              selectize = FALSE
            ),
            selectInput("ggplot_facet", "Facet",
              c("none" = "none",
                "wrap" = "wrap",
                "grid x" = "grid_x",
                "grid y" = "grid_y",
                "grid xy" = "grid_xy",
                "grid xy free" = "grid_xy_free"
              ),
              selectize = FALSE
            )
          )
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
        ),
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
            ),
            div(
              h4("Points near last click:"),
              tableOutput("plot_clicked_points"),
              h4("Brushed points:"),
              tableOutput("plot_brushed_points")
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

    # Currently selected dataset
    curdata <- reactive({
      switch(input$dataset, mtcars = mtc, diamonds = diamonds, grid = grid)
    })

    xvar <- reactive({
      if (input$plot_scaletype == "x_factor") {
        switch(input$dataset, mtcars = "cyl", diamonds = "cut")
      } else {
        switch(input$dataset, mtcars = "wt", diamonds = "carat", grid = "x")
      }
    })
    yvar <- reactive({
      switch(input$dataset, mtcars = "mpg", diamonds = "price", grid = "y")
    })


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

      if (input$plot_type == "base" || input$plot_type == "grid") {
        xvals <- curdata()[[xvar()]]
        yvals <- curdata()[[yvar()]]

        switch(input$plot_scaletype,
          normal =
            plot(xvals, yvals),
          log =
            plot(xvals, yvals, log = "xy"),
          x_factor =
            plot(xvals, yvals)
        )

      } else if (input$plot_type == "ggplot2") {
        dat <- curdata()
        pc <- ggplot(curdata(), aes_string(xvar(), yvar())) +
            geom_point() +
            theme_bw()

        p <- switch(input$ggplot_scaletype,
          normal =
            pc,
          reverse =
            pc + scale_x_reverse() + scale_y_reverse(),
          log10 =
            pc + scale_x_log10() + scale_y_log10(),
          log2 =
            pc + scale_x_continuous(trans = scales::log2_trans()) +
                 scale_y_continuous(trans = scales::log2_trans()),
          log10_trans =
            pc + coord_trans(xtrans = "log10", ytrans = "log10"),
          log2_trans =
            pc + coord_trans(xtrans = "log2", ytrans = "log2"),
          coord_cartesian =
            pc + coord_cartesian(xlim = c(2,4), ylim = c(0,50)),
          coord_flip =
            pc + coord_flip(),
          coord_polar =
            pc + coord_polar(),
          # Discrete x, continuous y
          x_factor =
            pc,
          # Datetime x, Date y
          x_datetime =
            ggplot(mtc, aes(datetime, date)) + geom_point() + theme_bw()
        )

        p <- switch(input$ggplot_facet,
          none =
            p,
          wrap =
            p + facet_wrap(~ cyl, ncol=2),
          grid_x =
            p + facet_grid(. ~ cyl),
          grid_y =
            p + facet_grid(cyl ~ .),
          grid_xy =
            p + facet_grid(am ~ cyl),
          grid_xy_free =
            p + facet_grid(am ~ cyl, scales = "free")
        )

        p
      }
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
    output$plot_clicked_points <- renderTable({
      dat <- curdata()

      # With base graphics, we need to explicitly tell it which variables were
      # used; with ggplot2, we don't.
      if (input$plot_type == "base")
        res <- nearPoints(dat, input$plot_click, xvar(), yvar(), addDist = TRUE)
      else if (input$plot_type == "ggplot2")
        res <- nearPoints(dat, input$plot_click, addDist = TRUE)

      if (nrow(res) == 0)
        return(NULL)
      res
    })
    output$plot_brushed_points <- renderTable({
      dat <- curdata()
      # With base graphics, we need to explicitly tell it which variables were
      # used; with ggplot2, we don't.
      if (input$plot_type == "base")
        res <- brushedPoints(dat, input$plot_brush, xvar(), yvar())
      else if (input$plot_type == "ggplot2")
        res <- brushedPoints(dat, input$plot_brush)

      if (nrow(res) == 0)
        return(NULL)
      res
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