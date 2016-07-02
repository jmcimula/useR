library(shiny)
library(datasets)
library(ggplot2)

tg <- ToothGrowth
tg$dose <- factor(tg$dose)

# Define server logic
shinyServer(function(input, output) {

  # Versions of ggplot2 < 0.9.3 had a bug with dotplot dodging
  if (packageVersion("ggplot2") < as.package_version("0.9.3"))
    old_ggplot2 <- TRUE
  else 
    old_ggplot2 <- FALSE

  # Tell the webpage that we're using the old or new version
  output$old_ggplot2 <- reactive(function() old_ggplot2)

  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$tgPlot <- reactivePlot(function() {

    # Figure out the aesthetic mappings. color_var requires special handling
    # because it can be "none".
    if (input$color_var == "") {
      aes_mapping <- aes_string(x = input$x_var, y = "len")
    } else {
      aes_mapping <- aes_string(x = input$x_var, y = "len",
                                fill = input$color_var)
    }

    p <- ggplot(tg, mapping = aes_mapping)



    if (input$geom_violin) {
      p <- p + geom_violin(trim = input$violin_trim,
                           adjust = input$violin_adjust,
                           position = position_dodge(input$dodgewidth))
    }


    if (input$geom_boxplot) {
      if (input$boxplot_outliers)
        outlier_color <- "black"
      else
        outlier_color <- NA

      p <- p + geom_boxplot(width = input$boxplot_width,
                            notch = input$boxplot_notch,
                            outlier.colour = outlier_color,
                            outlier.size = input$boxplot_outlier_size,
                            position = position_dodge(input$dodgewidth))
    }


    if (input$geom_dotplot) {
      if (old_ggplot2) {
        dotplot_dodge <- "dodge"
      } else {
        dotplot_dodge <- position_dodge(input$dodgewidth)
      }
      p <- p + geom_dotplot(binaxis="y", stackdir=input$dotplot_stackdir,
                            method = input$dotplot_method,
                            binwidth = input$dotplot_binwidth,
                            alpha = input$dotplot_alpha,
                            # Using position=position_dodge() is broken in ggplot2 0.9.2.1.
                            # So just use "dodge"
                            position = dotplot_dodge)
    }


    if (input$geom_point) {
      p <- p + geom_point(shape = 21, size = input$point_size,
                          colour = "black",
                          alpha = input$point_alpha,
                          position = position_dodge(input$dodgewidth))
    }

    print(p)
  })


  # Show the first "n" observations
  output$data_view <- reactiveTable(function() {
    head(ToothGrowth, n = input$show_nrows)
  })
})