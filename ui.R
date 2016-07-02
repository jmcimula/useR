library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Visualizing ToothGrowth data set with different geoms"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    selectInput("x_var", "x variable:",
                c("Dose"            = "dose",
                  "Supplement Type" = "supp")),
    selectInput("color_var", "fill color variable:",
                c("None"            = "",
                  "Dose"            = "dose",
                  "Supplement Type" = "supp"),
                selected = "Supplement Type"),

    conditionalPanel(
      condition = "input.color_var != ''",
      sliderInput("dodgewidth", "Horizontal dodging width (for different colors):",
                  min = 0.1, max = 1, value = .9, step = 0.1),
      conditionalPanel(
        condition = "output.old_ggplot2",
        p("The server is using a version of ggplot2 prior to 0.9.3. Note that
          this version has a bug where it doesn't allow control over dodging
          width of dotplots.")
      )
    ),

    wellPanel(
      checkboxInput("geom_point", strong("Geom: point"), FALSE),
      conditionalPanel(
        condition = "input.geom_point",
        sliderInput("point_alpha", "Alpha (transparency):",
                    min = 0.0, max = 1, value = 1, step = 0.1),
        sliderInput("point_size", "Size:",
                    min = 0.5, max = 8, value = 3, step = 0.5)
      )
    ),

    wellPanel(
      checkboxInput("geom_dotplot", strong("Geom: dotplot"), TRUE),
      conditionalPanel(
        condition = "input.geom_dotplot",
        radioButtons("dotplot_method", "Binning method",
                     c("Dot-density"                 = "dotdensity",
                       "Histodot (regular spacing)" = "histodot")),
        sliderInput("dotplot_binwidth", "Bin width:",
                    min = 0.5, max = 3, value = 1, step = 0.25),
        sliderInput("dotplot_alpha", "Alpha (transparency):",
                    min = 0.0, max = 1, value = 1, step = 0.1),

        selectInput("dotplot_stackdir", "Stacking direction:",
                    c("Centered"         = "center",
                      "Centered-aligned" = "centerwhole"))
      )
    ),

    wellPanel(
      checkboxInput("geom_boxplot", strong("Geom: boxplot"), FALSE),
      conditionalPanel(
        condition = "input.geom_boxplot",
        sliderInput("boxplot_width", "Width:",
                    min = 0.1, max = 1, value = .5, step = 0.1),
        checkboxInput("boxplot_notch", "Notched", FALSE),
        checkboxInput("boxplot_outliers", "Show outliers", TRUE),

        conditionalPanel(
          condition = "input.boxplot_outliers == true",
          sliderInput("boxplot_outlier_size", "Outlier size:",
                      min = 1, max = 8, value = 3, step = 1)
        )
      )
    ),

    wellPanel(
      checkboxInput("geom_violin", strong("Geom: violin"), FALSE),
      conditionalPanel(
        condition = "input.geom_violin",
        sliderInput("violin_adjust", "Bandwidth adjustment ratio:",
                    min = 0.25, max = 2, value = 1, step = 0.25),
        checkboxInput("violin_trim", "Trim violins to data range", TRUE)
      )
    )

  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    plotOutput("tgPlot"),

    sliderInput("show_nrows", "Show N rows of data in table below:",
                min = 1, max = 60, value = 10, step = 5),

    tableOutput("data_view")

  )
))