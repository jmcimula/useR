library(dygraphs)#Interactive Time Series Charting Library


dygraph(nhtemp, main = "New Haven Temperatures")

dygraph(nhtemp, main = "New Haven Temperatures") %>%
    dyAxis("y", label = "Temp (F)", valueRange = c(40, 60)) %>%
    dyOptions(fillGraph = TRUE, drawGrid = FALSE) %>%
    dyRangeSelector()
