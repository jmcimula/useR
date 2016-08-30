#Testing the package ReporteRs
#ReporteRs: Microsoft Word, Microsoft PowerPoint and HTML Documents Generation

library(ReporteRs)
doc = pptx()
# Slide 1 : Title slide
doc <- addSlide(doc, "Title Slide")
doc <- addTitle(doc,"Document containing plots and images")
doc <- addSubtitle(doc, "R and ReporteRs package")
# Slide 2 : Add plot
doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc,"Histogram plot")
doc <- addPlot(doc, function() hist(iris$Sepal.Width, col="lightblue"))
doc <- addParagraph(doc, "This histogram is generated using iris data sets")
# Slide 3 : Add  r stat

 
writeDoc(doc, "r-reporters-powerpoint-plot.pptx")