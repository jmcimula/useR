library(httr) #Tools for Working with URLs and HTTP
library(rvest) #Harvest information from the web using R


x <- "Big Data Analytics" #Example

y <- GET("http://google.com/", path = "search", query = list(q = x)) 


bigData <- read_html(y$url) %>%
           html_nodes("div h3")%>%
           html_text()

print(bigData) #Just the title fromt the search
               #It is possible to retrieve the link and open this last for extracting information
