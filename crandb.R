library(crandb)#Database of CRAN R packages

pack_list <- list_packages(format = "latest", limit = 9000)

result <- bind_rows(lapply(pack_list, function(x) as_tibble(x[c("Package", "Maintainer", "Author")])))

result$name <- gsub("\\s$", "",  unlist(lapply(strsplit(result$Maintainer, "<"), "[[", 1)))

result  %>% group_by(name)  %>% summarize(count  = n())  %>% arrange(desc(count))  %>% filter(count > 9)  %>% as.data.frame()