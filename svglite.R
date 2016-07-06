library(svglite)
x <- runif(1e3)
y <- runif(1e3)

tmp1 <- tempfile()
tmp2 <- tempfile()
system.time({
  svglite(tmp1)
  plot(x, y)
  dev.off()
})
#>    user  system elapsed 
#>   0.003   0.001   0.003 

system.time({
  svg(tmp2, onefile = TRUE)
  plot(x, y)
  dev.off()
})
#>    user  system elapsed 
#>   0.015   0.001   0.017 

#It also produces considerably smaller files:

file.size(tmp1) / 1024
#> [1] 93.54785
file.size(tmp2) / 1024
#> [1] 321.1357

#In both cases, compressing to make  .svgz  is worthwhile:

gz <- function(in_path, out_path = tempfile()) {
  out <- gzfile(out_path, "w")
  writeLines(readLines(in_path), out)
  close(out)

  invisible(out_path)
}
file.size(gz(tmp1)) / 1024
#> [1] 9.064453
file.size(gz(tmp2)) / 1024
#> [1] 38.6123
