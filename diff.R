library(git2r)
library(plyr)
library(daff)

differ = function(file1, file2){
  path <- tempfile(pattern="git2r-")
  dir.create(path)
  repo <- init(path)

  suppressWarnings({
    f1 = readLines(file1)
  })
  suppressWarnings({
    f2 = readLines(file2)
  })
  ## Create a file, add, commit
  base = "test.txt"
  tfile = file.path(path, base)
  writeLines(f1, tfile)

  add(repo, base)
  commit(repo, "Added First File")

  ## Change the file
  writeLines(f2, tfile)
  diff_1 <- diff(repo, as_char = TRUE)
  diff_1 = gsub("a/test.txt", file1, diff_1, fixed = TRUE)
  diff_1 = gsub("b/test.txt", file2, diff_1, fixed = TRUE)
  # summary(diff_1)
  # cat(diff(repo, as_char=TRUE))
  return(diff_1)
}

file1 = "difftest.txt"
file2 = "difftest2.txt"
write("I am John", file1)
file.copy(file1, file2, overwrite = TRUE)
write("butI am not John", file2, append = TRUE)

diff_msg = differ(file1, file2)
cat(diff_msg)


differ_print = function(file1, file2, ...) {
  diff_msg = differ(file1, file2)

  ss = strsplit(diff_msg, "\n")[[1]]
  ss = ss[-(1:4)]
  ss = grep("^( |\\+|\\-)", ss, value=TRUE)
  nc = nchar(ss)
  first = substr(ss, 1, 1)
  first = plyr::revalue(first, c("+" = "+++",
                                 "-" = "---"), warn_missing = FALSE)
  start = paste("@@,diff", file1, file2)
  ss = c(start, paste0(first, ",", substr(ss, 2, nc)))
  ss = paste(ss, collapse = "\n")
  tfile = tempfile(fileext = ".csv")
  writeLines(ss, con = tfile)

  patch <- read_diff(tfile)
  render_diff(patch, ...)
  return(diff_msg)
}

diff_msg = differ_print(file1, file2)
diff_msg = differ_print(file1, file2, pretty = TRUE)

library(gistr)

diff_gist = function(file1, file2, ...) {
  diff_msg = differ(file1, file2)
  ss = strsplit(diff_msg, "\n")[[1]]
  ind = grep("^@@", ss)
  start = paste("## difference between", file1, file2)
  ss =  ss[seq(ind + 1, length(ss))]
  ss = gsub("^ ", "", ss)
  ss = c(start, "```diff", ss, "```")
  ss = paste(ss, collapse = "\n")
  tfile = tempfile(fileext = ".md")
  writeLines(ss, con = tfile)
  res = gist_create(files = tfile, ...)
  return(list(diff_msg = diff_msg, gist = res))
}

res = diff_gist(file1, file2)


open = function(x) system(sprintf("open %s", x))

library(markdown)
diff_nogist = function(file1, file2, ...) {
  diff_msg = differ(file1, file2)
  ss = strsplit(diff_msg, "\n")[[1]]
  top = c(
    '<link rel="stylesheet" href="http://cdnjs.cloudflare.com/ajax/libs/highlight.js/8.9.1/styles/default.min.css">',
    '<script src="https://highlightjs.org/static/highlight.pack.js"></script>',
    "<script>hljs.initHighlightingOnLoad();</script>")
  ss = c(top, "```diff", ss, "```")
  ss = paste(ss, collapse = "\n")
  tfile = tempfile(fileext = ".md")
  writeLines(ss, con = tfile)
  ofile = gsub("[.]md$", ".html", tfile)
  markdownToHTML(tfile, output = ofile)
  browseURL(ofile)
#   library(rmarkdown)
#   render(tfile, output_file = ofile)
#   browseURL(ofile)
  return(ofile)
}

res = diff_nogist(file1, file2)