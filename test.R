.expr_to_fun <- function(x) {
  expr <- substitute(x)
  res <- force(x)
  
  if (is.function(res)) {
    list(f = res, val = res())
  } else {
    f <- eval.parent(substitute(function() expr))
    list(f = f, val = res)
  }
}

test <- function() {
  require(assertthat)
  f <- function(x) .expr_to_fun(x)
  
  inc <- function() i <<- i + 1
  
  i <- 0
  f2e <- f(inc())
  f2e$f()
  print(i)
  assert_that(i == 2)
}

test()
