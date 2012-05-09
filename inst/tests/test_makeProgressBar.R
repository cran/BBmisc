context("makeProgressBar")

test_that("makeProgressBar", {
  cat("\n")
  bar = makeProgressBar()
  for(i in 0:100) { 
    bar$set(i)
    Sys.sleep(0.01)
  }
  bar = makeProgressBar(min=10, max=50, label="foo")
  for(i in 11:50) {
    bar$set(i)
    Sys.sleep(0.01)
  }
  bar = makeProgressBar(min=0.1, max=0.2)
  for(i in seq(0.1, 0.2, length.out=5)) {
    bar$set(i)
    Sys.sleep(0.1)
  }
  bar$set(0.2)
  bar$set(0.2)
  bar = makeProgressBar(max=10^6, label="          ")
  for(i in 10^seq(1:6)) {
    bar$set(i, msg=sprintf("%i", i))
    Sys.sleep(0.1)
  }
  
  bar = makeProgressBar(min=0, max=0)
  bar$set(0)
  bar = makeProgressBar(min=0, max=0)
  bar$inc(0)
})