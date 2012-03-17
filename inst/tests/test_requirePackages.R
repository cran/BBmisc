context("requirePackages")

test_that("requirePackages", {
  expect_equal(requirePackages("MASS"), c(MASS=TRUE))
  expect_equal(requirePackages("xxx", stop=FALSE, suppress.warnings=TRUE),
    c(xxx=FALSE))
  expect_error(requirePackages("xxx", suppress.warnings=TRUE),
    "Please install the following packages: xxx")               
  expect_equal(requirePackages(c("xxx", "MASS"), stop=FALSE, suppress.warnings=TRUE), 
    c(xxx=FALSE, MASS=TRUE))
  expect_equal(requirePackages(c("MASS", "xxx"), stop=FALSE, suppress.warnings=TRUE), 
    c(MASS=TRUE, xxx=FALSE))
  expect_error(requirePackages(c("MASS", "xxx"), suppress.warnings=TRUE), 
    "Please install the following packages: xxx")               
  expect_error(requirePackages(c("MASS", "xxx"), why="test", suppress.warnings=TRUE), 
    "For test please install the following packages: xxx")                 
})
