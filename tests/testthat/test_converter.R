library(iskay)
context("Converter test")

test_that("converter test", {
  #library(agricolae)
  #data("grass")
  fp <- rprojroot::find_testthat_root_file("test_data", "grass.csv")
  datos <- read.csv(fp)
  
  #base <- as.POSIXct("2009-08-03 12:01:59.23", tz = "UTC")
  #datos <- readxl::read_excel("test_data/friedman_test.xlsx")
  out <- agricolae::friedman(datos[,"judge"], datos[,"trt"], datos[,"evaluation"],alpha=0.05)
  meandt <- out$means
  adt <- agr2df(meandt)
  # ncolumns <- ncol(adt)
  # filas <- nrow(adt)
  expect_equivalent(ncol(adt), 6)
  expect_equivalent(nrow(adt), 4)
  
})