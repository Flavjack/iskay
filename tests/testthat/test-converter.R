library(iskay)
context("Converter test")

test_that("converter test friedman", {
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


test_that("converter test durbin", {
  fp <- rprojroot::find_testthat_root_file("test_data", "durbin_toxico.xlsx")
  datos <- readxl::read_excel(fp)
  datos <- as.data.frame(datos, stringsAsFactors=FALSE)
  out <- agricolae::durbin.test(judge = datos[,"days"], trt = datos[,"chemical"], evaluation = datos[,"toxic"], group = TRUE, alpha=0.05)
  meandt <- out$means
  adt <- agr2df(meandt, test = "durbin")
  expect_equivalent(ncol(adt), 6)
  expect_equivalent(nrow(adt), 7)
  
})



test_that("converter test kruskal", {
  #library(agricolae)
  #data("grass")
  fp <- rprojroot::find_testthat_root_file("test_data", "kruskal_corn.csv")
  datos <- read.csv(fp)
  
  #base <- as.POSIXct("2009-08-03 12:01:59.23", tz = "UTC")
  #datos <- readxl::read_excel("test_data/friedman_test.xlsx")
  out <- agricolae::kruskal(datos[,"observation"], datos[,"method"],group = TRUE, alpha=0.05)
  meandt <- out$means
  adt <- agr2df(meandt,test = "kruskal")
  # ncolumns <- ncol(adt)
  # filas <- nrow(adt)
  expect_equivalent(ncol(adt), 7)
  expect_equivalent(nrow(adt), 4)
  
})



test_that("converter test Jonckheere-Tepstra", {
  fp <- rprojroot::find_testthat_root_file("test_data", "Jonkcherre_NaCl.xlsx")
  datos <- readxl::read_excel(fp)
  outjonck <- clinfun::jonckheere.test(x=datos$dataset.Juicio, g=datos$dataset.NaCl, alternative = "in")
  adt <- broom::glance(outjonck)
  expect_equivalent(ncol(adt), 4)
  expect_equivalent(nrow(adt), 1)
  
})

test_that("converter test Median", {
  fp <- rprojroot::find_testthat_root_file("test_data", "Jonkcherre_NaCl.xlsx")
  datos <- readxl::read_excel(fp)
  outjonck <- clinfun::jonckheere.test(x=datos$dataset.Juicio, g=datos$dataset.NaCl, alternative = "in")
  adt <- broom::glance(outjonck)
  expect_equivalent(ncol(adt), 4)
  expect_equivalent(nrow(adt), 1)
  
})


#Mann-Withney does not need converter.