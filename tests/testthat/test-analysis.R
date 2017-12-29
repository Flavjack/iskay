library(iskay)
context("Test for test_analysis")

test_that("converter test friedman", {
  fp <- rprojroot::find_testthat_root_file("test_data", "grass.csv")
  datos <- read.csv(fp)
  out <- test_analysis(x= datos$trt, y =datos$evaluation , jud = datos$judge, test = "friedman")
  adt <- out$dt
  expect_equivalent(nrow(adt), 4)
  
})


test_that("converter test kruskal", {
  fp <- rprojroot::find_testthat_root_file("test_data", "kruskal_corn.csv")
  datos <- read.csv(fp)
  
  as <- test_analysis(x= datos$method, y = datos$observation, test = "kruskal")
  adt <- as$dt
  expect_equivalent(ncol(adt), 8 )
  expect_equivalent(nrow(adt), 4)
  
})



test_that("converter test Jonckheere-Tepstra", {
  fp <- rprojroot::find_testthat_root_file("test_data", "Jonkcherre_NaCl.xlsx")
  datos <- readxl::read_excel(fp)
  
  as <- test_analysis(x= datos$dataset.Juicio, y = datos$dataset.NaCl, hyp = "increasing", test = "jonckheere")
  adt <- as$dt 
  expect_equivalent(ncol(adt), NULL)
  expect_equivalent(nrow(adt), NULL)
  
})


