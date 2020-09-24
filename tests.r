library(magrittr)
library(testthat)
source("helpers.R")
source("graph.R")

fx1 <- "2*x+2"
fx2 <- "2+2*x^2"
fx3 <- c(fx2, "3+3*x") # to ensure vectors of functions work
fx4 <- c(fx3, "4+4x") # note missing multiplication sign
fx5 <- c(fx4, "2,5+1,4x") # note missing multiplication sign
fx6 <- c(fx4, "sin(x)+(x^2)/2")

xmin <- -2
xmax <- 2
xres <- 20
x <- seq(from = xmin, to = xmax, length.out = xres)

test_that("Missing star handled", {
  expect_equal(correct_missing_star("3+4"), "3+4")
  expect_equal(correct_missing_star(fx4)[2], "3+3*x")
  expect_equal(correct_missing_star(fx4)[3], "4+4*x")
})

test_that("Decimal comma handled", {
  expect_equal(correct_decimal_comma(fx5)[4], "2.5+1.4x")
  expect_equal(as.numeric(correct_decimal_comma("1,2")), 12/10)
  expect_equal(as.numeric(correct_decimal_comma("1,")), 1)
})

test_that("Function values", {
  expect_equal(sum(evaluate_functions("3+4", c(-1,0,1))), 21)
  expect_equal(sum(evaluate_functions(fx1,c(-1,0,1))[1,2]), 0)
})

fill_under <- FALSE
fx5 %>% correct_missing_star() %>% correct_decimal_comma() %>% evaluate_functions(x = x) %>% function_graph()
