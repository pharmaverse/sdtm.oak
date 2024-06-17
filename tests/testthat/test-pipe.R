`%>%` <- magrittr::`%>%`

test_that("Basic operations work correctly", {
  expect_equal(5 %.>% (2 + .), 5 %>% {2 + .})
  expect_equal("hello" %.>% toupper(.), "hello" %>% {toupper(.)})
})

test_that("Nested operations work correctly", {
  expect_equal(5 %.>% (2 + . + 3), 5 %>% {2 + . + 3})
  expect_equal("hello" %.>% paste(., "world"), "hello" %>% {paste(., "world")})
})

test_that("Piping with braces", {
  mtcars2 <- mtcars %.>% {.$cyl <- .$cyl * 2; .}
  expect_equal(mtcars2$cyl, mtcars$cyl * 2)
})

test_that("Dot used multiple times in rhs", {
  expect_equal(5 %.>% (. * 2 + .), 5 %>% { . * 2 + . })
  expect_equal("hello" %.>% paste(., toupper(.)), "hello" %>% {paste(., toupper(.))})
})

test_that("Dot used in nested functions", {
  expect_equal(mtcars %.>% subset(., 1:nrow(.) %% 2 == 0), mtcars %>% {subset(., 1:nrow(.) %% 2 == 0)})
  expect_equal(1:10 %.>% c(min(.), max(.)), 1:10 %>% {c(min(.), max(.))})
})

test_that("Error when dot is not used in rhs", {
  expect_error(5 %.>% (2 + 2))
  expect_error("hello" %.>% toupper)
})

test_that("Complex expressions work correctly", {
  expect_equal(5 %.>% (2 + . + 3 + . * 2), 5 %>% {2 + . + 3 + . * 2})
  expect_equal(mtcars %.>% subset(., gear == 4 & mpg > mean(mpg)), mtcars %>% {subset(., gear == 4 & mpg > mean(mpg))})
  expect_equal(mtcars %.>% subset(., cyl == 6) %.>% nrow(.), mtcars %>% {subset(., cyl == 6)} %>% nrow())
})

test_that("Functions returning functions", {
  expect_equal(1:5 %.>% (sapply(., function(x) x * 2)), 1:5 %>% {sapply(., function(x) x * 2)})
  expect_equal(mtcars %.>% (apply(., 2, function(x) mean(x))), mtcars %>% {apply(., 2, function(x) mean(x))})
})

test_that("Dot used in custom functions", {
  custom_function <- function(x) { x + 1 }
  expect_equal(5 %.>% custom_function(.), 5 %>% {custom_function(.)})
  expect_equal(mtcars %.>% head(.), mtcars %>% {head(.)})
})

test_that("Anonymous functions with \\(x)", {
  expect_equal(1:5 %.>% (purrr::map(., \(x) x * 2)), 1:5 %>% {purrr::map(., \(x) x * 2)})
})

test_that("Anonymous functions with function(x)", {
  expect_equal(1:5 %.>% (purrr::map(., function(x) x * 2)), 1:5 %>% {purrr::map(., function(x) x * 2)})
})

test_that("Piping with environment-dependent functions", {
  env <- environment()
  "x" %.>% assign(x = ., 100, envir = env)
  expect_equal(x, 100)
})

test_that("`.` is restored", {
  1 %.>% identity(.)
  expect_error(., "not found")

  . <- "foo"
  1 %.>% identity(.)
  expect_identical(., "foo")
})

# TODO: Support for lazy-evaluation
