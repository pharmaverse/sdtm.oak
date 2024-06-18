`%>%` <- magrittr::`%>%`

test_that("Basic operations work correctly", {
  expect_identical(5L %.>% (2L + .), 5L %>%
    {
      2L + .
    })
  expect_identical("hello" %.>% toupper(.), "hello" %>%
    {
      toupper(.)
    })
})

test_that("Nested operations work correctly", {
  expect_identical(5L %.>% (2L + . + 3L), 5L %>%
    {
      2L + . + 3L
    })
  expect_identical("hello" %.>% paste(., "world"), "hello" %>%
    {
      paste(., "world")
    })
})

test_that("Piping with braces", {
  mtcars2 <- mtcars %.>% {
    .$cyl <- .$cyl * 2L
    .
  }
  expect_identical(mtcars2$cyl, mtcars$cyl * 2L)
})

test_that("Dot used multiple times in rhs", {
  expect_identical(5L %.>% (. * 2L + .), 5L %>%
    {
      . * 2L + .
    })
  expect_identical("hello" %.>% paste(., toupper(.)), "hello" %>%
    {
      paste(., toupper(.))
    })
})

test_that("Dot used in nested functions", {
  expect_identical(mtcars %.>% subset(., seq_len(nrow(.)) %% 2L == 0L), mtcars %>%
    {
      subset(., seq_len(nrow(.)) %% 2L == 0L)
    })
  expect_identical(1L:10L %.>% c(min(.), max(.)), 1L:10L %>%
    {
      c(min(.), max(.))
    })
})

test_that("Error when dot is not used in rhs", {
  expect_error(5L %.>% (2L + 2L))
  expect_error("hello" %.>% toupper)
})

test_that("Complex expressions work correctly", {
  expect_identical(5L %.>% (2L + . + 3L + . * 2L), 5L %>%
    {
      2L + . + 3L + . * 2L
    })
  expect_identical(mtcars %.>% subset(., gear == 4L & mpg > mean(mpg)), mtcars %>%
    {
      subset(., gear == 4L & mpg > mean(mpg))
    })
  expect_identical(mtcars %.>% subset(., cyl == 6L) %.>% nrow(.), mtcars %>%
    {
      subset(., cyl == 6L)
    } %>%
    nrow())
})

test_that("Functions returning functions", {
  expect_identical(1L:5L %.>% (sapply(., function(x) x * 2L)), 1L:5L %>%
    {
      sapply(., function(x) x * 2L)
    })
  expect_identical(mtcars %.>% (apply(., 2L, function(x) mean(x))), mtcars %>%
    {
      apply(., 2L, function(x) mean(x))
    })
})

test_that("Dot used in custom functions", {
  custom_function <- function(x) {
    x + 1L
  }
  expect_identical(5L %.>% custom_function(.), 5L %>%
    {
      custom_function(.)
    })
  expect_identical(mtcars %.>% head(.), mtcars %>%
    {
      head(.)
    })
})

test_that("Anonymous functions with \\(x)", {
  expect_identical(1L:5L %.>% (purrr::map(., \(x) x * 2L)), 1L:5L %>%
    {
      purrr::map(., \(x) x * 2L)
    })
})

test_that("Anonymous functions with function(x)", {
  expect_identical(1L:5L %.>% (purrr::map(., function(x) x * 2L)), 1L:5L %>%
    {
      purrr::map(., function(x) x * 2L)
    })
})

test_that("Piping with environment-dependent functions", {
  env <- environment()
  "x" %.>% assign(x = ., 100L, envir = env)
  expect_identical(x, 100L)
})

test_that("`.` is restored", {
  1L %.>% identity(.)
  expect_error(., "not found")

  . <- "foo"
  1L %.>% identity(.)
  expect_identical(., "foo")
})

# TODO: Support for lazy-evaluation
