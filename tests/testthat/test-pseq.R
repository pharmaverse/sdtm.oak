test_that("`pseq()`: scalar inputs", {
  expect_identical(pseq(from = 0L, to = 5L), 0L:5L)
})

test_that("`pseq()`: vector inputs", {
  expect_identical(pseq(from = c(0L, 10L), to = c(5L, 15L)), c(0L:5L, 10L:15L))
})
