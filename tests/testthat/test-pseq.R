test_that("`pseq()`: scalar inputs", {
  expect_equal(pseq(from = 0, to = 5), 0:5)
})

test_that("`pseq()`: vector inputs", {
  expect_equal(pseq(from = c(0, 10), to = c(5, 15)), c(0:5, 10:15))
})
