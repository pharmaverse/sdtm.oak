test_that("`str_to_anycase()`: basic usage", {
  x <- c("JAN", "feb", "mAr")
  y <- c("[Jj][Aa][Nn]", "[Ff][Ee][Bb]", "[Mm][Aa][Rr]")
  expect_identical(str_to_anycase(x), y)
})
