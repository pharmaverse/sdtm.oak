test_that("`eval_conditions()` evaluates conditions correctly", {
  df <- tibble::tibble(
    x = c(1, 2, NA_integer_, 4, 5),
    y = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    z = c("a", "b", "a", "b", "a")
  )

  # Tag records for which `x` is greater than 2.
  expect_equal(sdtm.oak:::eval_conditions(df, x > 2),
               c(FALSE, FALSE, NA, TRUE, TRUE))

  # Tag records for which `x` is greater than 2 and `y` is TRUE.
  expect_equal(sdtm.oak:::eval_conditions(df, x > 2, y),
               c(FALSE, FALSE, NA, FALSE, TRUE))

  # Tag records for which `x` is greater than 2 and convert resulting NAs into FALSE.
  expect_equal(sdtm.oak:::eval_conditions(df, x > 2, .na = FALSE),
               c(FALSE, FALSE, FALSE, TRUE, TRUE))

  # Conditions may involve variables defined in the caller environment.
  w <- 1
  expect_equal(sdtm.oak:::eval_conditions(df, x > w),
               c(FALSE, TRUE, NA, TRUE, TRUE))

  # Conditions may look into variables defined in other scopes (e.g., in
  # environments).
  env <- rlang::env(w = 1)
  expect_equal(sdtm.oak:::eval_conditions(df, x > w, .env = env),
               c(FALSE, TRUE, NA, TRUE, TRUE))

  # Other scopes are not restricted to environments but lists and tibbles also
  # work as namespaces for look-up.
  expect_equal(sdtm.oak:::eval_conditions(df, x > w, .env = list(w = 3)),
               c(FALSE, FALSE, NA, TRUE, TRUE))
  expect_equal(
    sdtm.oak:::eval_conditions(df, x > w, .env = tibble::tibble(w = 4)),
    c(FALSE, FALSE, NA, FALSE, TRUE)
  )
})
