test_that("mutate.cnd_df correctly mutates conditioned data frame", {
  df <- tibble::tibble(x = 1:3, y = letters[1:3])
  cnd_df <- new_cnd_df(dat = df, cnd = c(FALSE, NA, TRUE))

  mutated_df <- dplyr::mutate(cnd_df, z = x + 1)
  expect_true("z" %in% colnames(mutated_df))
  expect_equal(mutated_df$z, c(NA, NA, 4))
})

test_that("mutate.cnd_df handles multiple mutations", {
  df <- tibble::tibble(x = 1:3, y = 1:3)
  cnd_df <- new_cnd_df(dat = df, cnd = c(TRUE, FALSE, TRUE))

  mutated_df <- dplyr::mutate(cnd_df, z = x + y, w = x * y)
  expect_true(all(c("z", "w") %in% colnames(mutated_df)))
  expect_equal(mutated_df$z, c(2, NA, 6))
  expect_equal(mutated_df$w, c(1, NA, 9))
})

test_that("mutate.cnd_df retains original data for non-conditioned rows", {
  df <- tibble::tibble(x = 1:4, y = 2:5)
  cnd_df <- new_cnd_df(dat = df, cnd = c(TRUE, FALSE, TRUE, NA))

  mutated_df <- dplyr::mutate(cnd_df, z = x + y)
  expect_equal(mutated_df$z, c(3, NA, 7, NA))
  expect_equal(mutated_df$x, df$x)
  expect_equal(mutated_df$y, df$y)
})

test_that("mutate.cnd_df works with different data types", {
  df <- tibble::tibble(x = 1:3, y = c(1.1, 2.2, 3.3), z = c("a", "b", "c"))
  cnd_df <- new_cnd_df(dat = df, cnd = c(TRUE, FALSE, TRUE))

  mutated_df <- dplyr::mutate(cnd_df, w = x * y, v = paste0(z, x))
  expect_true(all(c("w", "v") %in% colnames(mutated_df)))
  expect_equal(mutated_df$w, c(1.1, NA, 9.9))
  expect_equal(mutated_df$v, c("a1", NA, "c3"))
})

test_that("mutate.cnd_df handles empty data frames", {
  df <- tibble::tibble(x = integer(0), y = integer(0))
  cnd_df <- new_cnd_df(dat = df, cnd = logical(0))

  mutated_df <- dplyr::mutate(cnd_df, z = x + y)
  expect_equal(nrow(mutated_df), 0)
  expect_true("z" %in% colnames(mutated_df))
  expect_equal(mutated_df$z, numeric(0))
})

test_that("mutate.cnd_df handles .keep parameter correctly", {
  df <- tibble::tibble(x = 1:3, y = 1:3)
  cnd_df <- new_cnd_df(dat = df, cnd = c(TRUE, FALSE, TRUE))

  mutated_df_all <- dplyr::mutate(cnd_df, z = x + y, .keep = "all")
  expect_true(all(c("x", "y", "z") %in% colnames(mutated_df_all)))

  mutated_df_used <- dplyr::mutate(cnd_df, z = 2 * x, .keep = "used")
  expect_true(all(c("x", "z") %in% colnames(mutated_df_used)))

  mutated_df_unused <- dplyr::mutate(cnd_df, z = 2 * x, .keep = "unused")
  expect_true(all(c("y", "z") %in% colnames(mutated_df_unused)))

  mutated_df_none <- dplyr::mutate(cnd_df, z = x + y, .keep = "none")
  expect_true("z" == colnames(mutated_df_none))
  expect_false(any(c("x", "y") %in% colnames(mutated_df_none)))
})

test_that("mutate.cnd_df handles .after parameter correctly", {
  df <- tibble::tibble(x = 1:3, y = 1:3)
  cnd_df <- new_cnd_df(dat = df, cnd = c(TRUE, FALSE, TRUE))

  mutated_df_after <- dplyr::mutate(cnd_df, z = x + y, .after = "x")
  expect_equal(colnames(mutated_df_after), c("x", "z", "y"))
})

test_that("mutate.cnd_df works with named arguments", {
  df <- tibble::tibble(x = 1:3, y = 1:3)
  cnd_df <- new_cnd_df(dat = df, cnd = c(TRUE, FALSE, TRUE))

  mutated_df_named <- dplyr::mutate(cnd_df, new_col = x + y)
  expect_true("new_col" %in% colnames(mutated_df_named))
  expect_equal(mutated_df_named$new_col, c(2, NA, 6))
})

test_that("mutate.cnd_df errors when .by is used", {
  df <- tibble::tibble(x = 1:3, y = 1:3)
  cnd_df <- new_cnd_df(dat = df, cnd = c(TRUE, FALSE, TRUE))

  expect_error(
    dplyr::mutate(cnd_df, z = x + y, .by = "y"),
    regex = "`\\.by` is not supported on conditioned data frames."
  )
})

test_that("mutate.cnd_df errors when .before is used", {
  df <- tibble::tibble(x = 1:3, y = 1:3)
  cnd_df <- new_cnd_df(dat = df, cnd = c(TRUE, FALSE, TRUE))

  expect_error(
    dplyr::mutate(cnd_df, z = x + y, .before = "x"),
    regex = "`\\.before` is not supported on conditioned data frames, use `\\.after` instead."
  )
})
