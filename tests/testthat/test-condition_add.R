test_that("condition_add tags records correctly with single condition", {
  df <- tibble::tibble(x = 1L:3L, y = letters[1L:3L])

  cnd_df <- condition_add(dat = df, x > 1L)
  expect_true(is_cnd_df(cnd_df))
  expect_identical(get_cnd_df_cnd(cnd_df), c(FALSE, TRUE, TRUE))
  expect_identical(get_cnd_df_cnd_sum(cnd_df), c(n_true = 2L, n_false = 1L, n_na = 0L))
})

test_that("condition_add tags records correctly with multiple conditions", {
  df <- tibble::tibble(x = 1L:5L, y = c(1.1, 2.2, 3.3, 4.4, 5.5), z = factor(letters[1L:5L]))

  cnd_df <- condition_add(dat = df, x > 1L & y < 5.0)
  cnd_df_multiple <- condition_add(dat = df, x > 1L, y < 5.0)
  expect_true(is_cnd_df(cnd_df))
  expect_identical(get_cnd_df_cnd(cnd_df), c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_identical(get_cnd_df_cnd_sum(cnd_df), c(n_true = 3L, n_false = 2L, n_na = 0L))

  expect_identical(get_cnd_df_cnd(cnd_df_multiple), c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_identical(get_cnd_df_cnd_sum(cnd_df_multiple), c(n_true = 3L, n_false = 2L, n_na = 0L))
})

test_that("condition_add handles different data types correctly", {
  df <- tibble::tibble(x = 1L:5L, y = c(1.1, 2.2, 3.3, 4.4, 5.5), z = letters[1L:5L], w = factor(letters[1L:5L]))

  cnd_df <- condition_add(dat = df, x > 2L & y < 5.0 & z %in% c("c", "d", "e") & w %in% c("c", "d", "e"))
  expect_true(is_cnd_df(cnd_df))
  expect_identical(get_cnd_df_cnd(cnd_df), c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(get_cnd_df_cnd_sum(cnd_df), c(n_true = 2L, n_false = 3L, n_na = 0L))
})

test_that("condition_add does not care about conditions' arguments being named", {
  df <- tibble::tibble(x = 1L:5L, y = c(1.1, 2.2, 3.3, 4.4, 5.5))

  cnd_df_named <- condition_add(dat = df, cond1 = x > 2L, cond2 = y < 5.0)
  cnd_df_unnamed <- condition_add(dat = df, x > 2L, y < 5.0)

  expect_identical(cnd_df_named, cnd_df_unnamed)
})

test_that("condition_add handles empty data frames", {
  df <- tibble::tibble(x = integer(0L), y = character(0L))

  cnd_df <- condition_add(dat = df, x > 1L)
  expect_true(is_cnd_df(cnd_df))
  expect_identical(nrow(cnd_df), 0L)
  expect_identical(get_cnd_df_cnd(cnd_df), logical(0L))
  expect_identical(get_cnd_df_cnd_sum(cnd_df), c(n_true = 0L, n_false = 0L, n_na = 0L))
})

test_that("condition_add gives warning if dat is already a conditioned data frame", {
  df <- tibble::tibble(x = 1L:3L, y = letters[1L:3L])
  cnd_df <- new_cnd_df(dat = df, cnd = c(FALSE, NA, TRUE))

  expect_warning(condition_add(dat = cnd_df, x > 1L), "The previous condition will be replaced by the new one.")
})

test_that("`condition_add()`: `dat` variables take precedence over variables in `.dat2`", {
  df <- tibble::tibble(x = 1L:3L, y = letters[1L:3L])
  .dat2_env <- rlang::env(x = 2L)
  .dat2_list <- list(x = 2L)
  .dat2_df <- tibble::tibble(x = 2L)

  cnd_df_env <- condition_add(dat = df, x > 2L, .dat2 = .dat2_env)
  cnd_df_list <- condition_add(dat = df, x > 2L, .dat2 = .dat2_list)
  cnd_df_df <- condition_add(dat = df, x > 2L, .dat2 = .dat2_df)

  expect_identical(get_cnd_df_cnd(cnd_df_env), c(FALSE, FALSE, TRUE))
  expect_identical(get_cnd_df_cnd(cnd_df_list), c(FALSE, FALSE, TRUE))
  expect_identical(get_cnd_df_cnd(cnd_df_df), c(FALSE, FALSE, TRUE))
})

test_that("condition_add handles .dat2 with additional variables", {
  df <- tibble::tibble(x = 1L:3L, y = letters[1L:3L])
  .dat2_env <- rlang::env(z = 3L, w = 1L)
  .dat2_list <- list(z = 3L, w = 1L)
  .dat2_df <- tibble::tibble(z = 3L, w = 1L)

  cnd_df_env <- condition_add(dat = df, x > w & x < z, .dat2 = .dat2_env)
  cnd_df_list <- condition_add(dat = df, x > w & x < z, .dat2 = .dat2_list)
  cnd_df_df <- condition_add(dat = df, x > w & x < z, .dat2 = .dat2_df)

  expect_identical(get_cnd_df_cnd(cnd_df_env), c(FALSE, TRUE, FALSE))
  expect_identical(get_cnd_df_cnd(cnd_df_list), c(FALSE, TRUE, FALSE))
  expect_identical(get_cnd_df_cnd(cnd_df_df), c(FALSE, TRUE, FALSE))
})
