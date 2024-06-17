test_that("condition_add tags records correctly with single condition", {
  df <- tibble::tibble(x = 1:3, y = letters[1:3])

  cnd_df <- condition_add(dat = df, x > 1)
  expect_true(is_cnd_df(cnd_df))
  expect_equal(get_cnd_df_cnd(cnd_df), c(FALSE, TRUE, TRUE))
  expect_equal(get_cnd_df_cnd_sum(cnd_df), c(n_true = 2, n_false = 1, n_na = 0))
})

test_that("condition_add tags records correctly with multiple conditions", {
  df <- tibble::tibble(x = 1:5, y = c(1.1, 2.2, 3.3, 4.4, 5.5), z = factor(letters[1:5]))

  cnd_df <- condition_add(dat = df, x > 1 & y < 5)
  cnd_df_multiple <- condition_add(dat = df, x > 1, y < 5)
  expect_true(is_cnd_df(cnd_df))
  expect_equal(get_cnd_df_cnd(cnd_df), c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_equal(get_cnd_df_cnd_sum(cnd_df), c(n_true = 3, n_false = 2, n_na = 0))

  expect_equal(get_cnd_df_cnd(cnd_df_multiple), c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_equal(get_cnd_df_cnd_sum(cnd_df_multiple), c(n_true = 3, n_false = 2, n_na = 0))
})

test_that("condition_add handles different data types correctly", {
  df <- tibble::tibble(x = 1:5, y = c(1.1, 2.2, 3.3, 4.4, 5.5), z = letters[1:5], w = factor(letters[1:5]))

  cnd_df <- condition_add(dat = df, x > 2 & y < 5 & z %in% c("c", "d", "e") & w %in% c("c", "d", "e"))
  expect_true(is_cnd_df(cnd_df))
  expect_equal(get_cnd_df_cnd(cnd_df), c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(get_cnd_df_cnd_sum(cnd_df), c(n_true = 2, n_false = 3, n_na = 0))
})

test_that("condition_add does not care about conditions' arguments being named", {
  df <- tibble::tibble(x = 1:5, y = c(1.1, 2.2, 3.3, 4.4, 5.5))

  cnd_df_named <- condition_add(dat = df, cond1 = x > 2, cond2 = y < 5)
  cnd_df_unnamed <- condition_add(dat = df, x > 2, y < 5)

  expect_equal(cnd_df_named, cnd_df_unnamed)
})

test_that("condition_add handles empty data frames", {
  df <- tibble::tibble(x = integer(0), y = character(0))

  cnd_df <- condition_add(dat = df, x > 1)
  expect_true(is_cnd_df(cnd_df))
  expect_equal(nrow(cnd_df), 0)
  expect_equal(get_cnd_df_cnd(cnd_df), logical(0))
  expect_equal(get_cnd_df_cnd_sum(cnd_df), c(n_true = 0, n_false = 0, n_na = 0))
})

test_that("condition_add gives warning if dat is already a conditioned data frame", {
  df <- tibble::tibble(x = 1:3, y = letters[1:3])
  cnd_df <- new_cnd_df(dat = df, cnd = c(FALSE, NA, TRUE))

  expect_warning(condition_add(dat = cnd_df, x > 1), "The previous condition will be replaced by the new one.")
})

test_that("`condition_add()`: `dat` variables take precedence over variables in `.dat2`", {
  df <- tibble::tibble(x = 1:3, y = letters[1:3])
  .dat2_env <- rlang::env(x = 2)
  .dat2_list <- list(x = 2)
  .dat2_df <- tibble::tibble(x = 2)

  cnd_df_env <- condition_add(dat = df, x > 2, .dat2 = .dat2_env)
  cnd_df_list <- condition_add(dat = df, x > 2, .dat2 = .dat2_list)
  cnd_df_df <- condition_add(dat = df, x > 2, .dat2 = .dat2_df)

  expect_equal(get_cnd_df_cnd(cnd_df_env), c(FALSE, FALSE, TRUE))
  expect_equal(get_cnd_df_cnd(cnd_df_list), c(FALSE, FALSE, TRUE))
  expect_equal(get_cnd_df_cnd(cnd_df_df), c(FALSE, FALSE, TRUE))
})

test_that("condition_add handles .dat2 with additional variables", {
  df <- tibble::tibble(x = 1:3, y = letters[1:3])
  .dat2_env <- rlang::env(z = 3, w = 1)
  .dat2_list <- list(z = 3, w = 1)
  .dat2_df <- tibble::tibble(z = 3, w = 1)

  cnd_df_env <- condition_add(dat = df, x > w & x < z, .dat2 = .dat2_env)
  cnd_df_list <- condition_add(dat = df, x > w & x < z, .dat2 = .dat2_list)
  cnd_df_df <- condition_add(dat = df, x > w & x < z, .dat2 = .dat2_df)

  expect_equal(get_cnd_df_cnd(cnd_df_env), c(FALSE, TRUE, FALSE))
  expect_equal(get_cnd_df_cnd(cnd_df_list), c(FALSE, TRUE, FALSE))
  expect_equal(get_cnd_df_cnd(cnd_df_df), c(FALSE, TRUE, FALSE))
})
