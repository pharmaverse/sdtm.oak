test_that("new_cnd_df creates conditioned data frame correctly", {
  df <- tibble(x = 1L:3L, y = letters[1L:3L])
  cnd <- c(FALSE, NA, TRUE)
  cnd_df <- new_cnd_df(dat = df, cnd = cnd)

  expect_s3_class(cnd_df, "cnd_df")
  expect_identical(attr(cnd_df, "cnd"), cnd)
  expect_identical(attr(cnd_df, "cnd_sum"), c(n_true = 1L, n_false = 1L, n_na = 1L))
})

test_that("new_cnd_df gives warning if dat is already cnd_df", {
  df <- tibble(x = 1L:3L, y = letters[1L:3L])
  cnd <- c(FALSE, NA, TRUE)
  cnd_df <- new_cnd_df(dat = df, cnd = cnd)

  expect_warning(new_cnd_df(dat = cnd_df, cnd = cnd, .warn = TRUE))
})

test_that("new_cnd_df errors when cnd length doesn't match dat rows", {
  df <- tibble(x = 1L:3L, y = letters[1L:3L])
  cnd <- c(FALSE, TRUE)

  expect_error(new_cnd_df(dat = df, cnd = cnd))
})

test_that("is_cnd_df correctly identifies cnd_df class", {
  df <- tibble(x = 1L:3L, y = letters[1L:3L])
  cnd_df <- new_cnd_df(dat = df, cnd = c(FALSE, NA, TRUE))

  expect_true(is_cnd_df(cnd_df))
  expect_false(is_cnd_df(df))
})

test_that("get_cnd_df_cnd correctly extracts cnd attribute", {
  df <- tibble(x = 1L:3L, y = letters[1L:3L])
  cnd <- c(FALSE, NA, TRUE)
  cnd_df <- new_cnd_df(dat = df, cnd = cnd)

  expect_identical(get_cnd_df_cnd(cnd_df), cnd)
  expect_null(get_cnd_df_cnd(df))
})

test_that("get_cnd_df_cnd_sum correctly extracts cnd_sum attribute", {
  df <- tibble(x = 1L:3L, y = letters[1L:3L])
  cnd <- c(FALSE, NA, TRUE)
  cnd_df <- new_cnd_df(dat = df, cnd = cnd)

  expect_identical(get_cnd_df_cnd_sum(cnd_df), c(n_true = 1L, n_false = 1L, n_na = 1L))
  expect_null(get_cnd_df_cnd_sum(df))
})

test_that("rm_cnd_df correctly removes cnd_df class and attributes", {
  df <- tibble(x = 1L:3L, y = letters[1L:3L])
  cnd <- c(FALSE, NA, TRUE)
  cnd_df <- new_cnd_df(dat = df, cnd = cnd)
  un_cnd_df <- rm_cnd_df(cnd_df)

  expect_false(inherits(un_cnd_df, "cnd_df"))
  expect_null(attr(un_cnd_df, "cnd"))
  expect_null(attr(un_cnd_df, "cnd_sum"))
})

test_that("tbl_sum.cnd_df adds conditioning summary to tibble header", {
  df <- tibble(x = 1L:3L, y = letters[1L:3L])
  cnd <- c(FALSE, NA, TRUE)
  cnd_df <- new_cnd_df(dat = df, cnd = cnd)

  sum_output <- tbl_sum(cnd_df)
  expect_identical(sum_output["Cond. tbl"], c("Cond. tbl" = "1/1/1"))
})

test_that("ctl_new_rowid_pillar.cnd_df customizes row IDs with condition", {
  df <- tibble(x = 1L:3L, y = letters[1L:3L])
  cnd <- c(FALSE, NA, TRUE)
  cnd_df <- new_cnd_df(dat = df, cnd = cnd)

  rowid_pillar <- ctl_new_rowid_pillar(controller = cnd_df, x = cnd_df, width = 10L)

  expect_s3_class(rowid_pillar, "pillar")
  expect_identical(rowid_pillar$data[[1L]]$row_ids, c("1 F", "2 -", "3 T"))
})
