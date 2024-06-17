test_that("new_cnd_df creates conditioned data frame correctly", {
  df <- tibble(x = 1:3, y = letters[1:3])
  cnd <- c(FALSE, NA, TRUE)
  cnd_df <- sdtm.oak:::new_cnd_df(dat = df, cnd = cnd)

  expect_true(inherits(cnd_df, "cnd_df"))
  expect_equal(attr(cnd_df, "cnd"), cnd)
  expect_equal(attr(cnd_df, "cnd_sum"), c(n_true = 1, n_false = 1, n_na = 1))
})

test_that("new_cnd_df gives warning if dat is already cnd_df", {
  df <- tibble(x = 1:3, y = letters[1:3])
  cnd <- c(FALSE, NA, TRUE)
  cnd_df <- sdtm.oak:::new_cnd_df(dat = df, cnd = cnd)

  expect_warning(sdtm.oak:::new_cnd_df(dat = cnd_df, cnd = cnd, .warn = TRUE))
})

test_that("new_cnd_df errors when cnd length doesn't match dat rows", {
  df <- tibble(x = 1:3, y = letters[1:3])
  cnd <- c(FALSE, TRUE)

  expect_error(sdtm.oak:::new_cnd_df(dat = df, cnd = cnd))
})

test_that("is_cnd_df correctly identifies cnd_df class", {
  df <- tibble(x = 1:3, y = letters[1:3])
  cnd_df <- sdtm.oak:::new_cnd_df(dat = df, cnd = c(FALSE, NA, TRUE))

  expect_true(sdtm.oak:::is_cnd_df(cnd_df))
  expect_false(sdtm.oak:::is_cnd_df(df))
})

test_that("get_cnd_df_cnd correctly extracts cnd attribute", {
  df <- tibble(x = 1:3, y = letters[1:3])
  cnd <- c(FALSE, NA, TRUE)
  cnd_df <- sdtm.oak:::new_cnd_df(dat = df, cnd = cnd)

  expect_equal(sdtm.oak:::get_cnd_df_cnd(cnd_df), cnd)
  expect_null(sdtm.oak:::get_cnd_df_cnd(df))
})

test_that("get_cnd_df_cnd_sum correctly extracts cnd_sum attribute", {
  df <- tibble(x = 1:3, y = letters[1:3])
  cnd <- c(FALSE, NA, TRUE)
  cnd_df <- sdtm.oak:::new_cnd_df(dat = df, cnd = cnd)

  expect_equal(sdtm.oak:::get_cnd_df_cnd_sum(cnd_df), c(n_true = 1, n_false = 1, n_na = 1))
  expect_null(sdtm.oak:::get_cnd_df_cnd_sum(df))
})

test_that("rm_cnd_df correctly removes cnd_df class and attributes", {
  df <- tibble(x = 1:3, y = letters[1:3])
  cnd <- c(FALSE, NA, TRUE)
  cnd_df <- sdtm.oak:::new_cnd_df(dat = df, cnd = cnd)
  un_cnd_df <- sdtm.oak:::rm_cnd_df(cnd_df)

  expect_false(inherits(un_cnd_df, "cnd_df"))
  expect_null(attr(un_cnd_df, "cnd"))
  expect_null(attr(un_cnd_df, "cnd_sum"))
})

test_that("tbl_sum.cnd_df adds conditioning summary to tibble header", {
  df <- tibble(x = 1:3, y = letters[1:3])
  cnd <- c(FALSE, NA, TRUE)
  cnd_df <- sdtm.oak:::new_cnd_df(dat = df, cnd = cnd)

  sum_output <- tbl_sum(cnd_df)
  expect_equal(sum_output["Cond. tbl"], c("Cond. tbl" = "1/1/1"))
})

test_that("ctl_new_rowid_pillar.cnd_df customizes row IDs with condition", {
  df <- tibble(x = 1:3, y = letters[1:3])
  cnd <- c(FALSE, NA, TRUE)
  cnd_df <- sdtm.oak:::new_cnd_df(dat = df, cnd = cnd)

  rowid_pillar <- ctl_new_rowid_pillar(controller = cnd_df, x = cnd_df, width = 10)

  expect_true(inherits(rowid_pillar, "pillar"))
  expect_equal(rowid_pillar$data[[1]]$row_ids, c("1 F", "2 -", "3 T"))
})
