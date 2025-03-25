path <- here::here("inst")
dm <- readRDS(file.path(path, "/domain/dm.rds"))[1L:5L, ]
spec <- read.csv(file.path(path, "/spec/suppqual_spec.csv"))

test_that("`gen_sdtm_supp` works as expected", {
  final <-
    gen_sdtm_supp(
      dm,
      idvar = NULL,
      spec = spec,
      qnam = "Variable",
      label_var = "Label",
      orig_var = "Origin"
    )

  expect_snapshot(jsonlite::toJSON(final, pretty = TRUE, auto_unbox = TRUE))
})

test_that("`gen_sdtm_supp` input validation works", {
  expect_snapshot_error(
    gen_sdtm_supp(
      dm,
      idvar = 123L,
      spec = spec,
      qnam = "Variable",
      label_var = "Label",
      orig_var = "Origin"
    )
  )

  expect_snapshot_error(
    gen_sdtm_supp(
      dm,
      idvar = NULL,
      spec = "abc",
      qnam = "Variable",
      label_var = "Label",
      orig_var = "Origin"
    )
  )

  expect_snapshot_error(
    gen_sdtm_supp(
      dm,
      idvar = NULL,
      spec = spec,
      qnam = "Var",
      label_var = "Label",
      orig_var = "Origin"
    )
  )

  expect_snapshot_error(
    gen_sdtm_supp(
      dm,
      idvar = NULL,
      spec = spec,
      qnam = "Variable",
      label_var = c("label", "qlabel"),
      orig_var = "Origin"
    )
  )

  expect_snapshot_error(
    gen_sdtm_supp(
      dm |> dplyr::select(-COMPLT16),
      idvar = NULL,
      spec = spec,
      qnam = "Variable",
      label_var = "Label",
      orig_var = "Origin"
    )
  )
})
