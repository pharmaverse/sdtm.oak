test_that("`reg_matches()`: basic usage", {
  x <- c("sdtm.oak", "sdtm.cdisc", "adam")
  m <- gregexpr("sdtm", x, fixed = TRUE)

  # `regmatches()` returns `character(0)` for `"adam"`
  # But `reg_matches()` returns `NA` for `"adam"`
  expect_identical(reg_matches(x, m), list("sdtm", "sdtm", NA_character_))
})
