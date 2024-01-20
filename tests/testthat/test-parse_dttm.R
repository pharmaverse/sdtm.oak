test_that("`months_abb_regex()`: default behavior (case insensitive)", {
  x <- paste0(
    "[Jj][Aa][Nn]|",
    "[Ff][Ee][Bb]|",
    "[Mm][Aa][Rr]|",
    "[Aa][Pp][Rr]|",
    "[Mm][Aa][Yy]|",
    "[Jj][Uu][Nn]|",
    "[Jj][Uu][Ll]|",
    "[Aa][Uu][Gg]|",
    "[Ss][Ee][Pp]|",
    "[Oo][Cc][Tt]|",
    "[Nn][Oo][Vv]|",
    "[Dd][Ee][Cc]"
  )
  expect_identical(months_abb_regex(), x)
})

test_that("`months_abb_regex()`: uppercase", {
  x <- paste0(
    "JAN|",
    "FEB|",
    "MAR|",
    "APR|",
    "MAY|",
    "JUN|",
    "JUL|",
    "AUG|",
    "SEP|",
    "OCT|",
    "NOV|",
    "DEC"
  )
  expect_identical(months_abb_regex(case = "upper"), x)
})

test_that("`months_abb_regex()`: lowercase", {
  x <- paste0(
    "jan|",
    "feb|",
    "mar|",
    "apr|",
    "may|",
    "jun|",
    "jul|",
    "aug|",
    "sep|",
    "oct|",
    "nov|",
    "dec"
  )
  expect_identical(months_abb_regex(case = "lower"), x)
})
