test_that("`parse_dttm()` works as expected", {
  # Year
  expected_y <- tibble::tribble(
    ~year, ~mon, ~mday, ~hour, ~min, ~sec,
    "2020",  NA,    NA,    NA,   NA,   NA
  ) |>
    as.matrix()

  expect_identical(
    parse_dttm("2020", "y"),
    expected_y
  )

  # Year, Month
  expected_ym <- tibble::tribble(
    ~year,  ~mon, ~mday, ~hour, ~min, ~sec,
    "2020", "05",    NA,    NA,   NA,   NA
  ) |>
    as.matrix()

  expect_identical(
    parse_dttm("2020-05", "y-m"),
    expected_ym
  )

  # Year, Month, Day
  expected_ymd <- tibble::tribble(
    ~year,  ~mon, ~mday, ~hour, ~min, ~sec,
    "2020", "05",  "11",    NA,   NA,   NA
  ) |>
    as.matrix()

  expect_identical(
    parse_dttm("2020-05-11", "y-m-d"),
    expected_ymd
  )

  # Year, Month, Day in other formats
  parse_dttm("2020 05 11", "y m d") |> expect_identical(expected_ymd)
  parse_dttm("2020  05  11", "y\\s+m\\s+d") |> expect_identical(expected_ymd)
  parse_dttm("2020      05     11", "y\\s+m\\s+d") |> expect_identical(expected_ymd)

  # Year, Month, Day, Hour, Minute
  expected_ymdhm <- tibble::tribble(
    ~year, ~mon, ~mday, ~hour, ~min, ~sec,
    "2020", "05", "11", "11", "45", NA
  ) |>
    as.matrix()

  expect_identical(
    parse_dttm("2020-05-11 11:45", "y-m-d H:M"),
    expected_ymdhm
  )

  # Year, Month, Day, Hour, Minute, Second
  expected_ymdhms <- tibble::tribble(
    ~year, ~mon, ~mday, ~hour, ~min, ~sec,
    "2020", "05", "11", "11", "45", "15.6"
  ) |>
    as.matrix()

  expect_identical(
    parse_dttm("2020-05-11 11:45:15.6", "y-m-d H:M:S"),
    expected_ymdhms
  )

  # Multiple records
  expected_ymdhm_1 <- tibble::tribble(
    ~year, ~mon, ~mday, ~hour, ~min, ~sec,
    "2002", "05", "11", "11", "45", NA,
    NA, NA, NA, NA, NA, NA
  ) |>
    as.matrix()

  expect_identical(
    parse_dttm(c("2002-05-11 11:45", "-05-11 11:45"), "y-m-d H:M"),
    expected_ymdhm_1
  )

  expected_ymdhm_2 <- tibble::tribble(
    ~year, ~mon, ~mday, ~hour, ~min, ~sec,
    NA, NA, NA, NA, NA, NA,
    NA, "05", "11", "11", "45", NA
  ) |>
    as.matrix()

  expect_identical(
    parse_dttm(c("2002-05-11 11:45", "-05-11 11:45"), "-m-d H:M"),
    expected_ymdhm_2
  )



  expected_ymdhm_3 <- tibble::tribble(
    ~year, ~mon, ~mday, ~hour, ~min, ~sec,
    "2002", "05", "11", "11", "45", NA,
    NA, "05", "11", "11", "45", NA
  ) |>
    as.matrix()

  expect_identical(
    parse_dttm(c("2002-05-11 11:45", "-05-11 11:45"), c("y-m-d H:M", "-m-d H:M")),
    expected_ymdhm_3
  )

  # Different date formats
  expected_ymdhm_4 <- tibble::tribble(
    ~year,   ~mon, ~mday, ~hour, ~min,  ~sec,
    "1985", "feb",  "05",  "12", "55",  "02"
  ) |>
    as.matrix()

  expect_identical(
    parse_dttm("05 feb 1985 12 55 02", "d m y H M S"),
    expected_ymdhm_4
  )

  expect_identical(
    parse_dttm("12 55 02 05 feb 1985", "H M S d m y"),
    expected_ymdhm_4
  )

  # UNK included
  expected_unk <- tibble::tribble(
    ~year,   ~mon, ~mday, ~hour, ~min,  ~sec,
    "2020",  "05",  "18",    NA,   NA,    NA,
    NA,        NA,    NA,    NA,   NA,    NA,
    NA,        NA,    NA,    NA,   NA,    NA
  ) |>
    as.matrix()

  expect_identical(
    parse_dttm(c("2020-05-18", "2020-UN-18", "2020-UNK-UN"), "y-m-d"),
    expected_unk
  )
  expected_unk_1 <- tibble::tribble(
    ~year,   ~mon, ~mday, ~hour, ~min,  ~sec,
    "2020",  "05",  "18",    NA,   NA,    NA,
    "2020",  "UN",  "18",    NA,   NA,    NA,
    NA,        NA,    NA,    NA,   NA,    NA
  ) |>
    as.matrix()

  expect_identical(
    parse_dttm(c("2020-05-18", "2020-UN-18", "2020-UNK-UN"),
      "y-m-d",
      na = "UN"
    ),
    expected_unk_1
  )

  expected_unk_2 <- tibble::tribble(
    ~year,   ~mon, ~mday, ~hour, ~min,  ~sec,
    "2020",  "05",  "18",    NA,   NA,    NA,
    "2020",  "UN",  "18",    NA,   NA,    NA,
    "2020", "UNK",  "UN",    NA,   NA,    NA
  ) |>
    as.matrix()

  expect_identical(
    parse_dttm(c("2020-05-18", "2020-UN-18", "2020-UNK-UN"),
      "y-m-d",
      na = c("UN", "UNK")
    ),
    expected_unk_2
  )
})
