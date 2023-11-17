## code to prepare `dtc_formats` dataset goes here

dtc_formats <- tibble::tribble(
  ~fmt, ~type, ~description,
  "ymd", "date", "Parses a date: year, month, and month day.",
  "y m d", "date", "Parses a date: year, month, and month day.",
  "y-m-d", "date", "Parses a date: year, month, and month day.",
  "dmy", "date", "Parses a date: month day, month and year.",
  "d m y", "date", "Parses a date: month day, month and year.",
  "d-m-y", "date", "Parses a date: month day, month and year.",
  "ym", "date", "Parses a date: year and month.",
  "y m", "date", "Parses a date: year and month.",
  "y-m", "date", "Parses a date: year and month.",
  "my", "date", "Parses a date: month and year.",
  "m y", "date", "Parses a date: month and year.",
  "m-y", "date", "Parses a date: month and year.",
  "HM", "time", "Parses a time: hour and minutes.",
  "HMS", "time", "Parses a time: hour, minutes, and seconds.",
  "H:M", "time", "Parses a time: hour and minutes.",
  "H:M:S", "time", "Parses a time: hour, minutes and seconds.",
  "ymdH:M:S", "datetime", "Parses a date-time: year, month, month day, hour, minutes, and seconds.",
  "ymd H:M:S", "datetime", "Parses a date-time: year, month, month day, hour, minutes, and seconds.",
  "y-m-d H:M:S", "datetime", "Parses a date-time: year, month, month day, hour, minutes, and seconds.",
  "y m d H:M:S", "datetime", "Parses a date-time: year, month, month day, hour, minutes, and seconds."
)

usethis::use_data(dtc_formats, overwrite = TRUE)
