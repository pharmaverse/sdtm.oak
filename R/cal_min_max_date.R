#' Calculate minimum and maximum date and time in the data frame
#'
#' @description This function derives the earliest/latest ISO8601 datetime
#'
#' @param raw_dataset Raw source data frame
#' @param date_variable Single character string. Name of the date variable
#' @param time_variable Single character string. Name of the time variable
#' @param val_type Single character string determining whether to look
#'   for the earliest or the latest datetime combination. Permitted values:
#'   "min", "max". Default to "min".
#' @param date_format Format of source date variable
#' @param time_format Format of source time variable
#'
#' @return Data frame with 2 columns: unique patient_number and datetime variable
#'   column storing the earliest/latest datetime.
#'
#' @export
#' @examples
#' EX <- tibble::tribble(
#'   ~patient_number,    ~EX_ST_DT, ~EX_ST_TM,
#'   "001",           "25-04-2022",   "10:20",
#'   "001",           "25-04-2022",   "10:15",
#'   "001",           "25-04-2022",   "10:19",
#'   "002",           "26-05-2022", "UNK:UNK",
#'   "002",           "26-05-2022",   "05:59"
#' )
#'
#' min <- cal_min_max_date(EX,
#'   date_variable = "EX_ST_DT",
#'   time_variable = "EX_ST_TM",
#'   val_type = "min",
#'   date_format = "dd-mmm-yyyy",
#'   time_format = "H:M"
#' )
#'
#' max <- cal_min_max_date(EX,
#'   date_variable = "EX_ST_DT",
#'   time_variable = "EX_ST_TM",
#'   val_type = "max",
#'   date_format = "dd-mmm-yyyy",
#'   time_format = "H:M"
#' )
#'
cal_min_max_date <- function(raw_dataset,
                             date_variable,
                             time_variable,
                             val_type = "min",
                             date_format,
                             time_format) {
  # Check if date parameter is missing or date variable is present in the raw data frame
  date_not_in_data <- is.na(date_variable) ||
    !utils::hasName(raw_dataset, date_variable)

  # Check if time variable is used and present in the raw data frame
  time_not_in_data <- !is.na(time_variable) &&
    !utils::hasName(raw_dataset, time_variable)

  # If date/time variables not present return empty data frame
  if (date_not_in_data || time_not_in_data) {
    # Return Empty data frame with patient_number and datetime columns
    empty_df <- stats::setNames(
      data.frame(matrix(ncol = 2L, nrow = 0L)),
      c("patient_number", "datetime")
    )
    cli::cli_warn(paste(
      "Date variable", date_variable, "or Time variable", time_variable,
      "not present in source data"
    ))
    return(empty_df)
  }

  fin_df <- raw_dataset

  # Time is not used in reference date then use only date
  if (is.na(time_variable)) {
    fin_df$datetime <- create_iso8601(raw_dataset[[date_variable]],
      .format = date_format
    )
  } else {
    # If both date and time variables are present use both date and time
    raw_dataset$date_time <- paste0(
      raw_dataset[[date_variable]],
      raw_dataset[[time_variable]]
    )
    format <- paste0(date_format, time_format)

    fin_df$datetime <- as.character(create_iso8601(raw_dataset$date_time,
      .format = format,
      .na = c(
        "UNK", "NA", "U", "unk",
        "u", "un", "UN"
      )
    ))
  }

  fin_df <- fin_df |>
    dplyr::select(c("patient_number", "datetime")) |>
    unique()

  fin_df <- fin_df |>
    dplyr::mutate(date_time = datetime) |>
    tidyr::separate(
      date_time,
      sep = "-|T|:",
      into = c("year", "month", "day", "hour", "minute"),
      fill = "right",
      extra = "drop"
    ) |>
    list() |>
    stats::setNames("x") |>
    with(replace(x, x == "UNK", NA)) |>
    list() |>
    stats::setNames("x") |>
    with(replace(x, x == "", NA))

  if (val_type == "min") {
    final_df <- fin_df |>
      dplyr::arrange(year, month, day, hour, minute)
  } else {
    final_df <- fin_df |>
      dplyr::arrange(dplyr::desc(year), dplyr::desc(month), dplyr::desc(day), dplyr::desc(hour), dplyr::desc(minute))
  }

  # Keep first appearance in the data frame since it is already sorted
  final_df <- final_df[!duplicated(final_df$patient_number), c("patient_number", "datetime")]

  final_df <- final_df |> dplyr::filter(!is.na(datetime))

  return(final_df)
}
