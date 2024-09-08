#' Populate RFSTDTC variable in demographic domain in ISO8601 character format.
#'
#' Derive RFSTDTC based on the configuration file.
#'
#' @details
#'
#' Calculate minimum or maximum dates

cal_min_max_date <- function(raw_dataset,
                              sdtm_var_name,
                              date_variable,
                              val_type = "min"
) {
  browser()
  # Check if date and time variable are present in the raw dataset
  date_not_in_data <- !(date_variable %in% colnames(raw_dataset))

  if (date_not_in_data) {
    # Return Empty Dataset with SUBJID and sdtm_var_name
    final_df <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("patient_number", "datetime"))
    return(final_df)
  }

  final_df <- raw_dataset

  final_df$datetime <- create_iso8601(raw_dataset[[date_variable]], .format = "dd-mm-yyyy")

  final_df <- final_df |>
    dplyr::select(c("SUBJID", "datetime"))|> unique()

  final_df <- final_df |>
    dplyr::mutate(date_time = datetime) |>
    tidyr::separate(
      date_time,
      sep = "-|T|:",
      into = c("year", "month", "day"), #, "hour", "minute"
      fill = "right",
      extra = "drop"
    )|>
    list() |>
    setNames("x") |>
    with(replace(x, x == "UNK", NA)) |>
    list() |>
    setNames("x") |>
    with(replace(x, x == "", NA))


  if (val_type == "min") {
    final_df1 <- final_df |>
      dplyr::arrange(year, month, day)#, hour, minute)
  } else {
    final_df1 <- final_df |>
      dplyr::arrange(dplyr::desc(year), dplyr::desc(month), dplyr::desc(day))#, dplyr::desc(hour), dplyr::desc(minute))
  }

  # Keep first appearance in the data frame since it is already sorted
  final_df2 <- final_df1[!duplicated(final_df1$SUBJID), c("SUBJID", "datetime")]

  final_df2 <- final_df2 |> dplyr::filter(!is.na(datetime))

  # colnames(final_df2)[colnames(final_df2) == "datetime"] <- sdtm_var_name

  return(final_df2)
}
