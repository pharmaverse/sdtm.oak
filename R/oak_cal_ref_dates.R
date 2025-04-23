#' Calculate Reference dates in ISO8601 character format.
#'
#' Populate Reference date variables in demographic domain in ISO8601 character format.
#'
#' @description Derive RFSTDTC, RFENDTC, RFXENDTC, RFXSTDTC, etc. based on the input dates and time.
#'
#'
#' @param ds_in Data frame. DM domain.
#' @param der_var Character string. The reference date to be derived.
#' @param min_max Minimum or Maximum date to be calculated based on the input.
#'        Default set to Minimum. Values should be min or max.
#' @param ref_date_config_df Data frame which has the details of the variables to
#'        be used for the calculation of reference dates.
#'        Should have columns listed below:
#'          raw_dataset_name : Name of the raw dataset.
#'          date_var : Date variable name from the raw dataset.
#'          time_var : Time variable name from the raw dataset.
#'          dformat : Format of the date collected in raw data.
#'          tformat: Format of the time collected in raw data.
#'          sdtm_var_name : Reference variable name.
#' @param raw_source List contains all the raw datasets.
#' @return DM data frame with the reference dates populated.
#' @export
#' @examples
#' dm <- tibble::tribble(
#'   ~patient_number,   ~USUBJID, ~SUBJID, ~SEX,
#'   "001",           "XXXX-001",   "001",  "F",
#'   "002",           "XXXX-002",   "002",  "M",
#'   "003",           "XXXX-003",   "003",  "M"
#' )
#'
#' ref_date_config_df <- tibble::tribble(
#'   ~raw_dataset_name,   ~date_var,   ~time_var,      ~dformat, ~tformat, ~sdtm_var_name,
#'   "ex1_raw",         "EX_ST_DT1", "EX_ST_TM1",  "dd-mm-yyyy",    "H:M",      "RFSTDTC",
#'   "ex2_raw",         "EX_ST_DT2",          NA, "dd-mmm-yyyy",       NA,      "RFSTDTC",
#'   "ex1_raw",         "EX_EN_DT1", "EX_EN_TM1",  "dd-mm-yyyy",    "H:M",      "RFENDTC",
#'   "ex2_raw",         "EX_ST_DT2",          NA, "dd-mmm-yyyy",       NA,      "RFENDTC"
#' )
#'
#' ex1_raw <- tibble::tribble(
#'   ~patient_number, ~EX_ST_DT1,   ~EX_EN_DT1, ~EX_ST_TM1, ~EX_EN_TM1,
#'   "001",         "15-05-2023", "15-05-2023",    "10:20",    "11:00",
#'   "001",         "15-05-2023", "15-05-2023",     "9:15",    "10:00",
#'   "001",         "15-05-2023", "15-05-2023",     "8:19",    "09:00",
#'   "002",         "02-10-2023", "02-10-2023",  "UNK:UNK",         NA,
#'   "002",         "03-11-2023", "03-11-2023",    "11:19",         NA
#' )
#'
#' ex2_raw <- tibble::tribble(
#'   ~patient_number,     ~EX_ST_DT2,
#'   "001",            "11-JUN-2023",
#'   "002",            "24-OCT-2023",
#'   "002",            "25-JUL-2023",
#'   "002",            "30-OCT-2023",
#'   "002",           "UNK-OCT-2023"
#' )
#'
#' raw_data_list <- list(ex1_raw = ex1_raw, ex2_raw = ex2_raw)
#'
#' dm_df <- oak_cal_ref_dates(dm,
#'   der_var = "RFSTDTC",
#'   min_max = "min",
#'   ref_date_config_df = ref_date_config_df,
#'   raw_source = raw_data_list
#' )
#'
oak_cal_ref_dates <- function(ds_in = dm,
                              der_var,
                              min_max = "min",
                              ref_date_config_df,
                              raw_source) {
  # Check if ref_date_config_df is a data frame and has all required variables
  admiraldev::assert_data_frame(ref_date_config_df, required_vars = exprs(
    raw_dataset_name, date_var,
    time_var, dformat,
    tformat, sdtm_var_name
  ))

  admiraldev::assert_list_of(raw_source, "data.frame")

  ds_out <- data.frame()
  for (i in seq_along(ref_date_config_df$raw_dataset_name)) {
    raw_dataset_name <- ref_date_config_df$raw_dataset_name[i]
    date_variable <- ref_date_config_df$date_var[i]
    date_format <- ref_date_config_df$dformat[i]
    time_var <- ref_date_config_df$time_var[i]
    time_format <- ref_date_config_df$tformat[i]
    sdtm_var <- ref_date_config_df$sdtm_var_name[i]
    raw_dataset <- raw_source[[raw_dataset_name]]

    if (der_var == sdtm_var && !is.null(raw_dataset)) {
      ds_out1 <- cal_min_max_date(
        raw_dataset = raw_dataset,
        date_variable = date_variable,
        time_variable = time_var,
        date_format = date_format,
        time_format = time_format,
        val_type = min_max
      )
      ds_out <- rbind(ds_out, ds_out1)
    } else if (der_var == sdtm_var && is.null(raw_dataset)) {
      warning(paste0(
        raw_dataset_name,
        " is not present in the source data list but referenced in ref_date_config_df"
      ))
    }
  }

  if (min_max == "min") {
    df_final <- ds_out |> dplyr::arrange(patient_number, datetime)
  } else {
    df_final <- ds_out |> dplyr::arrange(dplyr::desc(datetime))
  }

  df_final <- df_final[!duplicated(df_final$patient_number), c("patient_number", "datetime")]
  colnames(df_final)[colnames(df_final) == "datetime"] <- der_var

  dm <- dplyr::left_join(ds_in, y = df_final, by = "patient_number")
  return(dm)
}
