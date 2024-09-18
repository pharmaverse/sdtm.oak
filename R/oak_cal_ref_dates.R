#' Calculate Reference dates RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC
#'


oak_cal_ref_dates <- function(ds_in = dm,
                             der_var = "RFSTDTC",
                             min_max = "min",
                             ref_date_conf,
                             raw_source) {

  ds_out <- data.frame()
  for(i in 1:length(ref_date_conf$dataset_name)) {
browser()
    raw_dataset_name <- ref_date_conf$dataset_name[i]
    date_variable <- ref_date_conf$date_var[i]
    date_format <- ref_date_conf$dformat[i]
    time_var <- ref_date_conf$time_var[i]
    time_format <- ref_date_conf$tformat[i]
    sdtm_var <- ref_date_conf$sdtm_var_name[i]
    raw_dataset <- raw_source[[raw_dataset_name]]

    if (der_var == sdtm_var) {
      ds_out1 <- cal_min_max_date(
        raw_dataset = raw_dataset,
        sdtm_var_name = der_var,
        date_variable = date_variable,
        time_variable = time_var,
        date_format = date_format,
        time_format = time_format,
        val_type = min_max
      )
      ds_out <- rbind(ds_out, ds_out1)
    }
  }
browser()

#ref_dates <- purrr::pmap_df(ds_out, .f = get_df)
if (min_max == "min") {
  df_final <- ds_out %>% dplyr::arrange(patient_number,datetime)
} else {
  df_final <- ds_out %>% dplyr::arrange(dplyr::desc(patient_number),dplyr::desc(datetime))
}
df_final <- df_final[!duplicated(df_final$patient_number), c("patient_number", "datetime")]
colnames(df_final)[colnames(df_final) == "datetime"] <- der_var

dm <- dplyr::left_join(ds_in, y = df_final, by = "patient_number")
return(dm)

}
