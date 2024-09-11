#' Calculate Reference dates RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC
#'


oak_cal_ref_dates <- function(ds_in = dm,
                             der_var = "RFSTDTC",
                             min_max = "min",
                             raw_dataset_list,
                             raw_date_list) {

  ds_out <- data.frame()
  for(i in 1:length(raw_dataset_list)) {
browser()
    raw_dataset <- raw_source[[raw_dataset_list[i]]]
    date_variable <- raw_date_list[[i]]

    ds_out1 <- cal_min_max_date(
      raw_dataset = raw_dataset,
      sdtm_var_name = der_var,
      date_variable = date_variable,
      val_type = min_max
    )
    ds_out <- rbind(ds_out, ds_out1)
  }
browser()

# ref_dates <- purrr::pmap_df(ds_out, .f = get_df)
if (min_max == "min") {
  df_final <- ds_out %>% dplyr::arrange(SUBJID,datetime)
} else {
  df_final <- ds_out %>% dplyr::arrange(dplyr::desc(SUBJID),dplyr::desc(datetime))
}
df_final <- df_final[!duplicated(df_final$SUBJID), c("SUBJID", "datetime")]
colnames(df_final)[colnames(df_final) == "datetime"] <- der_var

return(df_final)

}
