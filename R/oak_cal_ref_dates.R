#' Calculate Reference dates RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC
#'


oak_cal_ref_dates <- function(ds_in = dm,
                             der_var = "RFSTDTC",
                             min_max = "min",
                             raw_dataset_list,
                             raw_date_list) {
  # current_function()

  # validate the input
  # oak_assert_that(is.data.frame(ds_in))
  # # oak_assert_that(
  # #   der_var == "RFSTDTC",
  # #   msg = "der_var must be equal to 'RFSTDTC'"
  # # )
  # oak_assert_that(unique(ds_in$DOMAIN) == "DM",
  #                 msg = "ds_in must contain only 'DM' domain"
  # )
  # oak_assert_that(
  #   !(der_var %in% colnames(ds_in)),
  #   msg = paste(
  #     "ds_in data frame must not contain",
  #     der_var,
  #     "column"
  #   )
  # )
  # oak_assert_that(is.list(raw_dataset_list))
  # oak_assert_that(length(raw_dataset_list) > 0)
  #
  # oak_assert_that(is.list(raw_var_list))
  # oak_assert_that(length(raw_var_list) > 0)
  #
  # # Check if the raw_var_list has equal count of raw_dataset_list
  # oak_assert_that(length(raw_dataset_list) == length(raw_var_list))

  # check if ref_date_conf file is present in the oak_pkg_env
  # oak_assert_that(
  #   "ref_date_conf" %in% ls(oak_pkg_env),
  #   msg = paste(
  #     "'ref_date_conf' not present in the 'oak_pkg_env'.",
  #     "Provide reference_date_configuration.csv."
  #   )
  # )

  # ref_date_conf_loaded <- oak_pkg_env$ref_date_conf
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
