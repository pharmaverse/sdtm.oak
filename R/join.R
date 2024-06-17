#' SDTM join
#'
#' [sdtm_join()] is a special join between a raw data set and a target data
#' set. This function supports conditioned data frames.
#'
#' @param raw_dat The raw dataset: a dataframe or a conditioned data frame. Must
#'   include the variables passed in `id_vars`.
#' @param tgt_dat Target dataset: a data frame or a conditioned data frame to be
#'   merged against `raw_dat` by the variables indicated in `id_vars`.
#' @param id_vars Key variables to be used in the join between the raw dataset
#'   (`raw_dat`) and the target data set (`raw_dat`).
#'
#' @returns A data frame, or a conditioned data frame if at least one of the
#'   input data sets is a conditioned data frame.
#'
#' @keywords internal
#' @importFrom rlang %||%
sdtm_join <- function(raw_dat,
                      tgt_dat = NULL,
                      id_vars = oak_id_vars()) {
  raw_dat_cnd <- get_cnd_df_cnd(raw_dat) %||% rep(TRUE, nrow(raw_dat))
  tgt_dat <- tgt_dat %||% raw_dat[id_vars]
  tgt_dat_cnd <- get_cnd_df_cnd(tgt_dat) %||% rep(TRUE, nrow(tgt_dat))

  # `rm_cnd_df()` prevents `mutate` from dispatching.
  raw_dat <- dplyr::mutate(rm_cnd_df(raw_dat), `__raw_dat_cond__` = raw_dat_cnd)
  tgt_dat <- dplyr::mutate(rm_cnd_df(tgt_dat), `__tgt_dat_cond__` = tgt_dat_cnd)

  res <- dplyr::right_join(raw_dat, y = tgt_dat, by = id_vars)

  cnd <- res$`__raw_dat_cond__` & res$`__tgt_dat_cond__`
  res |>
    dplyr::select(-dplyr::all_of(c(
      "__raw_dat_cond__", "__tgt_dat_cond__"
    ))) |>
    new_cnd_df(cnd = cnd, .warn = FALSE)
}
