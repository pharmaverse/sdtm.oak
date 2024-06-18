#' Derive an SDTM variable with a hardcoded value
#'
#' @description
#' [sdtm_hardcode()] is an internal function packing the same functionality as
#' [hardcode_no_ct()] and [hardcode_ct()] together but aimed at developers only.
#' As a user please use either [hardcode_no_ct()] or [hardcode_ct()].
#'
#' @param raw_dat The raw dataset (dataframe); must include the
#'   variables passed in `id_vars` and `raw_var`.
#' @param raw_var The raw variable: a single string indicating the name of the
#'   raw variable in `raw_dat`.
#' @param tgt_var The target SDTM variable: a single string indicating the name
#'   of variable to be derived.
#' @param tgt_val The target SDTM value to be hardcoded into the variable
#'   indicated in `tgt_var`.
#' @param ct_spec Study controlled terminology specification: a dataframe with a
#'   minimal set of columns, see [ct_spec_vars()] for details. This parameter is
#'   optional, if left as `NULL` no controlled terminology recoding is applied.
#' @param ct_clst A codelist code indicating which subset of the controlled
#'   terminology to apply in the derivation. This parameter is optional, if left
#'   as `NULL`, all possible recodings in `ct_spec` are attempted.
#' @param tgt_dat Target dataset: a data frame to be merged against `raw_dat` by
#'   the variables indicated in `id_vars`. This parameter is optional, see
#'   section Value for how the output changes depending on this argument value.
#' @param id_vars Key variables to be used in the join between the raw dataset
#'   (`raw_dat`) and the target data set (`tgt_dat`).
#'
#' @returns The returned data set depends on the value of `tgt_dat`:
#' - If no target dataset is supplied, meaning that `tgt_dat` defaults to
#' `NULL`, then the returned data set is `raw_dat`, selected for the variables
#' indicated in `id_vars`, and a new extra column: the derived variable, as
#' indicated in `tgt_var`.
#' - If the target dataset is provided, then it is merged with the raw data set
#' `raw_dat` by the variables indicated in `id_vars`, with a new column: the
#' derived variable, as indicated in `tgt_var`.
#'
#' @importFrom rlang :=
#' @keywords internal
sdtm_hardcode <- function(tgt_dat = NULL,
                          tgt_var,
                          raw_dat,
                          raw_var,
                          tgt_val,
                          ct_spec = NULL,
                          ct_clst = NULL,
                          id_vars = oak_id_vars()) {
  admiraldev::assert_character_scalar(raw_var)
  admiraldev::assert_character_scalar(tgt_var)
  admiraldev::assert_character_scalar(tgt_val)
  admiraldev::assert_character_vector(id_vars)
  assertthat::assert_that(contains_oak_id_vars(id_vars),
    msg = "`id_vars` must include the oak id vars."
  )
  admiraldev::assert_data_frame(raw_dat, required_vars = rlang::syms(c(id_vars, raw_var)))
  admiraldev::assert_data_frame(tgt_dat, required_vars = rlang::syms(id_vars), optional = TRUE)
  assert_ct_spec(ct_spec, optional = TRUE)
  assert_ct_clst(ct_spec = ct_spec, ct_clst = ct_clst, optional = TRUE)

  join_dat <-
    raw_dat |>
    dplyr::select(dplyr::all_of(c(id_vars, raw_var))) |>
    sdtm_join(tgt_dat = tgt_dat, id_vars = id_vars)

  # Recode the hardcoded value following terminology.
  tgt_val <- ct_map(tgt_val, ct_spec = ct_spec, ct_clst = ct_clst)

  join_dat |>
    mutate("{tgt_var}" := recode(x = !!rlang::sym(raw_var), to = tgt_val)) |> # nolint object_name_linter()
    dplyr::select(-dplyr::any_of(setdiff(raw_var, tgt_var))) |>
    dplyr::relocate(dplyr::all_of(tgt_var), .after = dplyr::last_col())
}

#' Derive an SDTM variable with a hardcoded value
#'
#'
#' @description
#' - [hardcode_no_ct()] maps a hardcoded value to a target SDTM variable that has
#' no terminology restrictions.
#'
#' - [hardcode_ct()] maps a hardcoded value to a target SDTM variable with
#' controlled terminology recoding.
#'
#' @param raw_dat The raw dataset (dataframe); must include the
#'   variables passed in `id_vars` and `raw_var`.
#' @param raw_var The raw variable: a single string indicating the name of the
#'   raw variable in `raw_dat`.
#' @param tgt_var The target SDTM variable: a single string indicating the name
#'   of variable to be derived.
#' @param tgt_val The target SDTM value to be hardcoded into the variable
#'   indicated in `tgt_var`.
#' @param ct_spec Study controlled terminology specification: a dataframe with a
#'   minimal set of columns, see [ct_spec_vars()] for details. This parameter is
#'   optional, if left as `NULL` no controlled terminology recoding is applied.
#' @param ct_clst A codelist code indicating which subset of the controlled
#'   terminology to apply in the derivation. This parameter is optional, if left
#'   as `NULL`, all possible recodings in `ct_spec` are attempted.
#' @param tgt_dat Target dataset: a data frame to be merged against `raw_dat` by
#'   the variables indicated in `id_vars`. This parameter is optional, see
#'   section Value for how the output changes depending on this argument value.
#' @param id_vars Key variables to be used in the join between the raw dataset
#'   (`raw_dat`) and the target data set (`raw_dat`).
#'
#' @returns The returned data set depends on the value of `tgt_dat`:
#' - If no target dataset is supplied, meaning that `tgt_dat` defaults to
#' `NULL`, then the returned data set is `raw_dat`, selected for the variables
#' indicated in `id_vars`, and a new extra column: the derived variable, as
#' indicated in `tgt_var`.
#' - If the target dataset is provided, then it is merged with the raw data set
#' `raw_dat` by the variables indicated in `id_vars`, with a new column: the
#' derived variable, as indicated in `tgt_var`.
#'
#' @examples
#' md1 <-
#'   tibble::tribble(
#'     ~oak_id, ~raw_source, ~patient_number, ~MDRAW,
#'     1L,      "MD1",       101L,            "BABY ASPIRIN",
#'     2L,      "MD1",       102L,            "CORTISPORIN",
#'     3L,      "MD1",       103L,            NA_character_,
#'     4L,      "MD1",       104L,            "DIPHENHYDRAMINE HCL"
#'   )
#'
#' # Derive a new variable `CMCAT` by overwriting `MDRAW` with the
#' # hardcoded value "GENERAL CONCOMITANT MEDICATIONS".
#' hardcode_no_ct(
#'   tgt_val = "GENERAL CONCOMITANT MEDICATIONS",
#'   raw_dat = md1,
#'   raw_var = "MDRAW",
#'   tgt_var = "CMCAT"
#' )
#'
#' cm_inter <-
#'   tibble::tribble(
#'     ~oak_id, ~raw_source, ~patient_number, ~CMTRT,                ~CMINDC,
#'     1L,      "MD1",       101L,            "BABY ASPIRIN",        NA,
#'     2L,      "MD1",       102L,            "CORTISPORIN",         "NAUSEA",
#'     3L,      "MD1",       103L,            "ASPIRIN",             "ANEMIA",
#'     4L,      "MD1",       104L,            "DIPHENHYDRAMINE HCL", "NAUSEA",
#'     5L,      "MD1",       105L,            "PARACETAMOL",         "PYREXIA"
#'   )
#'
#' # Derive a new variable `CMCAT` by overwriting `MDRAW` with the
#' # hardcoded value "GENERAL CONCOMITANT MEDICATIONS" with a prior join to
#' # `target_dataset`.
#' hardcode_no_ct(
#'   tgt_dat = cm_inter,
#'   tgt_val = "GENERAL CONCOMITANT MEDICATIONS",
#'   raw_dat = md1,
#'   raw_var = "MDRAW",
#'   tgt_var = "CMCAT"
#' )
#'
#' # Controlled terminology specification
#' (ct_spec <- read_ct_spec_example("ct-01-cm"))
#'
#' # Hardcoding of `CMCAT` with the value `"GENERAL CONCOMITANT MEDICATIONS"`
#' # involving terminology recoding. `NA` values in `MDRAW` are preserved in
#' # `CMCAT`.
#' hardcode_ct(
#'   tgt_dat = cm_inter,
#'   tgt_var = "CMCAT",
#'   raw_dat = md1,
#'   raw_var = "MDRAW",
#'   tgt_val = "GENERAL CONCOMITANT MEDICATIONS",
#'   ct_spec = ct_spec,
#'   ct_clst = "C66729"
#' )
#'
#' @name harcode
NULL

#' @export
#' @rdname harcode
hardcode_no_ct <- function(tgt_dat = NULL,
                           tgt_val,
                           raw_dat,
                           raw_var,
                           tgt_var,
                           id_vars = oak_id_vars()) {
  admiraldev::assert_character_scalar(raw_var)
  admiraldev::assert_character_scalar(tgt_var)
  assertthat::assert_that(assertthat::is.scalar(tgt_val),
    msg = "`tgt_val` must be a scalar value."
  )
  admiraldev::assert_character_vector(id_vars)
  assertthat::assert_that(contains_oak_id_vars(id_vars),
    msg = "`id_vars` must include the oak id vars."
  )
  admiraldev::assert_data_frame(raw_dat, required_vars = rlang::syms(c(id_vars, raw_var)))
  admiraldev::assert_data_frame(tgt_dat, required_vars = rlang::syms(id_vars), optional = TRUE)

  sdtm_hardcode(
    tgt_dat = tgt_dat,
    tgt_val = tgt_val,
    raw_dat = raw_dat,
    raw_var = raw_var,
    tgt_var = tgt_var,
    id_vars = id_vars
  )
}

#' @export
#' @rdname harcode
hardcode_ct <-
  function(tgt_dat = NULL,
           tgt_val,
           raw_dat,
           raw_var,
           tgt_var,
           ct_spec,
           ct_clst,
           id_vars = oak_id_vars()) {
    admiraldev::assert_character_scalar(raw_var)
    admiraldev::assert_character_scalar(tgt_var)
    assertthat::assert_that(assertthat::is.scalar(tgt_val),
      msg = "`tgt_val` must be a scalar value."
    )
    admiraldev::assert_character_vector(id_vars)
    assertthat::assert_that(contains_oak_id_vars(id_vars),
      msg = "`id_vars` must include the oak id vars."
    )
    admiraldev::assert_data_frame(raw_dat, required_vars = rlang::syms(c(id_vars, raw_var)))
    admiraldev::assert_data_frame(tgt_dat,
      required_vars = rlang::syms(id_vars),
      optional = TRUE
    )

    assert_ct_spec(ct_spec, optional = FALSE)
    assert_ct_clst(ct_spec = ct_spec, ct_clst = ct_clst, optional = FALSE)

    sdtm_hardcode(
      tgt_dat = tgt_dat,
      tgt_val = tgt_val,
      raw_dat = raw_dat,
      raw_var = raw_var,
      tgt_var = tgt_var,
      ct_spec = ct_spec,
      ct_clst = ct_clst,
      id_vars = id_vars
    )
  }
