#' Derive an SDTM variable
#'
#' @description
#' [sdtm_assign()] is an internal function packing the same functionality as
#' [assign_no_ct()] and [assign_ct()] together but aimed at developers only.
#' As a user please use either [assign_no_ct()] or [assign_ct()].
#'
#' @param raw_dat The raw dataset (dataframe); must include the
#'   variables passed in `id_vars` and `raw_var`.
#' @param raw_var The raw variable: a single string indicating the name of the
#'   raw variable in `raw_dat`.
#' @param tgt_var The target SDTM variable: a single string indicating the name
#'   of variable to be derived.
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
#'
#' @importFrom rlang :=
#' @keywords internal
sdtm_assign <- function(tgt_dat = NULL,
                        tgt_var,
                        raw_dat,
                        raw_var,
                        ct_spec = NULL,
                        ct_clst = NULL,
                        id_vars = oak_id_vars()) {
  admiraldev::assert_character_scalar(raw_var)
  admiraldev::assert_character_scalar(tgt_var)
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

  # Recode the raw variable following terminology.
  tgt_val <- ct_map(join_dat[[raw_var]], ct_spec = ct_spec, ct_clst = ct_clst)

  join_dat |>
    mutate("{tgt_var}" := tgt_val) |> # nolint object_name_linter()
    dplyr::select(-dplyr::any_of(setdiff(raw_var, tgt_var))) |>
    dplyr::relocate(dplyr::all_of(tgt_var), .after = dplyr::last_col())
}

#' Derive an SDTM variable
#'
#' @description
#' - [assign_no_ct()] maps a variable in a raw dataset to a target SDTM
#' variable that has no terminology restrictions.
#'
#' - [assign_ct()] maps a variable in a raw dataset to a target SDTM variable
#' following controlled terminology recoding.
#'
#' @param raw_dat The raw dataset (dataframe); must include the
#'   variables passed in `id_vars` and `raw_var`.
#' @param raw_var The raw variable: a single string indicating the name of the
#'   raw variable in `raw_dat`.
#' @param tgt_var The target SDTM variable: a single string indicating the name
#'   of variable to be derived.
#' @param ct_spec Study controlled terminology specification: a dataframe with a
#'   minimal set of columns, see [ct_spec_vars()] for details.
#' @param ct_clst A codelist code indicating which subset of the controlled
#'   terminology to apply in the derivation.
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
#'
#' md1 <-
#'   tibble::tibble(
#'     oak_id = 1:14,
#'     raw_source = "MD1",
#'     patient_number = 101:114,
#'     MDIND = c(
#'       "NAUSEA", "NAUSEA", "ANEMIA", "NAUSEA", "PYREXIA",
#'       "VOMITINGS", "DIARHHEA", "COLD",
#'       "FEVER", "LEG PAIN", "FEVER", "COLD", "COLD", "PAIN"
#'     )
#'   )
#'
#' assign_no_ct(
#'   tgt_var = "CMINDC",
#'   raw_dat = md1,
#'   raw_var = "MDIND"
#' )
#'
#' cm_inter <-
#'   tibble::tibble(
#'     oak_id = 1:14,
#'     raw_source = "MD1",
#'     patient_number = 101:114,
#'     CMTRT = c(
#'       "BABY ASPIRIN",
#'       "CORTISPORIN",
#'       "ASPIRIN",
#'       "DIPHENHYDRAMINE HCL",
#'       "PARCETEMOL",
#'       "VOMIKIND",
#'       "ZENFLOX OZ",
#'       "AMITRYPTYLINE",
#'       "BENADRYL",
#'       "DIPHENHYDRAMINE HYDROCHLORIDE",
#'       "TETRACYCLINE",
#'       "BENADRYL",
#'       "SOMINEX",
#'       "ZQUILL"
#'     ),
#'     CMROUTE = c(
#'       "ORAL",
#'       "ORAL",
#'       NA,
#'       "ORAL",
#'       "ORAL",
#'       "ORAL",
#'       "INTRAMUSCULAR",
#'       "INTRA-ARTERIAL",
#'       NA,
#'       "NON-STANDARD",
#'       "RANDOM_VALUE",
#'       "INTRA-ARTICULAR",
#'       "TRANSDERMAL",
#'       "OPHTHALMIC"
#'     )
#'   )
#'
#' # Controlled terminology specification
#' (ct_spec <- read_ct_spec_example("ct-01-cm"))
#'
#' assign_ct(
#'   tgt_dat = cm_inter,
#'   tgt_var = "CMINDC",
#'   raw_dat = md1,
#'   raw_var = "MDIND",
#'   ct_spec = ct_spec,
#'   ct_clst = "C66729"
#' )
#'
#' @name assign
NULL

#' @order 1
#' @export
#' @rdname assign
assign_no_ct <- function(tgt_dat = NULL,
                         tgt_var,
                         raw_dat,
                         raw_var,
                         id_vars = oak_id_vars()) {
  admiraldev::assert_character_scalar(raw_var)
  admiraldev::assert_character_scalar(tgt_var)
  admiraldev::assert_character_vector(id_vars)
  assertthat::assert_that(contains_oak_id_vars(id_vars),
    msg = "`id_vars` must include the oak id vars."
  )
  admiraldev::assert_data_frame(raw_dat, required_vars = rlang::syms(c(id_vars, raw_var)))
  admiraldev::assert_data_frame(tgt_dat, required_vars = rlang::syms(id_vars), optional = TRUE)

  sdtm_assign(
    tgt_dat = tgt_dat,
    tgt_var = tgt_var,
    raw_dat = raw_dat,
    raw_var = raw_var,
    id_vars = id_vars
  )
}

#' @order 2
#' @export
#' @rdname assign
assign_ct <- function(tgt_dat = NULL,
                      tgt_var,
                      raw_dat,
                      raw_var,
                      ct_spec,
                      ct_clst,
                      id_vars = oak_id_vars()) {
  admiraldev::assert_character_scalar(raw_var)
  admiraldev::assert_character_scalar(tgt_var)
  admiraldev::assert_character_vector(id_vars)
  assertthat::assert_that(contains_oak_id_vars(id_vars),
    msg = "`id_vars` must include the oak id vars."
  )
  admiraldev::assert_data_frame(raw_dat, required_vars = rlang::syms(c(id_vars, raw_var)))
  admiraldev::assert_data_frame(tgt_dat, required_vars = rlang::syms(id_vars), optional = TRUE)

  sdtm_assign(
    tgt_dat = tgt_dat,
    tgt_var = tgt_var,
    raw_dat = raw_dat,
    raw_var = raw_var,
    id_vars = id_vars,
    ct_spec = ct_spec,
    ct_clst = ct_clst
  )
}
