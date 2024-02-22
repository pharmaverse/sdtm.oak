#' Controlled terminology mappings
#'
#' @description
#' [ct_mappings()] takes a controlled terminology specification and returns the
#' mappings in the form of a [tibble][tibble::tibble-package] in long format,
#' i.e. the recoding of values in the `from` column to the `to` column values,
#' one mapping per row.
#'
#' The resulting mappings are unique, i.e. if `from` values are duplicated in
#' two `from` columns, the first column indicated in `from` takes precedence,
#' and only that mapping is retained in the controlled terminology map.
#'
#' @param ct Controlled terminology specification as a
#'   [tibble][tibble::tibble-package]. Each row is for a mapped controlled term.
#'   Controlled terms are expected in the column indicated by `to_col`.
#' @param from A character vector of column names indicating the variables
#' containing values to be recoded.
#' @param to A single string indicating the column whose values are to be
#'   recoded into.
#'
#' @returns A [tibble][tibble::tibble-package] with two columns, `from` and
#'   `to`, indicating the mapping of values, one per row.
#'
#' @examples
#' # example code
#'
#'
#'
#'
#'
#'
#'
#' @importFrom rlang .data
#' @keywords internal
ct_mappings <- function(ct, from =  c("collected_value", "term_synonyms"), to = "term_value") {

  # TODO: Assertions and memoisation.

  cols <- c(to, from)

  ct_mappings <-
    ct |>
    dplyr::mutate(to = !!rlang::sym(to)) |>
    tidyr::pivot_longer(cols = dplyr::all_of(cols),
                        values_to = "from",
                        names_to = "type") |>
    dplyr::select(c("type", "from", "to")) |>
    dplyr::mutate(type = factor(.data$type, levels = cols)) |>
    dplyr::arrange(.data$type) |>
    dplyr::select(-"type") |>
    tidyr::drop_na(.data$from) |>
    dplyr::mutate(from = str_split(.data$from)) |>
    tidyr::unnest(from) |>
    dplyr::filter(from != "") |> # In case the split resulted in empty strings.
    dplyr::mutate(from = trimws(.data$from), to = trimws(.data$to)) |>
    dplyr::distinct(.data$from, .keep_all = TRUE)

  ct_mappings
}

#' Recode according to controlled terminology
#'
#' [ct_map()] recodes a vector following a controlled terminology.
#'
#' @param x A character vector of terms to be recoded following a controlled
#' terminology.
#' @param ct A [tibble][tibble::tibble-package] providing a controlled
#'   terminology specification.
#' @param cl A character vector indicating a set of possible controlled
#'   terminology code-lists codes to be used for recoding. By default (`NULL`)
#'   all code-lists available in `ct` are used.
#' @param from A character vector of column names indicating the variables
#' containing values to be matched against for terminology recoding.
#' @param to A single string indicating the column whose values are to be
#'   recoded into.
#'
#' @returns A character vector of terminology recoded values from `x`. If no
#'   match is found in the controlled terminology spec provided in `ct`, then
#'   `x` values are returned in uppercase. If `ct` is not provided `x` is
#'   returned unchanged.
#'
#' @importFrom rlang %||% .data
#' @export
ct_map <-
  function(x,
           ct = NULL,
           cl = NULL,
           from =  c("collected_value", "term_synonyms"),
           to = "term_value") {

    ct %||% return(x)

    cl <- cl %||% unique(ct$codelist_code)
    ct <- dplyr::filter(ct, .data$codelist_code %in% cl)

    mappings <- ct_mappings(ct, from = from, to = to)
    recode(
      x,
      from = mappings$from,
      to = mappings$to,
      .no_match = toupper(x)
    )

  }
