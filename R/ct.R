#' Controlled terminology variables
#'
#' @description
#' [ct_vars()] returns the mandatory variables to be present in a data set
#' representing a controlled terminology. By default, it returns all required
#' variables.
#'
#' If only the subset of variables used for matching terms are needed, then
#' request this subset of variables by passing the argument value `"from"`. If
#' only the mapping-to variable is to be requested, then simply pass `"to"`. If
#' only the code-list code variable name is needed then pass `"cl"`.
#'
#' @param set A scalar character (string), one of: `"all"` (default), `"cl"`,
#'   `"from"` or `"to"`.
#'
#' @examples
#' # These two calls are equivalent and return all required variables in a
#' # controlled terminology data set.
#' sdtm.oak:::ct_vars()
#' sdtm.oak:::ct_vars("all")
#'
#' # "Codelist code" variable name.
#' sdtm.oak:::ct_vars("cl")
#'
#' # "From" variables
#' sdtm.oak:::ct_vars("from")
#'
#' # The "to" variable.
#' sdtm.oak:::ct_vars("to")
#'
#' @keywords internal
ct_vars <- function(set = c("all", "cl", "from", "to")) {

  admiraldev::assert_character_vector(set)

  set <- match.arg(set)
  cl_var <- "codelist_code"
  from_vars <- c("collected_value", "term_synonyms")
  to_var <- "term_value"

  if (identical(set, "all"))
    return(c(cl_var, from_vars, to_var))

  if (identical(set, "cl"))
    return(cl_var)

  if (identical(set, "from"))
    return(from_vars)

  if (identical(set, "to"))
    return(to_var)

}

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
ct_mappings <- function(ct, from = ct_vars("from"), to = ct_vars("to")) {

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
           from =  ct_vars("from"),
           to = ct_vars("to")) {

    ct %||% return(x)

    cl <- cl %||% unique(ct[[ct_vars("cl")]])
    ct <- dplyr::filter(ct, .data[[ct_vars("cl")]] %in% cl)

    mappings <- ct_mappings(ct, from = from, to = to)
    recode(
      x,
      from = mappings$from,
      to = mappings$to,
      .no_match = toupper(x)
    )

  }

#' Read in a controlled terminology
#'
#' [read_ct()] imports a controlled terminology specification data set as a
#' [tibble][tibble::tibble-package].
#'
#' @param file A path to a file containing a controlled terminology
#'   specification data set. The following are expected of this file:
#'
#' - The file is expected to be a CSV file;
#' - The file is expected to contain a first row of column names;
#' - This minimal set of variables is expected: `r knitr::combine_words(ct_vars())`.
#'
#' @returns A [tibble][tibble::tibble-package] with a controlled terminology
#'   specification.
#'
#' @export
read_ct <- function(file = stop("`file` must be specified")) {

  # TODO: Until we have a more precise specification of the controlled
  # terminology, we just read all columns as character. We assert nonetheless
  # for the existence of the ct variables: `collected_value`, `term_synonyms` and
  # `term_value` (provided by the helper `ct_vars()`).
  ct <- readr::read_csv(file = file, col_types = "c")
  admiraldev::assert_data_frame(arg = ct, required_vars = rlang::syms(ct_vars()))

  ct
}

#' Find the path to an example controlled terminology file
#'
#' [ct_example()] resolves the local path to an example controlled
#' terminology file.
#'
#' @param example A string with either the basename, file name, or relative path
#'   to a controlled terminology file bundled with `{stdm.oak}`, see examples.
#'
#'
#' @examples
#' # Get the local path to controlled terminology example file 01
#' # Using the basename only:
#' ct_example("ct-01-cm")
#'
#' # Using the file name:
#' ct_example("ct-01-cm.csv")
#'
#' # Using the relative path:
#' ct_example("ct/ct-01-cm.csv")
#'
#' # If no example is provided it returns a vector of possible choices.
#' ct_example()
#'
#' @export
ct_example <- function(example) {
  # If no example is requested, then return all available files.
  if (missing(example)) {
    ct_path <- system.file("ct", package = "sdtm.oak", mustWork = TRUE)
    ct_files <- list.files(ct_path, pattern = "*.csv")
    return(ct_files)
  }

  # Otherwise, resolve the local path to the example requested.
  admiraldev::assert_character_scalar(example, optional = TRUE)
  base_name <- tools::file_path_sans_ext(basename(example))
  path <- file.path("ct", paste0(base_name, ".csv"))
  local_path <- system.file(path, package = "sdtm.oak")

  if (identical(local_path, "")) {
    stop(
      glue::glue(
        "'{example}' does not match any ct files. Run `ct_example()` for options."
      ),
      call. = FALSE
    )

  } else {
    local_path <-
      system.file(path, package = "sdtm.oak", mustWork = TRUE)
    return(local_path)
  }
}

#' Read an example controlled terminology specification
#'
#' [read_ct_example()] imports one of the bundled controlled terminology
#' specification data sets as a [tibble][tibble::tibble-package] into R.
#'
#' @param example The file name of a controlled terminology data set bundled
#'   with `{stdm.oak}`, run `read_ct_example()` for available example files.
#'
#' @returns A [tibble][tibble::tibble-package] with a controlled terminology
#'   specification data set.
#'
#' @examples
#' # Leave the `example` parameter as missing for available example files.
#' read_ct_example()
#'
#' # Read an example ct file.
#' read_ct_example("ct-01-cm.csv")
#'
#' # You may omit the file extension.
#' read_ct_example("ct-01-cm")
#'
#' @export
read_ct_example <- function(example) {

  if (missing(example)) {
    return(ct_example())
  } else {
    admiraldev::assert_character_scalar(example)
  }

  path <- ct_example(example)
  read_ct(file = path)

}
