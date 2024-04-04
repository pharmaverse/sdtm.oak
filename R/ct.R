#' Controlled terminology variables
#'
#' @description
#' [ct_spec_vars()] returns the mandatory variables to be present in a data set
#' representing a controlled terminology. By default, it returns all required
#' variables.
#'
#' If only the subset of variables used for matching terms are needed, then
#' request this subset of variables by passing the argument value `"from"`. If
#' only the mapping-to variable is to be requested, then simply pass `"to"`. If
#' only the codelist code variable name is needed then pass `"ct_cltc"`.
#'
#' @param set A scalar character (string), one of: `"all"` (default), `"ct_cltc"`,
#'   `"from"` or `"to"`.
#'
#' @examples
#' # These two calls are equivalent and return all required variables in a
#' # controlled terminology data set.
#' sdtm.oak:::ct_spec_vars()
#' sdtm.oak:::ct_spec_vars("all")
#'
#' # "Codelist code" variable name.
#' sdtm.oak:::ct_spec_vars("ct_cltc")
#'
#' # "From" variables
#' sdtm.oak:::ct_spec_vars("from")
#'
#' # The "to" variable.
#' sdtm.oak:::ct_spec_vars("to")
#'
#' @keywords internal
#' @export
ct_spec_vars <- function(set = c("all", "ct_cltc", "from", "to")) {
  admiraldev::assert_character_vector(set)

  set <- match.arg(set)
  ct_cltc_var <- "codelist_code"
  from_vars <- c("collected_value", "term_synonyms")
  to_var <- "term_value"

  if (identical(set, "all")) {
    return(c(ct_cltc_var, from_vars, to_var))
  }

  if (identical(set, "ct_cltc")) {
    return(ct_cltc_var)
  }

  if (identical(set, "from")) {
    return(from_vars)
  }

  if (identical(set, "to")) {
    return(to_var)
  }
}

#' Assert a controlled terminology specification
#'
#' @description
#' [assert_ct_spec()] will check whether `ct_spec` is a data frame and if it contains the
#' variables: `r knitr::combine_words(ct_spec_vars())`.
#'
#' In addition, it will also check if the data frame is not empty (no rows), and
#' whether the columns \code{`r ct_spec_vars('ct_cltc')`} and \code{`r ct_spec_vars('to')`} do
#' not contain any `NA` values.
#'
#' @param ct_spec A data frame to be asserted as a valid controlled terminology data
#'   set.
#'
#' @returns The function throws an error if `ct_spec` is not a valid controlled
#'   terminology data set; otherwise, `ct_spec` is returned invisibly.
#'
#' @examples
#' # If `ct_spec` is a valid controlled terminology then it is returned invisibly.
#' ct_spec_01 <- read_ct_spec_example("ct-01-cm")
#' all.equal(ct_spec_01, sdtm.oak:::assert_ct_spec(ct_spec_01))
#'
#' # A minimal set of variables needs to be present in `ct_spec` for it to pass the
#' # assertion; `sdtm.oak:::ct_spec_vars()` defines their names.
#' (req_vars <- sdtm.oak:::ct_spec_vars())
#'
#' # Other (facultative) variables also present in the controlled terminology
#' # example.
#' (opt_vars <- setdiff(colnames(ct_spec_01), req_vars))
#'
#' # With only the mandatory variables, the assertion still passes.
#' sdtm.oak:::assert_ct_spec(ct_spec_01[req_vars])
#'
#' # Not having the required variables results in an error.
#' try(sdtm.oak:::assert_ct_spec(ct_spec_01[opt_vars]))
#'
#' @keywords internal
assert_ct_spec <- function(ct_spec, optional = FALSE) {
  admiraldev::assert_data_frame(
    arg = ct_spec,
    required_vars = rlang::syms(ct_spec_vars()),
    optional = optional
  )

  if (!is.null(ct_spec) && nrow(ct_spec) == 0L) {
    rlang::abort("`ct_spec` can't be empty.")
  }

  if (!is.null(ct_spec) && anyNA(ct_spec[[ct_spec_vars("ct_cltc")]])) {
    rlang::abort(glue::glue("`{ct_spec_vars('ct_cltc')}` can't have any NA values."))
  }

  if (!is.null(ct_spec) && anyNA(ct_spec[[ct_spec_vars("to")]])) {
    rlang::abort(glue::glue("`{ct_spec_vars('to')}` can't have any NA values."))
  }

  invisible(ct_spec)
}

#' Assert a codelist code
#'
#' [assert_ct_cltc()] asserts the validity of a codelist code in the context of
#' a controlled terminology specification.
#'
#' @param ct_spec Either a data frame encoding a controlled terminology data set, or
#'   `NULL`.
#' @param ct_cltc A string with a to-be asserted codelist code, or `NULL`.
#' @param optional A scalar logical, indicating whether `ct_cltc` can be `NULL` or
#'   not.
#'
#' @returns The function throws an error if `ct_cltc` is not a valid codelist code
#'   given the controlled terminology data set; otherwise, `ct_cltc` is returned
#'   invisibly.
#'
#' @examples
#' # Load a controlled terminology example.
#' (ct_spec <- read_ct_spec_example("ct-01-cm"))
#'
#' # Should work fine.
#' sdtm.oak:::assert_ct_cltc(ct_spec = ct_spec, ct_cltc = "C71113")
#'
#' # In certain cases, you might allow `ct_cltc` to be `NULL` as to indicate absence,
#' # in that case, set `optional` to `TRUE` to make `assert_ct_cltc()` more
#' # forgiving.
#' sdtm.oak:::assert_ct_cltc(ct_spec = ct_spec, ct_cltc = NULL, optional = TRUE)
#'
#' # Otherwise it would err.
#' try(sdtm.oak:::assert_ct_cltc(ct_spec = ct_spec, ct_cltc = NULL, optional = FALSE))
#'
#' @keywords internal
assert_ct_cltc <- function(ct_spec, ct_cltc, optional = FALSE) {

  is_ct_spec_missing <- is.null(ct_spec)
  is_ct_cltc_missing <- is.null(ct_cltc)
  is_required_ct_cltc_missing <- is_ct_cltc_missing && !optional
  is_ct_cltc_without_ct_spec <- is_ct_spec_missing && !is_ct_cltc_missing
  are_ct_spec_ct_cltc_available <- !is_ct_spec_missing && !is_ct_cltc_missing

  if (!is_ct_cltc_missing) {
    admiraldev::assert_character_scalar(ct_cltc)
  }

  if (is_required_ct_cltc_missing) {
    rlang::abort("`ct_cltc` is a required parameter.")
  }

  if (is_ct_cltc_without_ct_spec) {
    rlang::abort("`ct_spec` must be a valid controlled terminology if `ct_cltc` is supplied.")
  }

  if (is_ct_cltc_missing) {
    return(invisible(NULL))
  }

  if (!is_ct_spec_missing && is.na(ct_cltc)) {
    rlang::abort("`ct_cltc` can't be NA. Did you mean `NULL`?")
  }

  if (are_ct_spec_ct_cltc_available) {
    assert_ct_spec(ct_spec, optional = FALSE)
    ct_cltc_possibilities <-
      ct_spec |>
      dplyr::pull(ct_spec_vars("ct_cltc")) |>
      unique()
    admiraldev::assert_character_scalar(ct_cltc, values = ct_cltc_possibilities)
  }

  return(ct_cltc)
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
#' @param ct_spec Controlled terminology specification as a
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
#' # Read in a bundled controlled terminology spec example (ex. 01).
#' (ct_spec_01 <- read_ct_spec_example("ct-01-cm"))
#'
#' # Generate mappings from the terminology specification.
#' sdtm.oak:::ct_mappings(ct_spec = ct_spec_01)
#'
#' # Take a glimpse at those mappings where an actual recoding happens.
#' sdtm.oak:::ct_mappings(ct_spec = ct_spec_01) |>
#'   dplyr::filter(from != to) |>
#'   print(n = 20)
#'
#' @importFrom rlang .data
#' @keywords internal
ct_mappings <- function(ct_spec, from = ct_spec_vars("from"), to = ct_spec_vars("to")) {
  assert_ct_spec(ct_spec)
  cols <- c(to, from)

  ct_mappings <-
    ct_spec |>
    dplyr::mutate(to = !!rlang::sym(to)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(cols),
      values_to = "from",
      names_to = "type"
    ) |>
    dplyr::select(c("type", "from", "to")) |>
    dplyr::mutate(type = factor(.data$type, levels = cols)) |>
    dplyr::arrange(.data$type) |>
    dplyr::select(-"type") |>
    tidyr::drop_na("from") |>
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
#' @param ct_spec A [tibble][tibble::tibble-package] providing a controlled
#'   terminology specification.
#' @param ct_cltc A character vector indicating a set of possible controlled
#'   terminology codelists codes to be used for recoding. By default (`NULL`)
#'   all codelists available in `ct_spec` are used.
#' @param from A character vector of column names indicating the variables
#' containing values to be matched against for terminology recoding.
#' @param to A single string indicating the column whose values are to be
#'   recoded into.
#'
#' @returns A character vector of terminology recoded values from `x`. If no
#'   match is found in the controlled terminology spec provided in `ct_spec`, then
#'   `x` values are returned in uppercase. If `ct_spec` is not provided `x` is
#'   returned unchanged.
#'
#' @examples
#' # A few example terms.
#' terms <-
#'   c("/day",
#'     "Yes",
#'     "Unknown",
#'     "Prior",
#'     "Every 2 hours",
#'     "Percentage",
#'     "International Unit")
#'
#' # Load a controlled terminology example
#' (ct_spec <- read_ct_spec_example("ct-01-cm"))
#'
#' # Use all possible matching terms in the controlled terminology.
#' ct_map(x = terms, ct_spec = ct_spec)
#'
#' # Note that if the controlled terminology mapping is restricted to a codelist
#' # code, e.g. C71113, then only `"/day"` gets mapped to `"QD"`; remaining terms
#' # won't match given the codelist code restriction, and will be mapped to an
#' # uppercase version of the original terms.
#' ct_map(x = terms, ct_spec = ct_spec, ct_cltc = "C71113")
#'
#' @importFrom rlang %||% .data
#' @export
ct_map <-
  function(x,
           ct_spec = NULL,
           ct_cltc = NULL,
           from = ct_spec_vars("from"),
           to = ct_spec_vars("to")) {
    ct_spec %||% return(x)
    assert_ct_spec(ct_spec)

    ct_cltc <- ct_cltc %||% unique(ct_spec[[ct_spec_vars("ct_cltc")]])
    ct_spec <- dplyr::filter(ct_spec, .data[[ct_spec_vars("ct_cltc")]] %in% ct_cltc)

    mappings <- ct_mappings(ct_spec, from = from, to = to)
    recode(
      x,
      from = mappings$from,
      to = mappings$to,
      .no_match = toupper(x)
    )
  }

#' Read in a controlled terminology
#'
#' [read_ct_spec()] imports a controlled terminology specification data set as a
#' [tibble][tibble::tibble-package].
#'
#' @param file A path to a file containing a controlled terminology
#'   specification data set. The following are expected of this file:
#'
#' - The file is expected to be a CSV file;
#' - The file is expected to contain a first row of column names;
#' - This minimal set of variables is expected: `r knitr::combine_words(ct_spec_vars())`.
#'
#' @returns A [tibble][tibble::tibble-package] with a controlled terminology
#'   specification.
#'
#' @examples
#' # Get the local path to one of the controlled terminology example files.
#' path <- ct_spec_example("ct-01-cm")
#'
#' # Import it to R.
#' read_ct_spec(file = path)
#'
#' @export
read_ct_spec <- function(file = stop("`file` must be specified")) {
  ct_spec <- readr::read_csv(file = file, col_types = "c")
  assert_ct_spec(ct_spec)

  ct_spec
}

#' Find the path to an example controlled terminology file
#'
#' [ct_spec_example()] resolves the local path to an example controlled
#' terminology file.
#'
#' @param example A string with either the basename, file name, or relative path
#'   to a controlled terminology file bundled with `{stdm.oak}`, see examples.
#'
#' @returns The local path to an example file if `example` is supplied, or a
#'   character vector of example file names.
#'
#' @examples
#' # Get the local path to controlled terminology example file 01
#' # Using the basename only:
#' ct_spec_example("ct-01-cm")
#'
#' # Using the file name:
#' ct_spec_example("ct-01-cm.csv")
#'
#' # Using the relative path:
#' ct_spec_example("ct/ct-01-cm.csv")
#'
#' # If no example is provided it returns a vector of possible choices.
#' ct_spec_example()
#'
#' @export
ct_spec_example <- function(example) {
  # If no example is requested, then return all available files.
  if (missing(example)) {
    ct_spec_path <- system.file("ct", package = "sdtm.oak", mustWork = TRUE)
    ct_spec_files <- list.files(ct_spec_path, pattern = "*.csv")
    return(ct_spec_files)
  }

  # Otherwise, resolve the local path to the example requested.
  admiraldev::assert_character_scalar(example, optional = TRUE)
  base_name <- tools::file_path_sans_ext(basename(example))
  path <- file.path("ct", paste0(base_name, ".csv"))
  local_path <- system.file(path, package = "sdtm.oak")

  if (identical(local_path, "")) {
    stop(
      glue::glue(
        "'{example}' does not match any ct spec files. Run `ct_spec_example()` for options."
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
#' [read_ct_spec_example()] imports one of the bundled controlled terminology
#' specification data sets as a [tibble][tibble::tibble-package] into R.
#'
#' @param example The file name of a controlled terminology data set bundled
#'   with `{stdm.oak}`, run `read_ct_spec_example()` for available example files.
#'
#' @returns A [tibble][tibble::tibble-package] with a controlled terminology
#'   specification data set, or a character vector of example file names.
#'
#' @examples
#' # Leave the `example` parameter as missing for available example files.
#' read_ct_spec_example()
#'
#' # Read an example controlled terminology spec file.
#' read_ct_spec_example("ct-01-cm.csv")
#'
#' # You may omit the file extension.
#' read_ct_spec_example("ct-01-cm")
#'
#' @export
read_ct_spec_example <- function(example) {
  if (missing(example)) {
    return(ct_spec_example())
  } else {
    admiraldev::assert_character_scalar(example)
  }

  path <- ct_spec_example(example)
  read_ct_spec(file = path)
}
