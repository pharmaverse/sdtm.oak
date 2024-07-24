# Surprisingly, admiraldev doesn't provide `assert_logical_vector`.
assert_logical_vector <- function(arg, optional = FALSE) {
  if (optional && is.null(arg)) {
    return(invisible(arg))
  }

  if (!is.logical(arg)) {
    cli::cli_abort("`arg` must be a logical vector but is {.obj_type_friendly {arg}}")
  }

  invisible(arg)
}
