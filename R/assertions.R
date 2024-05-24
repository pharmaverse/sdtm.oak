# Surprisingly, admiraldev doesn't provide `assert_logical_vector`.
assert_logical_vector <- function(arg, optional = FALSE) {
  if (optional && is.null(arg)) {
    return(invisible(arg))
  }

  if (!is.logical(arg)) {
    err_msg <- sprintf(
      "`arg` must be a logical vector but is %s.",
      admiraldev::what_is_it(arg)
    )
    rlang::abort(err_msg)
  }

  invisible(arg)
}
