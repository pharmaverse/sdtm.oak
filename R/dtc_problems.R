add_problems <- function(x, dtc) {
  is_x_na <- is.na(x)
  if (!any(is_x_na)) {
    return(x)
  }

  names <- names(dtc)
  bad_names <- duplicated(names) | names == ""
  compat_names <- paste0("..var", seq_along(dtc))

  if (is.null(names)) {
    names <- compat_names
  } else {
    names[bad_names] <- compat_names[bad_names]
  }

  names(dtc) <- names

  index <- which(is_x_na)
  problems <- tibble::as_tibble(dtc)[is_x_na, ]
  problems <- tibble::add_column(problems, ..i = index, .before = 1L)
  attr(x, "problems") <- problems
  x
}

problems <- function(x = .Last.value) {
  probs <- attr(x, "problems")
  if (!is.null(probs)) {
    probs
  } else {
    invisible(NULL)
  }
}

n_problems <- function(x) {
  probs <- problems(x)
  if (is.null(probs)) {
    return(0L)
  } else {
    nrow(probs)
  }
}

warn_problems <- function(x) {
  n_probs <- n_problems(x)
  if (n_probs > 0) {
    msg <- paste(
      sprintf("There were parsing %d problems.", n_probs),
      "Run `problems()` on parsed results for details."
    )
    rlang::warn(msg)
    invisible(NULL)
  } else {
    invisible(NULL)
  }
}
