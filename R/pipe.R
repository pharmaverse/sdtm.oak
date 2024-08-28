#' Explicit Dot Pipe
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This operator pipes an object forward into a function or call expression
#' using an explicit placement of the dot (`.`) placeholder. Unlike magrittr's
#' [%>%][magrittr::%>%] operator, `%.>%` does not automatically place the
#' left-hand side (`lhs`) as the first argument in the right-hand side (`rhs`)
#' call. This operator provides a simpler alternative to the use of braces with
#' magrittr, while achieving similar behavior.
#'
#' @param lhs A value to be piped forward.
#' @param rhs A function call that utilizes the dot (`.`) placeholder to specify
#'   where `lhs` should be placed.
#'
#' @details
#' The `%.>%` operator is used to pipe the `lhs` value into the `rhs` function
#' call. Within the `rhs` expression, the placeholder `.` represents the
#' position where `lhs` will be inserted. This provides more control over where
#' the `lhs` value appears in the `rhs` function call, compared to the magrittr
#' pipe operator which always places `lhs` as the first argument of `rhs`.
#'
#' Unlike magrittr's pipe, which may require the use of braces to fully control
#' the placement of `lhs` in nested function calls, `%.>%` simplifies this by
#' directly allowing multiple usages of the dot placeholder without requiring
#' braces. For example, the following expression using magrittr's pipe and
#' braces:
#'
#' ```r
#' library(magrittr)
#'
#' 1:10 %>% { c(min(.), max(.)) }
#' ```
#'
#' can be written as:
#'
#' ```r
#' 1:10 %.>% c(min(.), max(.))
#' ```
#'
#' without needing additional braces.
#'
#' ## Downside
#'
#' The disadvantage of `%.>%` is that you always need to use
#' the dot placeholder, even when piping to the first argument of the
#' right-hand side (`rhs`).
#'
#' @return No Return Value.
#'
#' @examples
#'
#' # Equivalent to `subset(head(iris), 1:nrow(head(iris)) %% 2 == 0)`
#' head(iris) %.>% subset(., 1:nrow(.) %% 2 == 0)
#'
#' # Equivalent to `c(min(1:10), max(1:10))`
#' 1:10 %.>% c(min(.), max(.))
#'
#' @rdname dot_pipe
#' @export
`%.>%` <- function(lhs, rhs) {
  rhs_expr <- rlang::enexpr(rhs)
  if (!contains_dot(rhs_expr)) {
    cli::cli_abort("The right-hand side (rhs) of `%.>%` must contain at least one dot (.) placeholder.")
  }

  rlang::eval_tidy(rhs_expr, list(. = lhs), env = rlang::caller_env())
}

# Recursively find if an expression contains a dot.
contains_dot <- function(expr) {
  rlang::is_symbol(expr) && identical(expr, rlang::sym(".")) ||
    rlang::is_call(expr) && purrr::some(as.list(expr), contains_dot)
}
