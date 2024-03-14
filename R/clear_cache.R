#' Clear `{sdtm.oak}` cache of memoised functions
#'
#' @description
#' Some of `{sdtm.oak}` functions have their results cached for runtime
#' efficiency. Use this function to reset the cache.
#'
#' Memoised functions:
#' - [ct_mappings()]
#'
#' @return Returns a logical value, indicating whether the resetting of the
#'   cache was successful (`TRUE`) or not (`FALSE`).
#'
#' @examples
#' clear_cache()
#'
#' @export
clear_cache <- function() {
  memoise::forget(ct_mappings)
}
