#' Date/time collection formats
#'
#' @format A [tibble][tibble::tibble-package] of `r nrow(dtc_formats)` formats
#'   with three variables:
#' \describe{
#'   \item{`fmt`}{Format string.}
#'   \item{`type`}{Whether a date, time or date-time.}
#'   \item{`description`}{Description of which date-time components are parsed.}
#' }
#'
#' @examples
#' dtc_formats
#'
"dtc_formats"
