# new_dttm_tbl <- function(sec = character(),
#                          min = character(),
#                          hour = character(),
#                          mday = character(),
#                          mon = character(),
#                          year = character()) {
#
#   tibble::tibble(
#     year = year,
#     mon = mon,
#     mday = mday,
#     hour = hour,
#     min = min,
#     sec = sec
#   )
# }

# parse_dttm <- function(dttm,
#                        fmt,
#                        na = NULL,
#                        sec_na = na,
#                        min_na = na,
#                        hour_na = na,
#                        mday_na = na,
#                        mon_na = na,
#                        year_na = na) {
#
#
#   tbl_fmt_c <- parse_dttm_fmt(fmt)
#   regex <-
#     dttm_fmt_to_regex(
#       tbl_fmt_c,
#       fmt_regex = fmt_rg(
#         na = na,
#         sec_na = sec_na,
#         min_na = min_na,
#         hour_na = hour_na,
#         mday_na = mday_na,
#         mon_na = mon_na,
#         year_na = year_na
#       )
#     )
#
#   m <- stringr::str_match(dttm, regex)
#   colnames(m)[1] <- "dttm"
#   # Remove unnamed capture groups (these are subgroups within the dttm components)
#   m <- m[, colnames(m) != "", drop = FALSE]
#   dplyr::bind_rows(new_dttm_tbl(), tibble::as_tibble(m))
# }

# convert NA in character x to "-"
# iso8601_na <- function(x) {
#   x[is.na(x)] <- "-"
#   x
# }



# iso8601_mday <- function(x) {
#   iso8601_two_digits(x) |> iso8601_na()
# }
#
# iso8601_hour <- function(x) {
#   iso8601_two_digits(x) |> iso8601_na()
# }
#
# iso8601_min <- function(x) {
#   iso8601_two_digits(x) |> iso8601_na()
# }

# iso8601_sec <- function(x) {
#   x_iso8601 <- stringr::str_extract(x, "^\\d?\\d(\\.\\d*)?$")
#   #x_iso8601 <- sprintf("%02f", x_dbl)
#   x_iso8601 <- stringr::str_replace(x_iso8601, "^\\d(\\.\\d*)?$", "0\\0")
#   x_iso8601 <- stringr::str_replace(x_iso8601, "(\\.[^0]*)(0*)$", "\\1")
#   x_iso8601 <- stringr::str_remove(x_iso8601, "\\.$")
#   x_iso8601[is.na(x_iso8601)] <- NA_character_
#   x_iso8601 |> iso8601_na()
# }

# Month abbreviation (en) to numeric month mapping
# mon_abb_to_mon_num <- setNames(sprintf("%02d", seq_along(month.abb)), tolower(month.abb))

# iso8601_mon <- function(x) {
#
#   x <- tolower(x)
#   # Translate month abbreviations to numeric months
#   num_mon <- mon_abb_to_mon_num[x]
#   num_mon_chr <- num_mon
#   num_mon_chr[is.na(num_mon)] <- iso8601_two_digits(x[is.na(num_mon)])
#
#   mon_int <- as.integer(num_mon_chr)
#   x_iso8601 <- sprintf("%02d", mon_int)
#   x_iso8601[is.na(mon_int)] <- NA_character_
#   iso8601_na(x_iso8601)
# }

# iso8601_year <- function(x, cutoff_2000 = 68L) {
#   x_int <- as.integer(stringr::str_match(x, "^\\d{1,4}$"))
#   x_int <- ifelse (x_int <= cutoff_2000, x_int + 2000L, x_int)
#   x_int <- ifelse (x_int <= 99, x_int + 1900L, x_int)
#
#
#   x_iso8601 <- sprintf("%04d", x_int)
#   x_iso8601[is.na(x_int)] <- NA_character_
#   iso8601_na(x_iso8601)
# }

# iso8601_truncate <- function(x, empty_as_na = TRUE) {
#
#   x <- stringr::str_remove(x, "[^\\d]*$")
#
#   if (empty_as_na) x[x == ""] <- NA_character_
#
#   x
# }

#' #' @importFrom rlang .data
#' #' @export
#' format_iso8601 <- function(dttm, fmt, na = NULL) {
#'
#'   tbl <- parse_dttm(dttm, fmt, na = na)
#'   tbl |>
#'     dplyr::mutate(
#'       year = iso8601_year(.data$year),
#'       mon = iso8601_mon(.data$mon),
#'       mday = iso8601_mday(.data$mday),
#'       hour = iso8601_hour(.data$hour),
#'       min = iso8601_min(.data$min),
#'       sec = iso8601_sec(.data$sec),
#'       iso8601 = dplyr::if_else(
#'         !is.na(dttm),
#'         iso8601_truncate(
#'           stringr::str_glue("{.data$year}-{.data$mon}-{.data$mday}T{.data$hour}:{.data$min}:{.data$sec}")
#'         ),
#'         NA_character_
#'       )
#'     ) |>
#'     dplyr::relocate(.data$iso8601, .data$dttm, .before = 1L)
#'
#' }
