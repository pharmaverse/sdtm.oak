# # pseq <-
# #   function(from = 1,
# #            to = 1) {
# #       mapply(
# #         `:`,
# #         from = from,
# #         to = to,
# #         SIMPLIFY = FALSE
# #       ) |>
# #       unlist()
# #   }
#
# find_gaps <- function(x, x_min, x_max) {
#   y <- setdiff(seq(x_min, x_max), x)
#   streaks <- split(y, cumsum(c(TRUE, diff(y) != 1)))
#
#   lapply(X = streaks, \(x) c(start = min(x), end = max(x)))
# }
#
# fmt_tokens <-
#   list(
#     sec = "S+",
#     min = "M+",
#     hour = "H+",
#     mday = "d+",
#     mon = "m+",
#     year = "y+"
#   )
#
# fmt_regex <-
#   list(
#     sec = "(\\d{2})",
#     min = "(\\d{2})",
#     hour = "(\\d{2})",
#     mday = "(\\d{2})",
#     mon = "(\\d{2}|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)",
#     year = "(\\d{4})"
#   )
#
#
# as_order <- function(fmt) {
#   gsub("[^[:alpha:]]+", "", fmt)
# }
#
# fmt_token_tally <- function(fmt, tokens = fmt_tokens) {
#   sapply(fmt_tokens, \(x) stringr::str_count(fmt, x))
# }
#
# is_dt_fmt <- function(fmt) {
#
#   tally <- fmt_token_tally(fmt)
#   all(tally < 2) && sum(tally) > 0
# }
#
# reg_matches <- function(x, m, invert = FALSE) {
#   match <- regmatches(x, m, invert = invert)
#   ifelse(length(match), match, NA_character_)
# }
#
# # parse_dt_fmt <- function(fmt, tokens = fmt_tokens) {
# #   if (!is_dt_fmt(fmt)) {
# #    stop("Not a valid format in `fmt`.", call. = TRUE)
# #   }
# #   sapply(fmt_tokens, \(x) stringr::str_match(fmt, x))
# # }
# #
# # parse_dt_fmt2 <- function(fmt, tokens = fmt_tokens) {
# #   if (!is_dt_fmt(fmt)) {
# #     stop("Not a valid format in `fmt`.", call. = TRUE)
# #   }
# #   sapply(fmt_tokens, \(x) grep(x, fmt))
# # }
#
# extract_tidy_match_ <- function(x, pattern) {
#
#   match_data <- regexpr(pattern, x)
#   match <- reg_matches(x, match_data)
#
#   is_match <- !is.na(match)
#
#   start <- ifelse(is_match, match_data, NA_integer_)
#   len <- ifelse(is_match, attr(match_data, "match.length"), NA_integer_)
#   end <- start + len - 1L
#   data.frame(pat = pattern, cap = match, start = start, end = end, len = len)
# }
#
# extract_tidy_match <- function(x, patterns, sort = TRUE) {
#
#   df <-
#     lapply(patterns, \(str) extract_tidy_match_(x, str)) |>
#     do.call(what = "rbind")
#
#   df$ord <- rank(df$start)
#   df$ord[is.na(df$start)] <- NA_integer_
#
#   df <- cbind(dttm_el = rownames(df), df)
#   rownames(df) <- NULL
#
#   if (sort) {
#     df[order(df$ord),]
#   } else {
#     df
#   }
# }
#
#
#
#
# parse_dttm_fields <- function(x,
#                               fmt,
#                               na =
#                                 list(
#                                   year = "UNKN",
#                                   month = "UNK",
#                                   day = "UN",
#                                   hour = "",
#                                   min = "",
#                                   sec = ""
#                                 )) {
#
#
#
#
#
# }
