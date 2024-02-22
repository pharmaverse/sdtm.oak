str_split_ <- function(x, split = ";", quote = '"') {
  scan(
    text = x,
    what = "character",
    sep = split,
    quote = quote,
    quiet = TRUE
  )
}

str_split <- function(x, split = ";", quote = '"') {
  lapply(x, str_split_, split = split, quote = quote)
}
