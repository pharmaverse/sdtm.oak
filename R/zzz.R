.onLoad <- function(libname, pkgname) {
  ct_mappings <<- memoise::memoise(ct_mappings)
}
