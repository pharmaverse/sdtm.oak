sort_by <- function(vars = oak_id_vars(), order = "asc") {

  order <- rlang::arg_match(order, values = c("asc", "desc"))
  recycled_args <- vctrs::vec_recycle_common(vars = vars, order = order)

  # dplyr::if_else(order == "asc", recycled_args$vars, rlang::expr(desc(!!recycled_args$vars)))

  purrr::map2(.x = recycled_args$order, .y = rlang::syms(recycled_args$vars), rlang::call2)

  # rlang::exprs(!!!recycled_args)
  # return(recycled_args)
  # rlang::exprs(!!!vars)

}
