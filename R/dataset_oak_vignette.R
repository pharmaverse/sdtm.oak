#' Output a Dataset in a Vignette in the sdtm.oak Format
#'
#' Output a dataset in a vignette with the pre-specified sdtm.oak format.
#'
#' @param dataset Dataset to output in the vignette
#'
#' @param display_vars Variables selected to demonstrate the outcome of the mapping
#'
#' Permitted Values: list of variables
#'
#' Default is NULL
#'
#' If `display_vars` is not NULL, only the selected variables are visible in the vignette while the
#' other variables are hidden. They can be made visible by clicking the`Choose the columns to
#'  display` button.
#'
#' @param filter Filter condition
#'
#' The specified condition is applied to the dataset before it is displayed.
#'
#' Permitted Values: a condition
#'
#' @return A HTML table
#'
#' @keywords dev_utility
#' @importFrom rlang exprs
#'
#' @keywords internal
#'
dataset_oak_vignette <- function(dataset, display_vars = NULL, filter = NULL) {
  filter <- admiraldev::assert_filter_cond(rlang::enexpr(filter), optional = TRUE)

  out <- dataset |>
    admiraldev::filter_if(filter) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))

  # Create a short markdown table when this function is called outside {pkgdown}
  if (!identical(Sys.getenv("IN_PKGDOWN"), "true")) {
    if (is.null(display_vars)) {
      return(knitr::kable(utils::head(out, 10L)))
    } else {
      return(knitr::kable(utils::head(dplyr::select(out, !!!display_vars), 10L)))
    }
  }

  if (!is.null(display_vars)) {
    hide_columns <- which(!(colnames(out) %in% admiraldev::vars2chr(display_vars)))
    cols_to_hide <- list(list(targets = hide_columns - 1L, visible = FALSE))
  } else {
    cols_to_hide <- list()
  }
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "dt-scroll",
      version = "1.0.0",
      src = "www",
      stylesheet = "style.css",
      package = "sdtm.oak"
    ),
    DT::datatable(
      out,
      rownames = FALSE,
      filter = "top",
      height = "auto",
      width = "auto",
      extensions = c("Buttons", "ColReorder", "Scroller"),
      options = list(
        columnDefs = cols_to_hide,
        searchHighlight = TRUE,
        searching = TRUE,
        pageLength = 5L,
        lengthMenu = c(5L, 10L, 15L, 20L, 50L, 100L),
        dom = "<Bfr<\"dt-scroll\"t>ipl>",
        buttons = list(list(
          extend = "colvis",
          text = "Choose the columns to display",
          scroller = TRUE,
          collectionLayout = "fixed two-column"
        )),
        colReorder = TRUE
      )
    )
  )
}
