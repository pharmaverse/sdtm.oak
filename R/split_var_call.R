#' Split a single string
#'
#' @param result A list to store the result
#' @param item The item to be added to the result
#' @param sep The separator used to split the string
#' @param max_length_out The maximum length of the output string
#' @return A list with the split strings
single_str_spilt <- function(result, item, sep, max_length_out) {
  current <- tail(result, 1L)
  if (nchar(paste0(current, sep, item)) <= (max_length_out - 1L)) {
    result[length(result)] <- paste0(current, sep, item)
  } else {
    if (!identical(sep, " ")) result[length(result)] <- paste0(current, sep)
    result <- c(result, item)
  }
  return(result)
}

#' Split a variable
#'
#' @param string The string to be split
#' @param max_length_out The maximum length of the output string (default is 200)
#' @return A list with the split strings
split_var <- function(string, max_length_out = 200L) {
  # Pattern spot
  pattern <- names(which.max(table(stringr::str_extract_all(string, "[:punct:]|[:blank:]")))) %>%
    ifelse(is.null(.), "", .)

  # Split the input string into a vector
  split_vector <- unlist(stringr::str_split(string, pattern))

  # Use reduce to apply the function across the vector
  split_vector <- split_vector[-1L] %>%
    purrr::reduce(single_str_spilt, .init = list(split_vector[1L]), pattern, max_length_out) %>%
    unlist()

  # Fix case where sentence do not exceed max_length_out
  last_two <- paste0(tail(split_vector, n = 2L), collapse = if (identical(pattern, " ")) " " else "")
  if (nchar(last_two) <= max_length_out) {
    split_vector <- c(
      head(split_vector, n = -1L),
      last_two
    )
  }
  return(as.list(split_vector))
}

#' Split a dataset string
#'
#' @param domain_dataset The dataset to be split
#' @param max_length_out The maximum length of output string (default is 200)
#' @return A dataset with the split strings
sdtm_str_split <- function(domain_dataset, max_length_out = 200L) {
  outt <- NULL

  # Filtering columns > 200
  char_200 <- dplyr::select_if(domain_dataset, ~ max(nchar(.)) >= max_length_out)

  # FUNCTION CALL
  outt <- purrr::map(char_200, ~ {
    split_list <- purrr::map(.x, ~ {
      cv <- as.data.frame(split_var(.x, max_length_out))
      names(cv) <- seq_along(cv)
      cv
    })
    split_df <- dplyr::bind_rows(split_list)
    return(split_df)
  }) %>%
    purrr::imap(., ~ set_names(.x, .y)) %>%
    dplyr::bind_cols()

  names(outt) <- sub("....$", "", names(outt)) %>% make.unique(., sep = "_")
  dataset_out <- dplyr::bind_cols(dplyr::select(domain_dataset, -names(char_200)), outt)
  return(dataset_out)
}
