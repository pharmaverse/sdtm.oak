R_SPLIT = function(domain_dataset,max_length_out = 200){
  out_n = outt = NULL

  #filtering columns > 200
  char_200 = domain_dataset %>% select_if(~ max(nchar(.)) >= max_length_out)

  #string split function
  split_var <- function(string,max_length_out = 200) {

    # Pattern spot
    pattern = names(which.max(table(str_extract_all(string, "[:punct:]|[:blank:]")))) %>%
      ifelse(is.null(.), "",.)

    # Split the input string into a vector
    split_vector <- unlist(stringr::str_split(string, pattern))

    # Function to concatenate strings and split when length exceeds 200
    split_when_needed <- function(result, item, sep, max_length_out) {
      current <- utils::tail(result, 1)
      if (nchar(paste0(current, sep, item)) <= (max_length_out - 1)) {
        result[length(result)] <- paste0(current, sep, item)
      } else {
        if (!identical(sep, " ")) result[length(result)] <- paste0(current, sep)
        result <- c(result, item)
      }
      result
    }

    # Use reduce to apply the function across the vector
    split_vector <- split_vector[-1] |>
      purrr::reduce(split_when_needed, .init = list(split_vector[1]), pattern, max_length_out) |>
      unlist()

    # Fix case where sentence do not exceed max_length_out
    last_two <- paste0(utils::tail(split_vector, n = 2), collapse = if (identical(pattern, " ")) " " else "")
    if (nchar(last_two) <= max_length_out) {
      split_vector <- c(
        utils::head(split_vector, n = -1),
        last_two
      )
    }
    return(as.list(split_vector))
  }

  #FUNCTION CALL
  outt <- map(char_200, ~ {
    split_list <- map(.x, ~ {
      cv <- as.data.frame(split_var(.x, max_length_out))
      names(cv) <- seq_along(cv)
      cv
    })
    split_df <- bind_rows(split_list)
    split_df
  }) %>% imap(.,~set_names(.x,.y)) %>% bind_cols()

  names(outt) = sub("....$","",names(outt)) %>% make.unique(., sep = "_")
  dataset_OUT = bind_cols(domain_dataset %>% select(-names(char_200)),outt)
  return(dataset_OUT)
}

