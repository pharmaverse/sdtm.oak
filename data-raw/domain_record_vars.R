# Record variables for SDTM domains
# Original file provided by Edgar Manukyan (Roche).
# Context: https://github.com/pharmaverse/sdtm.oak/issues/15
# Source URL: https://github.com/pharmaverse/sdtm.oak/files/15326343/domain_key_variables.2.csv
#
# Author: Ramiro Magno (Pattern Institute)
#
library(tidyverse)
path <- here::here("data-raw/domain_record_vars.csv.xz")

tbl <-
  readr::read_csv(path, col_names = TRUE, col_types = "cc") |>
  dplyr::mutate(key_variables = strsplit(key_variables, "|", fixed = TRUE))

domain_record_vars <- setNames(tbl$key_variables, tbl$domain)

# `domain_record_vars` is an internal data set but can be accessed via
# `rec_vars()`.
usethis::use_data(domain_record_vars, internal = TRUE)
