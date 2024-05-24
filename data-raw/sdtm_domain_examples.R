# Title: SDTM domain example datasets.

library(pharmaversesdtm)
library(readr)
library(here)

path <- here::here("inst/domain")

vs <- tibble::tribble(
  ~STUDYID, ~DOMAIN,      ~USUBJID,                  ~VSSPID, ~VSTESTCD,             ~VSDTC, ~VSTPTNUM,
  "ABC123",    "VS",  "ABC123-375", "/F:VTLS1-D:9795532-R:2", "DIABP", "2020-09-01T13:31",  NA,
  "ABC123",    "VS",  "ABC123-375", "/F:VTLS1-D:9795532-R:2", "TEMP",  "2020-09-01T13:31",  NA,
  "ABC123",    "VS",  "ABC123-375", "/F:VTLS2-D:9795533-R:2", "DIABP", "2020-09-28T11:00",  2L,
  "ABC123",    "VS",  "ABC123-375", "/F:VTLS2-D:9795533-R:2", "TEMP",  "2020-09-28T11:00",  2L,
  "ABC123",    "VS",  "ABC123-376", "/F:VTLS1-D:9795591-R:1", "DIABP", "2020-09-20",        NA,
  "ABC123",    "VS",  "ABC123-376", "/F:VTLS1-D:9795591-R:1", "TEMP",  "2020-09-20",        NA
)

apsc <- tibble::tribble(
  ~STUDYID, ~RSUBJID,     ~SCTESTCD,  ~DOMAIN, ~SREL,     ~SCCAT,
  "ABC123", "ABC123-210", "LVSBJIND", "APSC",  "FRIEND",  "CAREGIVERSTUDY",
  "ABC123", "ABC123-210", "EDULEVEL", "APSC",  "FRIEND",  "CAREGIVERSTUDY",
  "ABC123", "ABC123-210", "TMSPPT",   "APSC",  "FRIEND",  "CAREGIVERSTUDY",
  "ABC123", "ABC123-211", "CAREDUR",  "APSC",  "SIBLING", "CAREGIVERSTUDY",
  "ABC123", "ABC123-211", "LVSBJIND", "APSC",  "SIBLING", "CAREGIVERSTUDY",
  "ABC123", "ABC123-212", "JOBCLAS",  "APSC",  "SPOUSE",  "CAREGIVERSTUDY"
)

readr::write_rds(x = pharmaversesdtm::ae, file = file.path(path, "ae.rds"), compress = "xz")
readr::write_rds(x = pharmaversesdtm::cm, file = file.path(path, "cm.rds"), compress = "xz")
readr::write_rds(x = vs, file = file.path(path, "vs.rds"), compress = "xz")
readr::write_rds(x = apsc, file = file.path(path, "apsc.rds"), compress = "xz")
