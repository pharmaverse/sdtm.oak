# Title: SDTM domain datasets provided by R package pharmaversesdtm
# Description: This script is used to snatch those SDTM datasets provided by
#              pharmaversesdtm. These include datasets that are therapeutic area
#              (TA)-agnostic (DM, VS, EG, etc.) as well TA-specific ones
#              (RS, TR, OE, etc.). We export them to inst/domain as RDS.
#
# Source: R package pharmaversesdtm
# URL: (https://github.com/pharmaverse/pharmaversesdtm)

library(pharmaversesdtm)
library(readr)
library(here)

path <- here::here("inst/domain")

readr::write_rds(x = ae, file = file.path(path, "ae.rds"), compress = "xz")
readr::write_rds(x = ae_ophtha, file = file.path(path, "ae_ophtha.rds"), compress = "xz")
readr::write_rds(x = ce_vaccine, file = file.path(path, "ce_vaccine.rds"), compress = "xz")
readr::write_rds(x = cm, file = file.path(path, "cm.rds"), compress = "xz")
readr::write_rds(x = dm, file = file.path(path, "dm.rds"), compress = "xz")
readr::write_rds(x = dm_vaccine, file = file.path(path, "dm_vaccine.rds"), compress = "xz")
readr::write_rds(x = ds, file = file.path(path, "ds.rds"), compress = "xz")
readr::write_rds(x = eg, file = file.path(path, "eg.rds"), compress = "xz")
readr::write_rds(x = ex, file = file.path(path, "ex.rds"), compress = "xz")
readr::write_rds(x = ex_ophtha, file = file.path(path, "ex_ophtha.rds"), compress = "xz")
readr::write_rds(x = ex_vaccine, file = file.path(path, "ex_vaccine.rds"), compress = "xz")
readr::write_rds(x = face_vaccine, file = file.path(path, "face_vaccine.rds"), compress = "xz")
readr::write_rds(x = is_vaccine, file = file.path(path, "is_vaccine.rds"), compress = "xz")
readr::write_rds(x = lb, file = file.path(path, "lb.rds"), compress = "xz")
readr::write_rds(x = mh, file = file.path(path, "mh.rds"), compress = "xz")
readr::write_rds(x = oe_ophtha, file = file.path(path, "oe_ophtha.rds"), compress = "xz")
readr::write_rds(x = pc, file = file.path(path, "pc.rds"), compress = "xz")
readr::write_rds(x = pp, file = file.path(path, "pp.rds"), compress = "xz")
readr::write_rds(x = qs_ophtha, file = file.path(path, "qs_ophtha.rds"), compress = "xz")
readr::write_rds(x = rs_onco, file = file.path(path, "rs_onco.rds"), compress = "xz")
readr::write_rds(x = rs_onco_irecist, file = file.path(path, "rs_onco_irecist.rds"), compress = "xz")
