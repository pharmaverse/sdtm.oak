# derive_blfl example works

    {
      "type": "list",
      "attributes": {
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["DOMAIN", "oak_id", "raw_source", "patient_number", "USUBJID", "VSDTC", "VSTESTCD", "VSORRES", "VSSTAT", "VISIT", "VSLOBXFL"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["VS", "VS", "VS", "VS", "VS", "VS", "VS", "VS", "VS", "VS", "VS"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 1, 2, 1, 2, 1, 1, 2, 2, 3]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["VTLS1", "VTLS1", "VTLS1", "VTLS1", "VTLS2", "VTLS2", "VTLS1", "VTLS1", "VTLS1", "VTLS1", "VTLS1"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [375, 375, 375, 375, 375, 375, 376, 376, 376, 378, 378]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["test_study-375", "test_study-375", "test_study-375", "test_study-375", "test_study-375", "test_study-375", "test_study-376", "test_study-376", "test_study-376", "test_study-378", "test_study-378"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["2020-09-01T13:31", "2020-10-01T11:20", "2020-09-28T10:10", "2020-10-01T13:31", "2020-09-28T10:10", "2020-09-28T10:05", "2020-09-20", "2020-09-20", "2020-09-20", "2020-01-20T10:00", "2020-01-21T11:00"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["DIABP", "DIABP", "PULSE", "PULSE", "SYSBP", "SYSBP", "DIABP", "PULSE", "PULSE", "PULSE", "PULSE"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["90", "90", "ND", "85", "120", "120", "75", null, "110", "110", "105"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, null, null, null, null, null, "NOT DONE", null, null, null]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["SCREENING", "SCREENING", "SCREENING", "SCREENING", "SCREENING", "SCREENING", "SCREENING", "SCREENING", "SCREENING", "SCREENING", "SCREENING"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Y", null, null, null, "Y", null, "Y", null, "Y", "Y", null]
        }
      ]
    }

# derive_blfl sdmt_in validations work

    Required variable `DOMAIN` is missing in `sdtm_in`

---

    Required variables `oak_id`, `raw_source`, and `patient_number` are missing in `sdtm_in`

---

    Required variables `VSORRES`, `VSSTAT`, `VSTESTCD`, and `VSDTC` are missing in `sdtm_in`

# derive_blfl dm_domain validations work

    Required variables `USUBJID` and `RFXSTDTC` are missing in `dm_domain`

# derive_blfl tgt_var and ref_var validations work

    Argument `tgt_var` must be a scalar of class <character>, but is a list.

---

    Argument `ref_var` must be a scalar of class <character>, but is a tibble.

---

    Argument `tgt_var` must be equal to one of "VSBLFL" or "VSLOBXFL".

# derive_blfl DOMAIN validation works

    Argument `domain` must be a scalar of class <character>, but is an integer.

