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
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["DOMAIN", "oak_id", "raw_source", "patient_number", "USUBJID", "VSDTC", "VSTESTCD", "VSORRES", "VSSTAT", "VSLOBXFL"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["VS", "VS", "VS", "VS", "VS", "VS", "VS", "VS", "VS"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 1, 2, 1, 2, 1, 1, 2]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["VTLS1", "VTLS1", "VTLS1", "VTLS1", "VTLS2", "VTLS2", "VTLS1", "VTLS1", "VTLS1"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [375, 375, 375, 375, 375, 375, 376, 376, 376]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["test_study-375", "test_study-375", "test_study-375", "test_study-375", "test_study-375", "test_study-375", "test_study-376", "test_study-376", "test_study-376"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["2020-09-01T13:31", "2020-10-01T11:20", "2020-09-28T10:10", "2020-10-01T13:31", "2020-09-28T10:10", "2020-09-28T10:05", "2020-09-20", "2020-09-20", "2020-09-20"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["DIABP", "DIABP", "PULSE", "PULSE", "SYSBP", "SYSBP", "DIABP", "PULSE", "PULSE"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["90", "90", "ND", "85", "120", "120", "75", null, "110"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, null, null, null, null, null, "NOT DONE", null]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Y", null, null, null, null, "Y", "Y", null, "Y"]
        }
      ]
    }

# derive_blfl sdmt_in validations work

    Required variable `DOMAIN` is missing

---

    Required variables `oak_id`, `raw_source` and `patient_number` are missing

---

    Required variables `VSORRES`, `VSSTAT`, `VSTESTCD` and `VSDTC` are missing

# derive_blfl dm_domain validations work

    Required variables `USUBJID` and `RFXSTDTC` are missing

# derive_blfl tgt_var and ref_var validations work

    `tgt_var` must be a character scalar but is a list

---

    `ref_var` must be a character scalar but is a data frame

---

    `tgt_var` must be one of 'VSBLFL' or 'VSLOBXFL' but is 'DMLOBXFL'

# derive_blfl DOMAIN validation works

    `domain` must be a character scalar but is `4`

