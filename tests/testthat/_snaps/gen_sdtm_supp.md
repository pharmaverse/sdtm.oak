# `gen_sdtm_supp` works as expected

    Code
      jsonlite::toJSON(final, pretty = TRUE, auto_unbox = TRUE)
    Output
      {
        "DM": [
          {
            "STUDYID": "CDISCPILOT01",
            "DOMAIN": "DM",
            "USUBJID": "01-701-1015",
            "SUBJID": "1015",
            "RFSTDTC": "2014-01-02",
            "RFENDTC": "2014-07-02",
            "RFXSTDTC": "2014-01-02",
            "RFXENDTC": "2014-07-02",
            "RFPENDTC": "2014-07-02T11:45",
            "SITEID": "701",
            "AGE": 63,
            "AGEU": "YEARS",
            "SEX": "F",
            "RACE": "WHITE",
            "ETHNIC": "HISPANIC OR LATINO",
            "ARMCD": "Pbo",
            "ARM": "Placebo",
            "ACTARMCD": "Pbo",
            "ACTARM": "Placebo",
            "COUNTRY": "USA",
            "DMDTC": "2013-12-26",
            "DMDY": -7
          },
          {
            "STUDYID": "CDISCPILOT01",
            "DOMAIN": "DM",
            "USUBJID": "01-701-1023",
            "SUBJID": "1023",
            "RFSTDTC": "2012-08-05",
            "RFENDTC": "2012-09-02",
            "RFXSTDTC": "2012-08-05",
            "RFXENDTC": "2012-09-01",
            "RFPENDTC": "2013-02-18",
            "SITEID": "701",
            "AGE": 64,
            "AGEU": "YEARS",
            "SEX": "M",
            "RACE": "WHITE",
            "ETHNIC": "HISPANIC OR LATINO",
            "ARMCD": "Pbo",
            "ARM": "Placebo",
            "ACTARMCD": "Pbo",
            "ACTARM": "Placebo",
            "COUNTRY": "USA",
            "DMDTC": "2012-07-22",
            "DMDY": -14
          },
          {
            "STUDYID": "CDISCPILOT01",
            "DOMAIN": "DM",
            "USUBJID": "01-701-1028",
            "SUBJID": "1028",
            "RFSTDTC": "2013-07-19",
            "RFENDTC": "2014-01-14",
            "RFXSTDTC": "2013-07-19",
            "RFXENDTC": "2014-01-14",
            "RFPENDTC": "2014-01-14T11:10",
            "SITEID": "701",
            "AGE": 71,
            "AGEU": "YEARS",
            "SEX": "M",
            "RACE": "WHITE",
            "ETHNIC": "NOT HISPANIC OR LATINO",
            "ARMCD": "Xan_Hi",
            "ARM": "Xanomeline High Dose",
            "ACTARMCD": "Xan_Hi",
            "ACTARM": "Xanomeline High Dose",
            "COUNTRY": "USA",
            "DMDTC": "2013-07-11",
            "DMDY": -8
          },
          {
            "STUDYID": "CDISCPILOT01",
            "DOMAIN": "DM",
            "USUBJID": "01-701-1033",
            "SUBJID": "1033",
            "RFSTDTC": "2014-03-18",
            "RFENDTC": "2014-04-14",
            "RFXSTDTC": "2014-03-18",
            "RFXENDTC": "2014-03-31",
            "RFPENDTC": "2014-09-15",
            "SITEID": "701",
            "AGE": 74,
            "AGEU": "YEARS",
            "SEX": "M",
            "RACE": "WHITE",
            "ETHNIC": "NOT HISPANIC OR LATINO",
            "ARMCD": "Xan_Lo",
            "ARM": "Xanomeline Low Dose",
            "ACTARMCD": "Xan_Lo",
            "ACTARM": "Xanomeline Low Dose",
            "COUNTRY": "USA",
            "DMDTC": "2014-03-10",
            "DMDY": -8
          },
          {
            "STUDYID": "CDISCPILOT01",
            "DOMAIN": "DM",
            "USUBJID": "01-701-1034",
            "SUBJID": "1034",
            "RFSTDTC": "2014-07-01",
            "RFENDTC": "2014-12-30",
            "RFXSTDTC": "2014-07-01",
            "RFXENDTC": "2014-12-30",
            "RFPENDTC": "2014-12-30T09:50",
            "SITEID": "701",
            "AGE": 77,
            "AGEU": "YEARS",
            "SEX": "F",
            "RACE": "WHITE",
            "ETHNIC": "NOT HISPANIC OR LATINO",
            "ARMCD": "Xan_Hi",
            "ARM": "Xanomeline High Dose",
            "ACTARMCD": "Xan_Hi",
            "ACTARM": "Xanomeline High Dose",
            "COUNTRY": "USA",
            "DMDTC": "2014-06-24",
            "DMDY": -7
          }
        ],
        "SUPPDM": [
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1015",
            "QNAM": "COMPLT16",
            "QLABEL": "Completers of Week 16 Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1015",
            "QNAM": "COMPLT24",
            "QLABEL": "Completers of Week 16 Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1015",
            "QNAM": "COMPLT8",
            "QLABEL": "Completers of Week 16 Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1015",
            "QNAM": "EFFICACY",
            "QLABEL": "Efficacy Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1015",
            "QNAM": "ITT",
            "QLABEL": "Intent to Treat Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1015",
            "QNAM": "SAFETY",
            "QLABEL": "Safety Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1023",
            "QNAM": "EFFICACY",
            "QLABEL": "Efficacy Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1023",
            "QNAM": "ITT",
            "QLABEL": "Intent to Treat Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1023",
            "QNAM": "SAFETY",
            "QLABEL": "Safety Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1028",
            "QNAM": "COMPLT16",
            "QLABEL": "Completers of Week 16 Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1028",
            "QNAM": "COMPLT24",
            "QLABEL": "Completers of Week 16 Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1028",
            "QNAM": "COMPLT8",
            "QLABEL": "Completers of Week 16 Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1028",
            "QNAM": "EFFICACY",
            "QLABEL": "Efficacy Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1028",
            "QNAM": "ITT",
            "QLABEL": "Intent to Treat Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1028",
            "QNAM": "SAFETY",
            "QLABEL": "Safety Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1033",
            "QNAM": "EFFICACY",
            "QLABEL": "Efficacy Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1033",
            "QNAM": "ITT",
            "QLABEL": "Intent to Treat Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1033",
            "QNAM": "SAFETY",
            "QLABEL": "Safety Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1034",
            "QNAM": "COMPLT16",
            "QLABEL": "Completers of Week 16 Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1034",
            "QNAM": "COMPLT24",
            "QLABEL": "Completers of Week 16 Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1034",
            "QNAM": "COMPLT8",
            "QLABEL": "Completers of Week 16 Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1034",
            "QNAM": "EFFICACY",
            "QLABEL": "Efficacy Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1034",
            "QNAM": "ITT",
            "QLABEL": "Intent to Treat Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          },
          {
            "STUDYID": "CDISCPILOT01",
            "RDOMAIN": "DM",
            "USUBJID": "01-701-1034",
            "QNAM": "SAFETY",
            "QLABEL": "Safety Population Flag",
            "QVAL": "Y",
            "QORIG": "DERIVED"
          }
        ]
      } 

# `gen_sdtm_supp` input validation works

    Argument `idvar` must be a scalar of class <character>, but is NULL.

---

    Argument `spec` must be class <data.frame>, but is a string.

---

    Required variable `Var` is missing in `spec`

---

    Argument `label_var` must be a scalar of class <character>, but is length 2

---

    Required variable `COMPLT16` is missing in `sdtm_dataset`

