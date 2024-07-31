# sdtm.oak 0.1

The V0.1 release of {sdtm.oak} users can create the majority of the SDTM domains. Domains that are *NOT* in scope for the V0.1 release are DM, Trial Design Domains, SV, SE, RELREC, Associated Person domains, and EPOCH Variable across all domains.

-   Functions for commonly used SDTM mapping Algorithms
    -   assign_no_ct\
    -   assign_ct\
    -   hardcode_no_ct\
    -   hardcode_ct\
    -   assign_datetime\
    -   condition_add\
-  Functions for SDTM derived variables\
    -   Derive Baseline Flag or Last Observation Before Exposure Flag\
    -   Derive the sequence number (--SEQ) variable\
    -   Derive study day\
    -   ISO8601 date, datetime conversion.\
-  Functions to support {sdtm.oak}
    - derive oak id variables
    - Create conditioned dataframes to support if then else conditions in SDTM mappings
    - Import study controlled terminology
-  Articles
    - Algorithms
    - Creating an Events SDTM domain
    - Creating a Findings SDTM domain
    - Conditioned Data Frames
    - Converting dates, times or date-times to ISO 8601
    - Path to Automation

# sdtm.oak 0.0.0.9005 (development version)

-   New function for creating conditioned data frames: `condition_add()`.
-   New pipe operator: `%.>%` for explicit dot placeholder placement.
-   `oak_id_vars()` is now an exported function.

# sdtm.oak 0.0.0.9004 (development version)

-   New function: `derive_seq()` for deriving a sequence number variable.

# sdtm.oak 0.0.0.9003 (development version)

## New Features

-   New function: `assign_datetime()` for deriving an ISO8601 date-time variable.

# sdtm.oak 0.0.0.9002 (development version)

## New Features

-   New function: `derive_study_day()` for study day calculation.

-   New functions for basic SDTM derivations: `assign_no_ct()`, `assign_ct()`, `hardcode_no_ct()` and `hardcode_ct()`.

-   New functions for handling controlled terminologies: `read_ct_spec()`, `read_ct_spec_example()`, `ct_spec_example()` and `ct_map()`.

# sdtm.oak 0.0.0.9001 (development version)

## New Features

-   New function `create_iso8601()` for conversion of vectors of dates, times or date-times to ISO8601 format.
