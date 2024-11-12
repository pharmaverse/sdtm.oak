# sdtm.oak V0.1.1

Fixed the bug that was causing test cases to fail. Updated derive_blfl function to account for baseline_timepoints. Typo updates in the documention.

# sdtm.oak V0.1.0

The V0.1.0 release of {sdtm.oak} users can create the majority of the SDTM domains. Domains that are *NOT* in scope for the V0.1.0 release are DM, Trial Design Domains, SV, SE, RELREC, Associated Person domains, and EPOCH Variable across all domains.

-   Functions for commonly used SDTM mapping Algorithms
    -   `assign_no_ct()` to process assign_no_ct algorithm
    -   `assign_ct()` to process assign_ct algorithm
    -   `hardcode_no_ct()`  to process hardcode_no_ct algorithm
    -   `hardcode_ct()`  to process hardcode_ct algorithm
    -   `assign_datetime()` to process assign_datetime algorithm
    -   `condition_add()`  to process condition_add algorithm (if/else conditions)
-  Functions for SDTM derived variables
    -   `derive_blfl()` to Derive Baseline Flag or Last Observation Before Exposure Flag
    -   `derive_seq()` to Derive the sequence number (--SEQ) variable
    -   `derive_study_day()` to Derive study day
    -   `create_iso8601()` for ISO8601 date, datetime conversion.
-  Functions to support {sdtm.oak}
    - ` generate_oak_id_vars()` to derive oak id variables
    - `read_ct_spec()` to read the controlled terminology spec
    -  Functions to create conditioned dataframes to support if then else conditions in SDTM mappings
-  Articles
    - Algorithms
    - Creating an interventions SDTM domain
    - Creating a Findings SDTM domain
    - Conditioned Data Frames
    - Converting dates, times or date-times to ISO 8601
    - Path to Automation

## Further details on this Release

-   New function for creating conditioned data frames: `condition_add()`.
-   New pipe operator: `%.>%` for explicit dot placeholder placement.
-   `oak_id_vars()` is now an exported function.
-   New function: `derive_seq()` for deriving a sequence number variable.
-   New function: `assign_datetime()` for deriving an ISO8601 date-time variable.
-   New function: `derive_study_day()` for study day calculation.
-   New functions for basic SDTM derivations: `assign_no_ct()`, `assign_ct()`, `hardcode_no_ct()` and `hardcode_ct()`.
-   New functions for handling controlled terminologies: `read_ct_spec()`, `read_ct_spec_example()`, `ct_spec_example()` and `ct_map()`.
-   New function `create_iso8601()` for conversion of vectors of dates, times or date-times to ISO8601 format.
