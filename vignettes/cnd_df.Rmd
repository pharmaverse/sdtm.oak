---
title: "Conditioned Data Frames"
output: 
  rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Conditioned Data Frames}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(sdtm.oak)
library(tibble)
```

## Introduction

Conditioned data frames, or `cnd_df`, are a powerful tool in the `{sdtm.oak}`
package designed to facilitate conditional transformations on data frames. This
article explains how to create and use conditioned data frames, particularly in
the context of SDTM domain derivations.

## Creating Conditioned Data Frames

A conditioned data frame is a regular data frame extended with a logical vector
`cnd` that marks rows for subsequent conditional transformations. The
`condition_add()` function is used to create these conditioned data frames.

### Simple Example

Consider a simple data frame `df`:

```{r}
(df <- tibble(x = 1L:3L, y = letters[1L:3L]))
```

We can create a conditioned data frame where only rows where `x > 1` are marked:

```{r}
(cnd_df <- condition_add(dat = df, x > 1L))
```

Here, only the second and third rows are marked as `TRUE`.

## Usage in SDTM Domain Derivations

The real power of conditioned data frames manifests when they are used with
functions such as `assign_no_ct`, `assign_ct`, `hardcode_no_ct`, and
`hardcode_ct`. These functions perform derivations only for the records that
match the pattern of `TRUE` values in conditioned data frames.

### Example with Concomitant Medications (CM) Domain

Consider a simplified dataset of concomitant medications, where we want to
derive a new variable CMGRPID (Concomitant Medication Group ID) based on the
condition that the medication treatment (CMTRT) is `"BENADRYL"`.

Here is a simplified raw Concomitant Medications data set (`cm_raw`):

```{r}
cm_raw <- tibble::tibble(
  oak_id = seq_len(14L),
  raw_source = "ConMed",
  patient_number = c(375L, 375L, 376L, 377L, 377L, 377L, 377L, 378L, 378L, 378L, 378L, 379L, 379L, 379L),
  MDNUM = c(1L, 2L, 1L, 1L, 2L, 3L, 5L, 4L, 1L, 2L, 3L, 1L, 2L, 3L),
  MDRAW = c(
    "BABY ASPIRIN", "CORTISPORIN", "ASPIRIN",
    "DIPHENHYDRAMINE HCL", "PARCETEMOL", "VOMIKIND",
    "ZENFLOX OZ", "AMITRYPTYLINE", "BENADRYL",
    "DIPHENHYDRAMINE HYDROCHLORIDE", "TETRACYCLINE",
    "BENADRYL", "SOMINEX", "ZQUILL"
  )
)
cm_raw
```

To derive the `CMTRT` variable we use the `assign_no_ct()` function to map the
`MDRAW` variable to the `CMTRT` variable:

```{r}
tgt_dat <- assign_no_ct(
  tgt_var = "CMTRT",
  raw_dat = cm_raw,
  raw_var = "MDRAW"
)
tgt_dat
```

Then we create a conditioned data frame from the target data set (`tgt_dat`),
meaning we create a conditioned data frame where only rows with `CMTRT` equal to
`"BENADRYL"` are marked:

```{r}
(cnd_tgt_dat <- condition_add(tgt_dat, CMTRT == "BENADRYL"))
```

Finally, we derive the `CMGRPID` variable conditionally. Using `assign_no_ct()`,
we derive `CMGRPID` which indicates the group ID for the medication,
based on the conditioned target data set:

```{r}
derived_tgt_dat <- assign_no_ct(
  tgt_dat = cnd_tgt_dat,
  tgt_var = "CMGRPID",
  raw_dat = cm_raw,
  raw_var = "MDNUM"
)
derived_tgt_dat
```

Conditioned data frames in the `{sdtm.oak}` package provide a flexible way to
perform conditional transformations on data sets. By marking specific rows for
transformation, users can efficiently derive SDTM variables, ensuring that only
relevant records are processed.
