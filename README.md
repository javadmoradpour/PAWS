# PAWS

An R project for extracting and filtering patient health records from the SPEDW data warehouse. It queries two administrative health datasets — the Discharge Abstract Database (DAD) and the Medical Services Plan (MSP) — and applies configurable filters for date range, patient age, and diagnosis codes.

## Overview

PAWS connects to the SPEDW data warehouse over ODBC and pulls records from two views:

| Dataset | View | Description |
|---------|------|-------------|
| DAD | `nb0077aa.vw_dad` | Hospital inpatient discharge records |
| MSP | `nb0077aa.vw_msp` | Physician/outpatient billing records |

Filtering is performed server-side (via `dbplyr`) so only matching rows are transferred to R.

## Project Structure

```
PAWS/
├── script/
│   ├── main.R       # Entry point; loads libraries and sources other scripts
│   ├── setup.R      # Installs dependencies via renv
│   ├── load.R       # Database connection, query functions, and configuration
│   └── analysis.R   # Downstream analysis (currently a placeholder)
└── R_Project.Rproj
```

## Prerequisites

- R (>= 4.0 recommended)
- An ODBC DSN named **`SPEDW`** configured on your machine pointing to the data warehouse
- Access to the internal PHSA package mirror (`https://positpmlz.phsa.ca/prod-cran/latest`)

## Setup

Run `setup.R` once to install all dependencies via `renv`:

```r
source("script/setup.R")
```

This installs: `dplyr`, `readr`, `ggplot2`, `lubridate`, `purrr`, `openxlsx`, `DBI`, `odbc`, `dbplyr`, `tidyr`.

## Usage

Run the full pipeline from `main.R`:

```r
source("script/main.R")
```

This will:
1. Load all libraries
2. Connect to SPEDW
3. Execute the configured queries (see **Configuration** below)
4. Store results in `df_dad` and `df_msp`
5. Disconnect from the database
6. Run `analysis.R` on the loaded data

## Configuration

All parameters are set in `script/load.R` around lines 232–289. No function changes are needed — just edit the vectors and values in that section.

### Date Range

```r
df_dad <- get_dad_data(con, start_date = "2020-12-01", end_date = "2020-12-15", ...)
df_msp <- get_msp_data(con, start_date = "2020-12-01", end_date = "2020-12-15", ...)
```

- **DAD** filters on `separation_date` (hospital discharge date)
- **MSP** filters on `serv_dt` (service/claim date)

### Age Filter

Pass `age_filter` as a numeric range or an open-ended string:

```r
age_filter = c(18, 65)   # patients aged 18–65
age_filter = "65+"        # patients aged 65 and older
age_filter = NULL         # no age filter (default)
```

- **DAD**: filters directly on the `age_year` column
- **MSP**: derives age from `clnt_birth_year` relative to the service year

### Diagnosis Code Filter

**DAD — ICD-10 prefix matching** (checks all 25 diagnosis code columns):

```r
dad_icd_codes <- c("J44", "J45")   # COPD, Asthma
dad_icd_codes <- c()                # no filter (default)
```

A record is included if any of `diag_code_1` through `diag_code_25` starts with any of the provided prefixes (e.g., `"J44"` matches `"J44.0"`, `"J44.1"`, etc.).

**MSP — exact diagnosis code matching** (checks `diag_cd`, `diag_cd_2`, `diag_cd_3`):

```r
msp_diag_codes <- c("A001", "A002")
msp_diag_codes <- c()               # no filter (default)
```

A record is included if any of the three diagnosis columns exactly matches any of the provided codes.

### Column Selection

Edit `dad_cols` and `msp_cols` vectors to control which columns are returned.

- `dad_cols`: 40 columns including patient demographics, admission/discharge dates, CMG/RIW grouping, and all 25 diagnosis codes
- `msp_cols`: 23 columns including patient demographics, service date, billing amounts, and 3 diagnosis codes

## Output

After running `main.R`, two tibbles are available in the R environment:

| Object | Source | Description |
|--------|--------|-------------|
| `df_dad` | DAD | Inpatient discharge records matching all configured filters |
| `df_msp` | MSP | Outpatient/physician billing records matching all configured filters |

Both tibbles are passed to `analysis.R` for further processing.

## Patient Validation

Both queries automatically exclude records with invalid `patient_master_key` values (outside the range 1–1,999,999,999), which removes test or placeholder records from results.
