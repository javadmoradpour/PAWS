# =============================================================================
# test.R
# Purpose:  Creates synthetic patient data and tests get_dad_data() and
#           get_msp_data() without a real SPEDW connection.
#           Uses an in-memory DuckDB database in place of SPEDW.
#
# Usage:    source("script/test.R")   — or run line by line in RStudio
# Requires: duckdb package (installed automatically below if missing)
# =============================================================================


# 0. Setup --------------------------------------------------------------------

if (!requireNamespace("duckdb", quietly = TRUE)) {
  message("Installing duckdb...")
  install.packages("duckdb")
}

library(dplyr)
library(DBI)
library(dbplyr)
library(lubridate)
library(rlang)
library(duckdb)


# 1. Copy query functions from load.R -----------------------------------------
# These mirror load.R exactly. If you change load.R, update here too.

parse_age_filter <- function(x) {
  if (is.numeric(x) && length(x) == 2) {
    list(lower = x[1], upper = x[2])
  } else if (is.character(x) && endsWith(x, "+")) {
    list(lower = as.numeric(sub("\\+", "", x)), upper = Inf)
  } else {
    stop("'age_filter' must be a 2-element numeric vector or an open-ended string.")
  }
}

get_dad_data <- function(con, start_date, end_date, columns = NULL,
                         age_filter = NULL, icd_codes = NULL) {

  query <- tbl(con, in_schema("nb0077aa", "vw_dad"))
  if (!is.null(columns)) query <- query %>% select(all_of(columns))

  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  query <- query %>%
    filter(
      separation_date >= !!start_date,
      separation_date <= !!end_date,
      patient_master_key > 0,
      patient_master_key < 2000000000
    )

  if (!is.null(age_filter)) {
    age <- parse_age_filter(age_filter)
    if (is.finite(age$upper)) {
      query <- query %>% filter(age_year >= !!age$lower, age_year <= !!age$upper)
    } else {
      query <- query %>% filter(age_year >= !!age$lower)
    }
  }

  if (!is.null(icd_codes)) {
    make_prefix_filter <- function(col, codes) {
      lapply(codes, function(code) expr(!!col %like% !!paste0(code, "%")))
    }
    all_conditions <- make_prefix_filter(sym("diag_code_1"), icd_codes)
    for (col in paste0("diag_code_", 2:25)) {
      sym_col <- sym(col)
      all_conditions <- c(all_conditions,
        lapply(make_prefix_filter(sym_col, icd_codes),
               function(cond) expr(!is.na(!!sym_col) & !!cond)))
    }
    icd_filter <- Reduce(function(x, y) expr(!!x | !!y), all_conditions)
    query <- query %>% filter(!!icd_filter)
  }

  collect(query)
}

get_msp_data <- function(con, start_date, end_date, columns = NULL,
                         age_filter = NULL, diag_codes = NULL) {

  query <- tbl(con, in_schema("nb0077aa", "vw_msp"))
  if (!is.null(columns)) query <- query %>% select(all_of(columns))

  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  query <- query %>%
    filter(
      serv_dt >= !!start_date,
      serv_dt <= !!end_date,
      patient_master_key > 0,
      patient_master_key < 2000000000
    )

  if (!is.null(age_filter)) {
    age <- parse_age_filter(age_filter)
    if (is.finite(age$upper)) {
      query <- query %>% filter(
        clnt_birth_year <= year(serv_dt) - !!age$lower,
        clnt_birth_year >= year(serv_dt) - !!age$upper
      )
    } else {
      query <- query %>% filter(clnt_birth_year <= year(serv_dt) - !!age$lower)
    }
  }

  if (!is.null(diag_codes)) {
    query <- query %>%
      filter(
        diag_cd %in% !!diag_codes |
          (!is.na(diag_cd_2) & diag_cd_2 %in% !!diag_codes) |
          (!is.na(diag_cd_3) & diag_cd_3 %in% !!diag_codes)
      )
  }

  collect(query)
}


# 2. Fake data ----------------------------------------------------------------
#
# DAD — 8 rows. Date range tested: 2020-12-01 to 2020-12-15. Age filter: c(18, 40).
# ICD filter: "J45" (prefix match).
#
# pmk | age | sep_date   | diag_code_1 | diag_code_3 | Expected
# ----+-----+------------+-------------+-------------+----------
#   1 |  25 | 2020-12-05 | J45.0       | —           | INCLUDE  (primary match)
#   2 |  25 | 2020-12-07 | J11.0       | J45.1       | INCLUDE  (secondary match)
#   3 |  25 | 2020-12-09 | J44.0       | —           | EXCLUDE  (wrong ICD)
#   4 |  25 | 2020-11-30 | J45.0       | —           | EXCLUDE  (before date range)
#   5 |  45 | 2020-12-10 | J45.0       | —           | EXCLUDE  (age too high)
#   6 |  18 | 2020-12-09 | J45.1       | —           | INCLUDE  (min age boundary)
#   7 |  40 | 2020-12-10 | J45.2       | —           | INCLUDE  (max age boundary)
#   8 |  25 | 2020-12-16 | J45.0       | —           | EXCLUDE  (after date range)

diag_na <- rep(NA_character_, 8)

fake_dad <- tibble(
  patient_master_key  = 1:8,
  dad_id              = 101:108,
  gender              = rep(c("M", "F"), 4),
  age_year            = c(25, 25, 25, 25, 45, 18, 40, 25),
  admission_date      = as.Date(c("2020-12-01","2020-12-03","2020-12-05","2020-11-25",
                                   "2020-12-05","2020-12-05","2020-12-05","2020-12-12")),
  separation_date     = as.Date(c("2020-12-05","2020-12-07","2020-12-09","2020-11-30",
                                   "2020-12-10","2020-12-09","2020-12-10","2020-12-16")),
  file_year           = 2020L,
  hospital            = "HOSP_A",
  hosp_from           = NA_character_,
  hosp_to             = NA_character_,
  care_level          = "ACUTE",
  alc_days            = 0L,
  grpr_mthd_label     = "CMG",
  cmg_mcc_label       = NA_character_,
  cmg_cmg_label       = "CMG001",
  cmg_expct_stay_days = 5.0,
  grpr_riw            = 1.0,
  cmgp_riw_atpcl_label = NA_character_,
  cacs_mac_label      = NA_character_,
  cacs_cd_label       = NA_character_,
  diag_code_1         = c("J45.0","J11.0","J44.0","J45.0","J45.0","J45.1","J45.2","J45.0"),
  diag_code_2         = diag_na,
  diag_code_3         = c(NA, "J45.1", NA, NA, NA, NA, NA, NA),
  diag_code_4         = diag_na,  diag_code_5  = diag_na,  diag_code_6  = diag_na,
  diag_code_7         = diag_na,  diag_code_8  = diag_na,  diag_code_9  = diag_na,
  diag_code_10        = diag_na,  diag_code_11 = diag_na,  diag_code_12 = diag_na,
  diag_code_13        = diag_na,  diag_code_14 = diag_na,  diag_code_15 = diag_na,
  diag_code_16        = diag_na,  diag_code_17 = diag_na,  diag_code_18 = diag_na,
  diag_code_19        = diag_na,  diag_code_20 = diag_na,  diag_code_21 = diag_na,
  diag_code_22        = diag_na,  diag_code_23 = diag_na,  diag_code_24 = diag_na,
  diag_code_25        = diag_na
)

# MSP — 7 rows. Date range: 2020-12-01 to 2020-12-15. Age filter: c(18, 40).
# Diagnosis filter: "493" (exact match). ICD-9.
#
# Age at service in MSP is derived from birth year:
#   clnt_birth_year <= year(serv_dt) - 18  (not younger than 18)
#   clnt_birth_year >= year(serv_dt) - 40  (not older than 40)
#   → birth_year between 1980 and 2002 for year 2020
#
# pmk | birth_year | serv_dt    | diag_cd | diag_cd_2 | Expected
# ----+------------+------------+---------+-----------+----------
#   1 |       1995 | 2020-12-05 | 493     | —         | INCLUDE  (primary match, age≈25)
#   2 |       1995 | 2020-12-05 | 411     | 493       | INCLUDE  (secondary match, age≈25)
#   3 |       1995 | 2020-12-05 | 496     | —         | EXCLUDE  (wrong code)
#   4 |       1995 | 2020-11-30 | 493     | —         | EXCLUDE  (before date range)
#   5 |       1975 | 2020-12-05 | 493     | —         | EXCLUDE  (age≈45, too high)
#   6 |       2002 | 2020-12-05 | 493     | —         | INCLUDE  (min age boundary, age≈18)
#   7 |       1980 | 2020-12-05 | 493     | —         | INCLUDE  (max age boundary, age≈40)

fake_msp <- tibble(
  patient_master_key         = 1:7,
  msp_id                     = 201:207,
  paye_num                   = "BC001",
  clnt_gndr                  = rep(c("M","F"), length.out = 7),
  clnt_a_grp                 = "ADULT",
  clnt_birth_year            = c(1995, 1995, 1995, 1995, 1975, 2002, 1980),
  serv_dt                    = as.Date(c("2020-12-05","2020-12-05","2020-12-05",
                                          "2020-11-30","2020-12-05","2020-12-05","2020-12-05")),
  paye_stat                  = "PAID",
  fitm                       = NA_character_,
  paid_serv                  = "FP",
  clm_tp                     = "FP",
  clm_spec                   = "00",
  first_paid_date            = as.Date("2020-12-20"),
  last_paid_date             = as.Date("2020-12-20"),
  pmepd_amt                  = 35.0,
  pmeni_amt                  = 0.0,
  pmerl_amt                  = 0.0,
  expd_amt                   = 35.0,
  billed_service             = "FP_VISIT",
  int_amt                    = 0.0,
  bcp_amt                    = 0.0,
  billed_service_clfn_code   = "A",
  msp_claim_payment_category = "FEE",
  diag_cd                    = c("493","411","496","493","493","493","493"),
  diag_cd_2                  = c(NA,   "493", NA,   NA,   NA,   NA,   NA),
  diag_cd_3                  = NA_character_
)


# 3. In-memory database -------------------------------------------------------

con_test <- dbConnect(duckdb::duckdb(), ":memory:")

dbExecute(con_test, "CREATE SCHEMA nb0077aa")
dbWriteTable(con_test, Id(schema = "nb0077aa", table = "vw_dad"), fake_dad)
dbWriteTable(con_test, Id(schema = "nb0077aa", table = "vw_msp"), fake_msp)


# 4. Tests --------------------------------------------------------------------

pass <- 0L
fail <- 0L

check <- function(label, result_pmk, expected_pmk) {
  got      <- sort(as.integer(result_pmk))
  expected <- sort(as.integer(expected_pmk))
  if (identical(got, expected)) {
    message("PASS  ", label)
    pass <<- pass + 1L
  } else {
    message("FAIL  ", label)
    message("      expected pmk: ", paste(expected, collapse = ", "))
    message("      got pmk:      ", paste(got,      collapse = ", "))
    fail <<- fail + 1L
  }
}

message("\n--- DAD tests ---")

# A: all three filters active
check("date + age(18-40) + ICD J45",
  get_dad_data(con_test, "2020-12-01", "2020-12-15",
               age_filter = c(18, 40), icd_codes = "J45")$patient_master_key,
  c(1L, 2L, 6L, 7L))

# B: date only — rows excluded by age/ICD still within date range
check("date only",
  get_dad_data(con_test, "2020-12-01", "2020-12-15")$patient_master_key,
  c(1L, 2L, 3L, 5L, 6L, 7L))

# C: date + ICD, no age filter — age-excluded row (pmk 5) now included
check("date + ICD J45 (no age filter)",
  get_dad_data(con_test, "2020-12-01", "2020-12-15",
               icd_codes = "J45")$patient_master_key,
  c(1L, 2L, 5L, 6L, 7L))

# D: open-ended age — none of the test rows are 65+
check("age 65+ (expect 0 rows)",
  get_dad_data(con_test, "2020-12-01", "2020-12-15",
               age_filter = "65+")$patient_master_key,
  integer(0))

# E: multiple ICD prefixes — J45 and J44 both match
check("ICD c(J44, J45) returns both",
  get_dad_data(con_test, "2020-12-01", "2020-12-15",
               icd_codes = c("J44", "J45"))$patient_master_key,
  c(1L, 2L, 3L, 5L, 6L, 7L))

message("\n--- MSP tests ---")

# F: all three filters active
check("date + age(18-40) + diag 493",
  get_msp_data(con_test, "2020-12-01", "2020-12-15",
               age_filter = c(18, 40), diag_codes = "493")$patient_master_key,
  c(1L, 2L, 6L, 7L))

# G: date only
check("date only",
  get_msp_data(con_test, "2020-12-01", "2020-12-15")$patient_master_key,
  c(1L, 2L, 3L, 5L, 6L, 7L))

# H: date + diag, no age filter
check("date + diag 493 (no age filter)",
  get_msp_data(con_test, "2020-12-01", "2020-12-15",
               diag_codes = "493")$patient_master_key,
  c(1L, 2L, 5L, 6L, 7L))

# I: multiple diag codes
check("diag c(493, 496) returns both",
  get_msp_data(con_test, "2020-12-01", "2020-12-15",
               diag_codes = c("493", "496"))$patient_master_key,
  c(1L, 2L, 3L, 5L, 6L, 7L))


# 5. Summary ------------------------------------------------------------------

dbDisconnect(con_test, shutdown = TRUE)

message(sprintf("\n%d passed, %d failed", pass, fail))
