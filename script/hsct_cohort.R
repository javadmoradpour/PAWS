# =============================================================================
# hsct_cohort.R
# Project:  HSCT (Hematopoietic Stem Cell Transplant) Cohort
#
# Two-step extraction:
#   Step 1 — Identify HSCT patients by matching ICD-10 or CCI codes,
#             restricted to age 19–55 and years 2001–2026.
#   Step 2 — Retrieve ALL hospitalizations for those patients within the
#             study period, regardless of diagnosis (HSCT-related or not).
#
# Usage: source("script/hsct_cohort.R")
#        Results: df_hsct_index, df_hsct_all
# =============================================================================

library(dplyr)
library(lubridate)
library(DBI)
library(odbc)
library(dbplyr)
library(rlang)
library(tidyr)


# 1. Connection ---------------------------------------------------------------

con <- dbConnect(odbc(), "SPEDW")


# 2. Helper functions ---------------------------------------------------------
# Copied from load.R — keep in sync if load.R changes.

parse_age_filter <- function(x) {
  if (is.numeric(x) && length(x) == 2) {
    list(lower = x[1], upper = x[2])
  } else if (is.character(x) && endsWith(x, "+")) {
    list(lower = as.numeric(sub("\\+", "", x)), upper = Inf)
  } else {
    stop("age_filter must be c(min, max) or 'X+'")
  }
}

get_dad_data <- function(con, start_date, end_date, columns = NULL,
                         age_filter = NULL, icd_codes = NULL, cci_codes = NULL) {

  query <- tbl(con, in_schema("nb0077aa", "vw_dad"))
  if (!is.null(columns)) query <- query %>% select(all_of(columns))

  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  query <- query %>%
    filter(
      admission_date  >= !!start_date,
      separation_date <= !!end_date,
      patient_master_key > 0,
      patient_master_key < 2000000000
    )

  if (!is.null(age_filter)) {
    age <- parse_age_filter(age_filter)
    if (is.finite(age$upper))
      query <- query %>% filter(age_year >= !!age$lower, age_year <= !!age$upper)
    else
      query <- query %>% filter(age_year >= !!age$lower)
  }

  has_icd <- !is.null(icd_codes) && length(icd_codes) > 0
  has_cci <- !is.null(cci_codes) && length(cci_codes) > 0

  if (has_icd || has_cci) {

    make_prefix_filter <- function(col, codes) {
      lapply(codes, function(code) expr(!!col %like% !!paste0(code, "%")))
    }

    add_col_conditions <- function(all_conds, col_name, codes, null_guard) {
      sym_col <- sym(col_name)
      new_conds <- make_prefix_filter(sym_col, codes)
      if (null_guard)
        new_conds <- lapply(new_conds, function(cond) expr(!is.na(!!sym_col) & !!cond))
      c(all_conds, new_conds)
    }

    all_conditions <- list()

    if (has_icd) {
      icd_clean <- gsub("\\.", "", icd_codes)
      all_conditions <- add_col_conditions(all_conditions, "diag_code_1", icd_clean, null_guard = FALSE)
      for (col in paste0("diag_code_", 2:25))
        all_conditions <- add_col_conditions(all_conditions, col, icd_clean, null_guard = TRUE)
    }

    if (has_cci) {
      cci_clean <- gsub("[.-]", "", cci_codes)
      for (col in paste0("interv_code_", 1:20))
        all_conditions <- add_col_conditions(all_conditions, col, cci_clean, null_guard = TRUE)
    }

    combined_filter <- Reduce(function(x, y) expr(!!x | !!y), all_conditions)
    query <- query %>% filter(!!combined_filter)
  }

  collect(query)
}


# 3. Column configuration -----------------------------------------------------

dad_cols <- c(
  "patient_master_key", "dad_id", "gender", "age_year",
  "admission_date", "separation_date",
  "file_year", "hospital",
  "hosp_from", "hosp_to", "care_level",
  "alc_days", "grpr_mthd_label", "cmg_mcc_label", "cmg_cmg_label", "cmg_expct_stay_days",
  "grpr_riw", "cmgp_riw_atpcl_label", "cacs_mac_label", "cacs_cd_label",
  paste0("diag_code_",  1:25),
  paste0("interv_code_", 1:20)
)


# 4. Cohort definition --------------------------------------------------------

START_DATE <- "2001-01-01"
END_DATE   <- "2026-12-31"
AGE_FILTER <- c(19, 55)

# ICD-10-CA diagnosis codes (dots stripped automatically by get_dad_data):
#   Z94.80 = Bone marrow transplant status
#   Z94.83 = Stem cell transplant status
HSCT_ICD <- c(
  "Z94.80",   # Bone marrow transplant status
  "Z94.83"    # Stem cell transplant status
)

# CCI intervention codes (dots and hyphens stripped automatically):
#
#   Notation used in CCI documentation vs. what to pass here:
#     1.WZ.19.^^       → "1.WZ.19"     (^^ means "any attribute" — handled by prefix match)
#     1.LZ.19.HH-U7-*  → "1.LZ.19.HH-U7"  (* means "any qualifier" — handled by prefix match)
#     1.LZ.19.HH-U8-*  → "1.LZ.19.HH-U8"
#
#   Do NOT include ^^ or * in the strings — prefix matching already catches all
#   downstream subgroups automatically (e.g. "1WZ19" matches 1WZ19HHXXA, 1WZ19AAJXA, etc.)
#
#   1.WZ.19    = Transplantation, bone marrow (all attribute subgroups)
#   1.LZ.19.HH-U7 = Transplantation, blood/marrow, PBSC, autologous source
#   1.LZ.19.HH-U8 = Transplantation, blood/marrow, PBSC, allogeneic source
HSCT_CCI <- c(
  "1.WZ.19",
  "1.LZ.19.HH-U7",
  "1.LZ.19.HH-U8"
)


# 5. Step 1: Identify HSCT index admissions -----------------------------------
# Returns admissions matching an HSCT ICD-10 or CCI code, aged 19–55,
# within 2001–2026. A single patient may have multiple index admissions.

message("Step 1: identifying HSCT index admissions...")

df_hsct_index <- get_dad_data(
  con,
  start_date = START_DATE,
  end_date   = END_DATE,
  columns    = dad_cols,
  age_filter = AGE_FILTER,
  icd_codes  = HSCT_ICD,
  cci_codes  = HSCT_CCI
)

hsct_pmk <- unique(df_hsct_index$patient_master_key)

message(sprintf("  %d HSCT index admissions found", nrow(df_hsct_index)))
message(sprintf("  %d unique HSCT patients identified", length(hsct_pmk)))


# 6. Step 2: All hospitalizations for HSCT patients --------------------------
# No ICD or CCI filter — every admission for each identified patient is
# returned, including those unrelated to the transplant.

message("Step 2: retrieving all hospitalizations for HSCT patients...")

df_hsct_all <- tbl(con, in_schema("nb0077aa", "vw_dad")) %>%
  select(all_of(dad_cols)) %>%
  filter(
    patient_master_key %in% !!hsct_pmk,
    admission_date  >= !!as.Date(START_DATE),
    separation_date <= !!as.Date(END_DATE),
    patient_master_key > 0,
    patient_master_key < 2000000000
  ) %>%
  collect()

# Flag which records are HSCT index admissions vs. other hospitalizations
df_hsct_all <- df_hsct_all %>%
  mutate(is_hsct_index = dad_id %in% df_hsct_index$dad_id)

message(sprintf("  %d total hospitalizations for %d patients",
                nrow(df_hsct_all),
                n_distinct(df_hsct_all$patient_master_key)))
message(sprintf("  %d HSCT index admissions, %d other hospitalizations",
                sum(df_hsct_all$is_hsct_index),
                sum(!df_hsct_all$is_hsct_index)))


# 7. Close connection ---------------------------------------------------------

dbDisconnect(con)
rm(con)
