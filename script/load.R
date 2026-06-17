# =============================================================================
# load.R
# Purpose:  Establish database connection, define reusable query functions,
#           and load core datasets into R memory for downstream analysis.
#
# Structure:
#   1. Create Connection
#   2. Helper Functions
#      - parse_age_filter()
#   3. Query Functions
#      - get_dad_data()
#      - get_msp_data()
#   4. Close Connection
# =============================================================================


# 1. Create Connection --------------------------------------------------------
# Connects to the SPEDW data warehouse using the DSN (Data Source Name) method.
# The DSN must be configured in your ODBC driver settings beforehand.

con <- dbConnect(odbc(), "SPEDW")


# 2. Helper Functions ---------------------------------------------------------

#' Parse an age filter input into a named list of lower/upper bounds.
#'
#' Accepts two input formats:
#'   - Numeric vector of length 2: e.g. c(18, 65)  → age BETWEEN 18 AND 65
#'   - "X+" string:                e.g. "65+"       → age >= 65 (no upper bound)
#'
#' @param x  A length-2 numeric vector or a "X+" character string.
#' @return   A named list with elements `lower` (numeric) and `upper` (numeric or Inf).
#' @examples
#'   parse_age_filter(c(18, 65))  # list(lower = 18, upper = 65)
#'   parse_age_filter("65+")      # list(lower = 65, upper = Inf)

parse_age_filter <- function(x) {
  if (is.numeric(x) && length(x) == 2) {
    list(lower = x[1], upper = x[2])
    
  } else if (is.character(x) && endsWith(x, "+")) {
    list(lower = as.numeric(sub("\\+", "", x)), upper = Inf)
    
  } else {
    stop("'age_filter' must be a 2-element numeric vector (e.g. c(18, 65)) ",
         "or an open-ended string (e.g. '65+').")
  }
}


# 3. Query Functions ----------------------------------------------------------
# Each function builds a lazy dplyr query, optionally applies an age filter,
# then calls collect() to pull the result into R memory.
# Using dplyr + dbplyr means the filtering happens on the server side —
# only the rows you need are transferred across the network.
#
# Column lists are defined just before the Load Data section (section 5) and
# passed in via the `columns` argument. This separates configuration from logic:
# you edit column lists in one place without touching the function bodies.
# Passing NULL selects all columns (SELECT *).

#' Retrieve DAD (Discharge Abstract Database) records.
#'
#' Filters by separation_date range, optionally by age, and optionally by
#' ICD-10-CA diagnosis codes and/or CCI intervention codes.
#' A record is returned when it matches ANY ICD code OR ANY CCI code (OR logic
#' across both sets of columns).
#'
#' @param con         A DBI database connection object.
#' @param start_date  Start of date range (character "YYYY-MM-DD" or Date).
#' @param end_date    End of date range   (character "YYYY-MM-DD" or Date).
#' @param columns     Character vector of column names to select. NULL = all columns.
#' @param age_filter  Optional. c(min, max) or "X+" string. Default NULL (no filter).
#' @param icd_codes   Optional. ICD-10-CA prefixes matched against diag_code_1:25.
#'                    Prefix match — "J44" matches J440, J441, J449, etc.
#'                    Default NULL (no filter).
#' @param cci_codes   Optional. CCI prefixes matched against interv_code_1:20.
#'                    Supply codes with or without dots — dots are stripped automatically
#'                    (e.g. "1.WZ.19" and "1WZ19" are treated identically).
#'                    Prefix match — "1WZ19" matches 1WZ19HHXXA, 1WZ19AAJXA, etc.
#'                    Default NULL (no filter).
#' @return            A tibble of DAD records collected into R memory.
#' @examples
#'   get_dad_data(con, "2021-01-01", "2021-12-31", columns = dad_cols)
#'   get_dad_data(con, "2021-01-01", "2021-12-31", columns = dad_cols,
#'                icd_codes = c("J44", "J45"))
#'   get_dad_data(con, "2021-01-01", "2021-12-31", columns = dad_cols,
#'                cci_codes = c("1WZ19", "1WY19"))
#'   get_dad_data(con, "2021-01-01", "2021-12-31", columns = dad_cols,
#'                icd_codes = c("J44", "J45"), cci_codes = c("1WZ19"))

get_dad_data <- function(con, start_date, end_date, columns = NULL,
                         age_filter = NULL, icd_codes = NULL, cci_codes = NULL) {
  
  query <- tbl(con, in_schema("nb0077aa", "vw_dad"))
  
  if (!is.null(columns)) {
    query <- query %>% select(all_of(columns))
  }
  
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
  
  # Diagnosis (ICD-10-CA) and/or intervention (CCI) filter.
  # Both use prefix matching across their respective code columns and are
  # combined with OR — a record is included if it matches either filter.
  has_icd <- !is.null(icd_codes) && length(icd_codes) > 0
  has_cci <- !is.null(cci_codes) && length(cci_codes) > 0

  if (has_icd || has_cci) {

    make_prefix_filter <- function(col, codes) {
      lapply(codes, function(code) expr(!!col %like% !!paste0(code, "%")))
    }

    add_col_conditions <- function(all_conds, col_name, codes, null_guard) {
      sym_col <- sym(col_name)
      new_conds <- make_prefix_filter(sym_col, codes)
      if (null_guard) {
        new_conds <- lapply(new_conds, function(cond) expr(!is.na(!!sym_col) & !!cond))
      }
      c(all_conds, new_conds)
    }

    all_conditions <- list()

    if (has_icd) {
      # Strip dots — database stores ICD-10-CA codes without them (e.g. "J440" not "J44.0")
      icd_clean <- gsub("\\.", "", icd_codes)
      # diag_code_1 is always populated — no NULL guard needed
      all_conditions <- add_col_conditions(all_conditions, "diag_code_1", icd_clean, null_guard = FALSE)
      for (col in paste0("diag_code_", 2:25))
        all_conditions <- add_col_conditions(all_conditions, col, icd_clean, null_guard = TRUE)
    }

    if (has_cci) {
      # Strip dots and hyphens — database stores CCI codes without them
      # e.g. "1.LZ.19.HH-U7" → "1LZ19HHU7"
      cci_clean <- gsub("[.-]", "", cci_codes)
      for (col in paste0("interv_code_", 1:20))
        all_conditions <- add_col_conditions(all_conditions, col, cci_clean, null_guard = TRUE)
    }

    combined_filter <- Reduce(function(x, y) expr(!!x | !!y), all_conditions)
    query <- query %>% filter(!!combined_filter)
  }

  message("Collecting DAD data: ", start_date, " to ", end_date,
          if (!is.null(age_filter)) paste0(" | age: ", paste(age_filter, collapse = "-"))         else "",
          if (has_icd)              paste0(" | ICD: ", paste(gsub("\\.", "", icd_codes), collapse = ", ")) else "",
          if (has_cci)              paste0(" | CCI: ", paste(gsub("[.-]", "", cci_codes), collapse = ", ")) else "")
  
  collect(query)
}



#' Retrieve MSP (Medical Services Plan) records.
#'
#' Filters by serv_dt (service date) range, optionally by age, and optionally
#' by a list of diagnosis codes matched across diag_cd, diag_cd_2, diag_cd_3.
#' A record is returned if ANY of the three diagnosis code columns matches.
#'
#' @param con         A DBI database connection object.
#' @param start_date  Start of date range (character "YYYY-MM-DD" or Date).
#' @param end_date    End of date range   (character "YYYY-MM-DD" or Date).
#' @param columns     Character vector of column names to select. NULL = all columns.
#' @param age_filter  Optional. c(min, max) or "X+" string. Default NULL (no filter).
#' @param diag_codes  Optional. Character vector of diagnosis codes to filter on.
#'                    Matches against diag_cd, diag_cd_2, and diag_cd_3 (OR logic).
#'                    Default NULL (no filter).
#' @return            A tibble of MSP records collected into R memory.
#' @examples
#'   get_msp_data(con, "2020-12-01", "2020-12-15", columns = msp_cols)
#'   get_msp_data(con, "2020-12-01", "2020-12-15", columns = msp_cols,
#'                diag_codes = c("493", "496"))
#'   get_msp_data(con, "2020-12-01", "2020-12-15", columns = msp_cols,
#'                age_filter = c(18, 65), diag_codes = c("493", "496"))

get_msp_data <- function(con, start_date, end_date, columns = NULL,
                         age_filter = NULL, diag_codes = NULL) {
  
  query <- tbl(con, in_schema("nb0077aa", "vw_msp"))
  
  # Select specific columns if provided, otherwise keep all (SELECT *)
  if (!is.null(columns)) {
    query <- query %>% select(all_of(columns))
  }
  
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  query <- query %>%
    filter(
      serv_dt >= !!start_date,
      serv_dt <= !!end_date,
      patient_master_key > 0,
      patient_master_key < 2000000000
    )
  
  # Age filter using birth year (higher birth year = younger patient)
  if (!is.null(age_filter)) {
    age <- parse_age_filter(age_filter)
    
    if (is.finite(age$upper)) {
      query <- query %>% filter(
        clnt_birth_year <= year(serv_dt) - !!age$lower,
        clnt_birth_year >= year(serv_dt) - !!age$upper
      )
    } else {
      query <- query %>% filter(
        clnt_birth_year <= year(serv_dt) - !!age$lower
      )
    }
  }
  
  # Diagnosis code filter: match any of the three diag columns (OR logic).
  # diag_cd_2 and diag_cd_3 can be NULL, so only those columns are checked
  # with %in% when the value is not NA.
  if (!is.null(diag_codes) && length(diag_codes) > 0) {
    query <- query %>%
      filter(
        diag_cd %in% !!diag_codes |
          (!is.na(diag_cd_2) & diag_cd_2 %in% !!diag_codes) |
          (!is.na(diag_cd_3) & diag_cd_3 %in% !!diag_codes)
      )
  }
  
  message("Collecting MSP data: ", start_date, " to ", end_date,
          if (!is.null(age_filter))  paste0(" | age: ",  paste(age_filter,  collapse = "-")) else "",
          if (!is.null(diag_codes))  paste0(" | codes: ", paste(diag_codes, collapse = ", ")) else "")
  
  collect(query)
}


# 4. Load Data ----------------------------------------------------------------
# Define column lists here. Edit these vectors to add/remove fields without
# touching the function bodies above.
# Set a vector to NULL to retrieve all columns from that view (SELECT *).

dad_cols <- c(
  "patient_master_key", "dad_id", "gender", "age_year",
  "admission_date", "separation_date",
  "file_year", "hospital",
  "hosp_from", "hosp_to", "care_level",
  "alc_days", "grpr_mthd_label", "cmg_mcc_label", "cmg_cmg_label", "cmg_expct_stay_days",
  "grpr_riw", "cmgp_riw_atpcl_label", "cacs_mac_label", "cacs_cd_label",
  paste0("diag_code_", 1:25),
  paste0("interv_code_", 1:20)
)

msp_cols <- c(
  "patient_master_key", "msp_id", "paye_num",
  "clnt_gndr", "clnt_a_grp", "clnt_birth_year",
  "serv_dt",
  "paye_stat", "fitm", "paid_serv",
  "clm_tp", "clm_spec", "first_paid_date", "last_paid_date",
  "pmepd_amt", "pmeni_amt", "pmerl_amt", "expd_amt", "billed_service",
  "int_amt", "bcp_amt", "billed_service_clfn_code", "msp_claim_payment_category",
  "diag_cd", "diag_cd_2", "diag_cd_3"
)

# ICD-10-CA codes for DAD filtering — prefix match.
#
# How it works:
#   Dots are stripped automatically, so "J44.0" and "J440" are treated identically.
#   Each code is matched against diag_code_1 through diag_code_25 using a
#   prefix (LIKE) match, so a shorter code catches all its subcodes:
#     "J44"  matches J440, J441, J449  (all COPD subcodes)
#     "J45"  matches J450, J451, J459  (all Asthma subcodes)
#     "J44.0" or "J440" match only J4400, J4401, ... (narrower)
#   A record is returned if ANY of the 25 diagnosis columns matches ANY code.
#
# Set to NULL (or leave as NULL) to return all records regardless of diagnosis.
#
# Examples:
#   dad_icd_codes <- c("J44", "J45")          # COPD and Asthma
#   dad_icd_codes <- c("I21", "I50")          # Acute MI and Heart failure
#   dad_icd_codes <- NULL                      # no filter — return everything
dad_icd_codes <- NULL

# CCI (Canadian Classification of Health Interventions) codes for DAD filtering.
# Matched against interv_code_1 through interv_code_20 using prefix matching.
#
# How it works:
#   Dots and hyphens are stripped automatically, so "1.LZ.19.HH-U7" and
#   "1LZ19HHU7" are treated identically.
#   Prefix matching means shorter codes catch all subgroups:
#     "1WZ19"  matches 1WZ19HHXXA, 1WZ19AAJXA, and any other 1WZ19 subgroup
#     "1WZ"    matches all interventions starting with 1WZ (broader)
#   A record is returned if ANY of the 20 intervention columns matches ANY code.
#
# When both icd_codes and cci_codes are set, a record is included if it matches
# EITHER — ICD and CCI filters are combined with OR logic.
#
# Set to NULL (or leave as NULL) to apply no intervention filter.
#
# Examples:
#   dad_cci_codes <- c("1WZ19", "1WY19")            # specific intervention subgroups
#   dad_cci_codes <- c("1.LZ.19.HH-U7")             # dots and hyphens stripped automatically
#   dad_cci_codes <- c("1WZ")                        # all 1WZ interventions (broader)
#   dad_cci_codes <- NULL                            # no filter — return everything
dad_cci_codes <- NULL

# ICD-9 codes for MSP filtering — exact match.
#
# How it works:
#   Each code is matched exactly against diag_cd, diag_cd_2, and diag_cd_3.
#   Unlike DAD, MSP uses ICD-9 codes and does not support prefix matching —
#   you must supply the exact code as stored in the database.
#   A record is returned if ANY of the three diagnosis columns matches ANY code.
#
# Set to NULL (or leave as NULL) to return all records regardless of diagnosis.
#
# Examples:
#   msp_diag_codes <- c("493", "496")         # Asthma and COPD
#   msp_diag_codes <- c("410", "428")         # Acute MI and Heart failure
#   msp_diag_codes <- NULL                    # no filter — return everything
msp_diag_codes <- NULL

# Adjust date ranges, age_filter, and code lists as needed for your analysis.
# Remove any optional argument entirely if no restriction is required.

df_dad <- get_dad_data(
  con,
  start_date = "2020-12-01",
  end_date   = "2020-12-15",
  columns    = dad_cols,
  age_filter = c(18, 40),         # e.g. c(18, 65) or "65+" or NULL
  icd_codes  = dad_icd_codes,
  cci_codes  = dad_cci_codes
)

df_msp <- get_msp_data(
  con,
  start_date = "2020-12-01",
  end_date   = "2020-12-15",
  columns    = msp_cols,
  age_filter = c(18, 40),         # e.g. c(18, 65) or "65+"
  diag_codes = msp_diag_codes
)


# 5. Close Connection ---------------------------------------------------------
# Always disconnect when done to free server resources.

dbDisconnect(con)
rm(con)
