# =============================================================================
# generate_fake_data.R
# Purpose:  Generate a large synthetic dataset of 1,000 patients covering
#           2 years (2019–2020) to explore and test the PAWS query functions.
#
# Patient groups (randomly assigned):
#   ~30%  DAD only   — hospital admissions, no outpatient billing
#   ~30%  MSP only   — outpatient visits, no hospital admissions
#   ~40%  Both       — appear in both datasets
#
# Patients can have multiple records (repeated admissions / visits) for
# different reasons. Secondary diagnoses are added with decreasing probability.
#
# Usage:
#   source("script/generate_fake_data.R")
#   # Objects available after sourcing:
#   #   patients   — 1,000-row demographics table
#   #   df_dad     — all generated DAD records
#   #   df_msp     — all generated MSP records
#   #   con        — DuckDB connection with both views loaded
#   # Then query with get_dad_data(con, ...) / get_msp_data(con, ...)
# =============================================================================

set.seed(42)   # fix seed for reproducibility

if (!requireNamespace("duckdb", quietly = TRUE)) install.packages("duckdb")

library(dplyr)
library(tidyr)
library(lubridate)
library(DBI)
library(dbplyr)
library(rlang)
library(duckdb)


# 1. Query functions (mirror load.R — update both if logic changes) -----------

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
                         age_filter = NULL, icd_codes = NULL) {
  query <- tbl(con, in_schema("nb0077aa", "vw_dad"))
  if (!is.null(columns)) query <- query %>% select(all_of(columns))
  start_date <- as.Date(start_date); end_date <- as.Date(end_date)
  query <- query %>% filter(
    separation_date >= !!start_date, separation_date <= !!end_date,
    patient_master_key > 0, patient_master_key < 2000000000)
  if (!is.null(age_filter)) {
    age <- parse_age_filter(age_filter)
    if (is.finite(age$upper))
      query <- query %>% filter(age_year >= !!age$lower, age_year <= !!age$upper)
    else
      query <- query %>% filter(age_year >= !!age$lower)
  }
  if (!is.null(icd_codes)) {
    mkf <- function(col, codes) lapply(codes, function(c) expr(!!col %like% !!paste0(c, "%")))
    conds <- mkf(sym("diag_code_1"), icd_codes)
    for (col in paste0("diag_code_", 2:25)) {
      s <- sym(col)
      conds <- c(conds, lapply(mkf(s, icd_codes), function(e) expr(!is.na(!!s) & !!e)))
    }
    query <- query %>% filter(!!Reduce(function(x, y) expr(!!x | !!y), conds))
  }
  collect(query)
}

get_msp_data <- function(con, start_date, end_date, columns = NULL,
                         age_filter = NULL, diag_codes = NULL) {
  query <- tbl(con, in_schema("nb0077aa", "vw_msp"))
  if (!is.null(columns)) query <- query %>% select(all_of(columns))
  start_date <- as.Date(start_date); end_date <- as.Date(end_date)
  query <- query %>% filter(
    serv_dt >= !!start_date, serv_dt <= !!end_date,
    patient_master_key > 0, patient_master_key < 2000000000)
  if (!is.null(age_filter)) {
    age <- parse_age_filter(age_filter)
    if (is.finite(age$upper))
      query <- query %>% filter(
        clnt_birth_year <= year(serv_dt) - !!age$lower,
        clnt_birth_year >= year(serv_dt) - !!age$upper)
    else
      query <- query %>% filter(clnt_birth_year <= year(serv_dt) - !!age$lower)
  }
  if (!is.null(diag_codes))
    query <- query %>% filter(
      diag_cd %in% !!diag_codes |
        (!is.na(diag_cd_2) & diag_cd_2 %in% !!diag_codes) |
        (!is.na(diag_cd_3) & diag_cd_3 %in% !!diag_codes))
  collect(query)
}


# 2. Study parameters ---------------------------------------------------------

STUDY_START <- as.Date("2019-01-01")
STUDY_END   <- as.Date("2020-12-31")
N_PATIENTS  <- 1000L

# ICD-10-CA codes used as primary/secondary diagnoses in DAD
DAD_CODES <- c(
  "J45",  # Asthma
  "J44",  # COPD
  "J18",  # Pneumonia
  "I21",  # Acute MI
  "I50",  # Heart failure
  "E11",  # Type 2 diabetes
  "I10",  # Hypertension
  "F32",  # Major depression
  "M54",  # Back / neck pain
  "K92",  # GI bleeding
  "S72",  # Hip fracture
  "C34",  # Lung cancer
  "N39",  # UTI
  "K57",  # Diverticulosis
  "G47"   # Sleep disorders
)

DAD_SUFFIXES <- c(".0", ".1", ".2", ".9", "")   # subcodes appended randomly

# ICD-9 codes used in MSP (exact match)
MSP_CODES <- c(
  "493",  # Asthma
  "496",  # COPD
  "486",  # Pneumonia
  "410",  # Acute MI
  "428",  # Heart failure
  "250",  # Diabetes mellitus
  "401",  # Hypertension
  "311",  # Depression
  "724",  # Back pain
  "578",  # GI bleeding
  "820",  # Hip fracture
  "162",  # Lung cancer
  "599",  # UTI
  "562",  # Diverticulosis
  "780"   # General symptoms
)


# 3. Helper functions ---------------------------------------------------------

rand_date <- function(n) {
  as.Date(
    sample(as.integer(STUDY_START):as.integer(STUDY_END), n, replace = TRUE),
    origin = "1970-01-01"
  )
}

rand_icd10 <- function(n) {
  paste0(
    sample(DAD_CODES,    n, replace = TRUE),
    sample(DAD_SUFFIXES, n, replace = TRUE, prob = c(0.25, 0.25, 0.15, 0.20, 0.15))
  )
}

rand_icd9 <- function(n) sample(MSP_CODES, n, replace = TRUE)

# Fill a column with random codes at a given rate, NA otherwise
sparse_icd10 <- function(n, rate) if_else(runif(n) < rate, rand_icd10(n), NA_character_)
sparse_icd9  <- function(n, rate) if_else(runif(n) < rate, rand_icd9(n),  NA_character_)


# 4. Patient demographics -----------------------------------------------------

patients <- tibble(
  patient_master_key = 1:N_PATIENTS,
  gender     = sample(c("M", "F"), N_PATIENTS, replace = TRUE, prob = c(0.48, 0.52)),
  birth_year = sample(1940:2000, N_PATIENTS, replace = TRUE),
  group      = sample(
    c("dad_only", "msp_only", "both"),
    N_PATIENTS, replace = TRUE, prob = c(0.30, 0.30, 0.40)
  )
)


# 5. DAD records --------------------------------------------------------------
# Each DAD patient gets 1–6 admissions (Poisson λ=2, min 1).
# Length of stay: Poisson λ=4, min 1 day.

dad_patients <- filter(patients, group %in% c("dad_only", "both"))

df_dad <- dad_patients %>%
  mutate(n_admissions = pmax(1L, rpois(n(), lambda = 2))) %>%
  uncount(n_admissions, .remove = TRUE) %>%
  mutate(
    dad_id              = row_number() + 10000L,
    admission_date      = rand_date(n()),
    los                 = pmax(1L, rpois(n(), lambda = 4)),
    separation_date     = pmin(admission_date + los, STUDY_END),
    age_year            = as.integer(year(separation_date) - birth_year),
    file_year           = year(separation_date),
    hospital            = sample(paste0("HOSP_", LETTERS[1:8]), n(), replace = TRUE),
    hosp_from           = NA_character_,
    hosp_to             = NA_character_,
    care_level          = sample(c("ACUTE","COMPLEX","REHAB"), n(), replace = TRUE,
                                  prob = c(0.75, 0.15, 0.10)),
    alc_days            = pmax(0L, rpois(n(), lambda = 0.5)),
    grpr_mthd_label     = "CMG",
    cmg_mcc_label       = NA_character_,
    cmg_cmg_label       = paste0("CMG", sample(100:999, n(), replace = TRUE)),
    cmg_expct_stay_days = round(pmax(1, rnorm(n(), mean = 5, sd = 2)), 1),
    grpr_riw            = round(pmax(0.1, rnorm(n(), mean = 1.0, sd = 0.4)), 3),
    cmgp_riw_atpcl_label = NA_character_,
    cacs_mac_label      = NA_character_,
    cacs_cd_label       = NA_character_,
    # Diagnosis columns: primary always filled; secondary filled with decreasing probability
    diag_code_1  = rand_icd10(n()),
    diag_code_2  = sparse_icd10(n(), 0.40),
    diag_code_3  = sparse_icd10(n(), 0.25),
    diag_code_4  = sparse_icd10(n(), 0.15),
    diag_code_5  = sparse_icd10(n(), 0.10),
    diag_code_6  = sparse_icd10(n(), 0.07),
    diag_code_7  = sparse_icd10(n(), 0.05),
    diag_code_8  = sparse_icd10(n(), 0.03),
    diag_code_9  = sparse_icd10(n(), 0.02),
    diag_code_10 = sparse_icd10(n(), 0.01)
  )

# Columns 11–25 are NULL (rare in practice)
for (i in 11:25) df_dad[[paste0("diag_code_", i)]] <- NA_character_

df_dad <- df_dad %>%
  select(patient_master_key, dad_id, gender, age_year,
         admission_date, separation_date, file_year, hospital,
         hosp_from, hosp_to, care_level,
         alc_days, grpr_mthd_label, cmg_mcc_label, cmg_cmg_label,
         cmg_expct_stay_days, grpr_riw, cmgp_riw_atpcl_label,
         cacs_mac_label, cacs_cd_label,
         paste0("diag_code_", 1:25))


# 6. MSP records --------------------------------------------------------------
# Each MSP patient gets 2–20 visits (Poisson λ=8, min 2).
# Outpatient visits are much more frequent than hospital admissions.

msp_patients <- filter(patients, group %in% c("msp_only", "both"))

df_msp <- msp_patients %>%
  mutate(n_visits = pmax(2L, rpois(n(), lambda = 8))) %>%
  uncount(n_visits, .remove = TRUE) %>%
  mutate(
    msp_id      = row_number() + 20000L,
    paye_num    = paste0("BC", sprintf("%06d", patient_master_key)),
    clnt_gndr   = gender,
    clnt_a_grp  = case_when(
      2020 - birth_year < 20 ~ "YOUTH",
      2020 - birth_year < 65 ~ "ADULT",
      TRUE                   ~ "SENIOR"
    ),
    clnt_birth_year = birth_year,
    serv_dt         = rand_date(n()),
    paye_stat       = "PAID",
    fitm            = NA_character_,
    paid_serv       = sample(c("FP","SP","GP"), n(), replace = TRUE,
                              prob = c(0.60, 0.25, 0.15)),
    clm_tp          = paid_serv,
    clm_spec        = sample(c("00","01","03","13","27","62"), n(), replace = TRUE),
    first_paid_date = serv_dt + sample(10:30, n(), replace = TRUE),
    last_paid_date  = first_paid_date,
    pmepd_amt       = round(runif(n(), 30, 150), 2),
    pmeni_amt       = 0,
    pmerl_amt       = 0,
    expd_amt        = pmepd_amt,
    billed_service  = paid_serv,
    int_amt         = 0,
    bcp_amt         = 0,
    billed_service_clfn_code   = "A",
    msp_claim_payment_category = "FEE",
    diag_cd   = rand_icd9(n()),
    diag_cd_2 = sparse_icd9(n(), 0.25),
    diag_cd_3 = sparse_icd9(n(), 0.10)
  ) %>%
  select(patient_master_key, msp_id, paye_num,
         clnt_gndr, clnt_a_grp, clnt_birth_year, serv_dt,
         paye_stat, fitm, paid_serv, clm_tp, clm_spec,
         first_paid_date, last_paid_date,
         pmepd_amt, pmeni_amt, pmerl_amt, expd_amt, billed_service,
         int_amt, bcp_amt, billed_service_clfn_code, msp_claim_payment_category,
         diag_cd, diag_cd_2, diag_cd_3)


# 7. Load into DuckDB ---------------------------------------------------------

con <- dbConnect(duckdb::duckdb(), ":memory:")
dbExecute(con, "CREATE SCHEMA nb0077aa")
dbWriteTable(con, Id(schema = "nb0077aa", table = "vw_dad"), df_dad)
dbWriteTable(con, Id(schema = "nb0077aa", table = "vw_msp"), df_msp)


# 8. Summary ------------------------------------------------------------------

dad_pmk <- unique(df_dad$patient_master_key)
msp_pmk <- unique(df_msp$patient_master_key)

message("\n========================================")
message("  Synthetic dataset summary")
message("========================================")
message(sprintf("Study period : %s  to  %s", STUDY_START, STUDY_END))
message(sprintf("Total patients: %d", N_PATIENTS))
message(sprintf("  DAD only : %d", sum(patients$group == "dad_only")))
message(sprintf("  MSP only : %d", sum(patients$group == "msp_only")))
message(sprintf("  Both     : %d", sum(patients$group == "both")))
message("")
message(sprintf("DAD records  : %d  (across %d patients)", nrow(df_dad), length(dad_pmk)))
message(sprintf("  Date range : %s  to  %s",
                min(df_dad$separation_date), max(df_dad$separation_date)))
message(sprintf("  Avg admissions per patient : %.1f",
                nrow(df_dad) / length(dad_pmk)))
message(sprintf("  Age range  : %d – %d  (mean %.1f)",
                min(df_dad$age_year), max(df_dad$age_year), mean(df_dad$age_year)))
message("")
message(sprintf("MSP records  : %d  (across %d patients)", nrow(df_msp), length(msp_pmk)))
message(sprintf("  Date range : %s  to  %s",
                min(df_msp$serv_dt), max(df_msp$serv_dt)))
message(sprintf("  Avg visits per patient : %.1f",
                nrow(df_msp) / length(msp_pmk)))
message(sprintf("  Age range  : %d – %d  (mean %.1f)",
                min(2020 - df_msp$clnt_birth_year),
                max(2020 - df_msp$clnt_birth_year),
                mean(2020 - df_msp$clnt_birth_year)))
message("")
message(sprintf("Patients in both datasets : %d",
                length(intersect(dad_pmk, msp_pmk))))
message("========================================")
message("DuckDB ready. Use get_dad_data(con, ...) / get_msp_data(con, ...)")
message("Example:")
message("  get_dad_data(con, '2019-01-01', '2020-12-31', icd_codes = 'J45')")
message("  get_msp_data(con, '2019-01-01', '2020-12-31', diag_codes = '493')")
