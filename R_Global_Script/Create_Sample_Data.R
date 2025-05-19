## Code to Create the Sample Data for the 

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

#path for data pull to get unique value pools
pmt_path <- here("Postgres/results", 
                 "Patient_Matrix_Pull_Prescriptions_Patient_Type.csv")

#Read in your real data to grab unique pools
Patient_Matrix_Table <- read.csv(pmt_path, stringsAsFactors = FALSE)

#create vector of column names from real data
vars <- c(
  "subject_id",
  "formulary_drug_cd",
  "startdate",
  "hadm_id",
  "ethnicity",
  "admission_type",
  "diagnosis",
  "hospital_expire_flag"
)

#cols to check
cols_to_check  <- setdiff(vars, "subject_id")

#lists of unique values
unique_values  <- lapply(Patient_Matrix_Table[, cols_to_check], unique)

#get unique values of different column values in real data
ethnicities <- unique_values$ethnicity
admission_types <- unique_values$admission_type
drug_codes <- unique_values$formulary_drug_cd
diagnoses_pool <- unique_values$diagnosis
startdate_pool <- as.Date(unique_values$startdate)

# set seed
set.seed(42)  

# Generate 200 patients with ethnicity & a 5% expire flag
sim_patients <- tibble(
  subject_id = paste0("pat_", seq_len(200))
) %>%
  mutate(
    ethnicity            = sample(ethnicities, 200, replace = TRUE),
    hospital_expire_flag = sample(c(0, 1), 200, replace = TRUE, prob = c(0.95, 0.05)),
    n_admissions         = sample(1:5,   200, replace = TRUE)
  )

#Expand into admissions, each with its own type + 1–3 diagnoses
# use the real set of unique vlaues to sample column fields
sim_admissions <- sim_patients %>%
  transmute(
    subject_id,
    ethnicity,
    hospital_expire_flag,
    hadm_id = map2(subject_id, n_admissions, ~ paste0(.x, "_adm_", seq_len(.y))),
    admission_type = map(n_admissions, ~ sample(admission_types, .x, replace = TRUE)),
    diagnosis_list  = map(n_admissions, ~ replicate(
      .x,
      sample(diagnoses_pool, sample(1:3, 1), replace = FALSE),
      simplify = FALSE
    ))
  ) %>%
  unnest(c(hadm_id, admission_type, diagnosis_list)) %>%
  rename(diagnosis_options = diagnosis_list)

#For each patient, generate 5–75 prescriptions all within a 1-week window
# 
sim_prescriptions <- sim_patients %>%
  transmute(
    subject_id,
    ethnicity,
    hospital_expire_flag,
    base_date      = sample(startdate_pool, 200, replace = TRUE),
    n_prescriptions = sample(5:75, 200, replace = TRUE)
  ) %>%
  uncount(n_prescriptions) %>%
  group_by(subject_id) %>%
  mutate(
    # assign each prescription to one of that patient's admissions
    hadm_id          = sample(
      sim_admissions$hadm_id[sim_admissions$subject_id == subject_id],
      n(), replace = TRUE
    ),
    # pick a drug code
    formulary_drug_cd = sample(drug_codes, n(), replace = TRUE),
    # spread events over one week
    startdate         = base_date + days(sample(0:6, n(), replace = TRUE))
  ) %>%
  ungroup()

# Join in admission_type & pick one diagnosis per event
simulated_df <- sim_prescriptions %>%
  left_join(
    sim_admissions %>% 
      select(subject_id, hadm_id, admission_type, diagnosis_options),
    by = c("subject_id","hadm_id")
  ) %>%
  rowwise() %>%
  mutate(
    diagnosis = sample(diagnosis_options, 1)
  ) %>%
  ungroup() %>%
  select(
    subject_id,
    formulary_drug_cd,
    startdate,
    hadm_id,
    ethnicity,
    admission_type,
    diagnosis,
    hospital_expire_flag
  )

# Inspect the first few rows
print(simulated_df, n = 20)

#write csv of sample data
write.csv(simulated_df,
          paste0("Postgres/results/", 
                 "sample_data.csv"))
