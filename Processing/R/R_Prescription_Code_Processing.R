library(tidyr)
library(dplyr)
library(janitor)

###################################################
## Set working directory to the repository base  ##
###################################################

#set working directory two directory levels up from script
#setwd(dirname(getwd())) 
#getwd()

library(here)

data_file_name <- "sample_data.csv"
#data_file_name <- "Patient_Matrix_Pull_Prescriptions_Patient_Type.csv"

#get file path
pmt_path <- here("Postgres/results", 
                 data_file_name)
                

#Read in Mimic Prescription Table
Patient_Matrix_Table <- read.csv(pmt_path)
names(Patient_Matrix_Table)

#Filter out blank formulary drug codes and newborn patients
Patient_Matrix_Table <- Patient_Matrix_Table %>%
  mutate(
    #map individual codes to smaller abbreviations
    formulary_drug_cd = ifelse(formulary_drug_cd == '1/4NS1000', 'NS',
                               ifelse(formulary_drug_cd == '5000MLBAG', 'MLBAG',
                               ifelse(formulary_drug_cd == '0.45NS3000I', 'NS',
                               ifelse(formulary_drug_cd == '5FU500I', 'FU',
                               ifelse(formulary_drug_cd == '1/2NS250I', 'NS',
                                      formulary_drug_cd
                               )))))
  ) %>% 
  filter(!(formulary_drug_cd %in% c('', ' '))) %>% #No blanks
  filter(admission_type != c('NEWBORN')) %>% #No newborns
  mutate( 
    #create new code as unique identifier for prescription type
    new_drug_cd = gsub('/', 'Q', #change slashes to Q
                       gsub('\\.', '', #drop periods
                            gsub('[0-9]+.*$', '', formulary_drug_cd) #removes values after first num
                       )
    )
  ) %>% 
  mutate( 
    letter_check = grepl("^[A-Za-z]+$", new_drug_cd, perl = T) #Flag For Letters only
  ) %>% 
  arrange(subject_id, startdate)

#Get all unique prescriptions on the same day, order, and randomise date ties
Patient_Matrix_Table_Grouped <- Patient_Matrix_Table %>% 
  select(subject_id, startdate, new_drug_cd) %>% 
  unique %>% 
  group_by(subject_id, startdate) %>%
  arrange(subject_id, startdate) %>% 
  slice_sample(prop = 1) %>% 
  ungroup()

#Get list of drugs between 5 and 50000 total prescriptions-patient-days
Drug_Codes_Sufficient_Counts <- Patient_Matrix_Table_Grouped %>% 
  select(new_drug_cd) %>%
  group_by(new_drug_cd) %>% 
  dplyr::summarise(count = n(), .groups = "drop") %>% 
  ungroup() %>% 
  filter(count >= 5) %>% 
  filter(count <= 50000)

#Filter for sufficiently prevalent drugs
Patient_Matrix_Drug_Chosen <- Patient_Matrix_Table_Grouped %>% 
  filter(new_drug_cd %in% Drug_Codes_Sufficient_Counts$new_drug_cd)

#Get list of patients between 8 and 64 prescriptions unique to the day of drugs chosen
Patient_Prescription_Sufficient_Counts <- Patient_Matrix_Drug_Chosen %>%
  select(subject_id) %>%
  group_by(subject_id) %>% 
  dplyr::summarise(count = n()) %>% 
  ungroup() %>% 
  filter(count >= 8) %>% 
  filter(count <= 64) #Max Not Needed

#View prescription Count distribution for patients
quantile(Patient_Prescription_Sufficient_Counts$count)
mean(Patient_Prescription_Sufficient_Counts$count)


Patient_Matrix_Drug_Chosen <- Patient_Matrix_Drug_Chosen %>% 
  filter(subject_id %in% Patient_Prescription_Sufficient_Counts$subject_id)


#write.csv(Patient_Matrix_Drug_Chosen, "R_Pre-Processing/Patient_Matrix_Drug_Chosen.csv")


#Add unique ID number for all drugs (nec for convae)
Formulary_IDs <- Patient_Matrix_Drug_Chosen %>% 
  select(new_drug_cd) %>% 
  unique() %>% 
  mutate( 
    Drug_ID = rank(new_drug_cd)
  ) %>% 
  arrange(.,Drug_ID)


#Join Patient drug name to new ID for convae architecture 
Patient_Matrix_Table_Form_IDs <- 
  left_join(Patient_Matrix_Drug_Chosen, Formulary_IDs, by="new_drug_cd")

#write.csv(Patient_Matrix_Table_Form_IDs, "R_Pre-Processing/Patient_Matrix_Table_Form_IDs.csv")

#Split on patient for list of dataframes by patients
Patient_Matrix_Table_Split <- split(Patient_Matrix_Table_Form_IDs, f = Patient_Matrix_Drug_Chosen$subject_id)


#Create list of patient (new ids with prefix)
# and an prescription sequence to be loaded into convae
Patient_Vectors <- 
  lapply(Patient_Matrix_Table_Split, 
         function(x) 
           list(mrn = 
                  unique(
                    paste('pat_', x$subject_id, sep = '')
                  ),
                EHRseq = 
                  unique(
                    paste(x$Drug_ID, collapse=", "),
                  )
           )
  )

#Bind to one dataframe
cohort_ehr_seq <- bind_rows(Patient_Vectors)
View(cohort_ehr_seq)

#Get training data indicies from cohort-ehrseq.csv (5 to 1 split)
train_obs = sample(nrow(cohort_ehr_seq), size =nrow(cohort_ehr_seq)*(5.0/6.0))

#Get test data indicies on cohort_test-ehrseq.csv
sample.holdout = cohort_ehr_seq[-train_obs, ]

# Also divide our design matrix into training and testing sets
x_train = cohort_ehr_seq[train_obs, ]

#Gather vocab cohort for cohort-vocab.csv
Vocab_Cohort <- Patient_Matrix_Table_Form_IDs %>% 
  select(Drug_ID, new_drug_cd) %>% 
  unique() %>% 
  arrange(Drug_ID) %>% 
  mutate(
    LABEL = paste("cl_term_", Drug_ID, sep = ""),
    CODE  = Drug_ID
  ) %>% 
  select(LABEL, CODE, new_drug_cd)


#write to data example
# this flag will control whether the new training and test files
# are written to the data example directory, where convae
# code will reference
write_to_data_example <- FALSE

#logic to set appropriate path to write files
if(write_to_data_example) {
  #set directory
  directory_path_to_write <- 'data_example/'

} else {
  
  #set directory
  directory_path_to_write <- 'Processing/outputs/'
  
  #if directory doesnt exist, create the directory
  if(!dir.exists(directory_path_to_write)) {
    dir.create(directory_path_to_write)
  }
  
  #set directory for R results
  directory_path_to_write <- paste0(directory_path_to_write,
                                    'R/')
  
  #if directory does not exist for R results, create it
  if(!dir.exists(directory_path_to_write)) {
    dir.create(directory_path_to_write)
  }
  
}



#Write csv files to set path
write.csv(cohort_ehr_seq, paste0(directory_path_to_write, "All_Patients.csv"), 
          row.names = FALSE, 
          quote = FALSE)
write.csv(x_train, paste0(directory_path_to_write,"cohort-ehrseq.csv"), 
                          row.names = FALSE, 
                          quote = FALSE)
write.csv(sample.holdout, paste0(directory_path_to_write, "cohort_test-ehrseq.csv"), 
                            row.names = FALSE, 
                            quote = FALSE)
write.csv(Vocab_Cohort, paste0(directory_path_to_write, "cohort-vocab.csv"), 
                            row.names = FALSE, 
                            quote = FALSE)

