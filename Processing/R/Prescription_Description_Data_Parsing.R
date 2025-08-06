library(stringr)

#This gets a single drug name for all of the drug codes for the analysis at the end 
# (tile maps)

#Read in all of the data for the prescription formularies
All_Descriptions_For_Drug_Codes <- read.csv("Postgres/results/Prescription_Formularies.csv") %>% 
  filter(formulary_drug_cd != "NULL") %>% 
  unique

#get the distinct prescription formularies and counts
Distinct_Prescription_Formularies <- All_Descriptions_For_Drug_Codes %>%
  select(formulary_drug_cd, drug) %>% 
  mutate(
    formulary_drug_cd = ifelse(formulary_drug_cd == '1/4NS1000', 'NS',
                               ifelse(formulary_drug_cd == '5000MLBAG', 'MLBAG',
                                      ifelse(formulary_drug_cd == '0.45NS3000I', 'NS',
                                             ifelse(formulary_drug_cd == '5FU500I', 'FU',
                                                    ifelse(formulary_drug_cd == '1/2NS250I', 'NS',
                                                           formulary_drug_cd
                                                    )))))
  ) %>% 
    filter(!(formulary_drug_cd %in% c('', ' '))) %>%
  mutate( 
    new_drug_cd = gsub('/', 'Q',
                       gsub('\\.', '',
                            gsub('[0-9]+.*$', '', formulary_drug_cd) #removes values after first num
                       )
    )
  ) %>% 
  #filter(!(new_drug_cd %in% c('', ' '))) %>% #removes 1/4NS1000, 5000MLBAG, 0.45NS3000I, 5FU500I, 1/2NS250I
  mutate( 
    letter_check = grepl("^[A-Za-z]+$", new_drug_cd, perl = T)
  ) %>% 
  select(drug, new_drug_cd) %>% 
  group_by(drug, new_drug_cd) %>% 
  summarise (
    Count_Drug = n(),
    .groups = 'drop'
  )

#count the number of drug formulary descriptions
Descriptions_Drug_Codes_Parsing_Needed_for_Duplicates <- Distinct_Prescription_Formularies %>% 
  group_by(new_drug_cd) %>% 
  filter(Count_Drug == max(Count_Drug)) %>% 
  ungroup %>% 
  rename(
    NameForDrugCode = drug
  ) %>% 
  select(new_drug_cd, NameForDrugCode)


#Keep only 1 formulary description
Mult_Counted_Drug_Descriptions <- Distinct_Prescription_Formularies %>% 
  group_by(new_drug_cd) %>% 
  filter(Count_Drug == max(Count_Drug)) %>% 
  ungroup %>% 
  rename(
    NameForDrugCode = drug
  ) %>% 
  select(new_drug_cd, NameForDrugCode) %>% 
  group_by(new_drug_cd) %>% 
  mutate(counter = n()) %>% 
  filter(counter > 1) %>% 
  arrange(new_drug_cd) %>% 
  ungroup %>% 
  mutate(
    NameForDrugCode = word(NameForDrugCode,1) 
  ) %>% 
  unique %>% 
  group_by(new_drug_cd) %>% 
  mutate (
    ranking = rank(NameForDrugCode)
  ) %>% 
  filter(ranking == min(ranking)) %>% 
  ungroup %>% 
  select(new_drug_cd, NameForDrugCode)


#combine the final lists of unique formulary id drug descriptions
Descriptions_For_Drug_Codes <- rbind(
                                Descriptions_Drug_Codes_Parsing_Needed_for_Duplicates %>% 
                                  filter(!(new_drug_cd %in% Mult_Counted_Drug_Descriptions$new_drug_cd))
                                , Mult_Counted_Drug_Descriptions
                                ) 
    
#Write a csv for all of the distinct descriptions for drug codes                           
write.csv(
  Descriptions_For_Drug_Codes,
  file = "Processing/outputs/R/Descriptions_For_Drug_Codes.csv",
  row.names = FALSE
)
