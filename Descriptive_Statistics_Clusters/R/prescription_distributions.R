

library(ggplot2)
library(forcats)
library(scales)
library(here)


Descriptions_For_Drug_Codes <- read.csv("Processing/outputs/R/Descriptions_For_Drug_Codes.csv")

#clustering output
cluster_UMAP <- "UMAP/output/patient_representations_clustered.csv"

#Cluster Assignments
UMAP_Results_Convae <- read.csv(cluster_UMAP) %>% 
  select(mrn, cluster) %>% 
  rename(Cluster_Num = cluster)

##############
##Patient Prescription Info
##############

Prescriptions_Pull <- read.csv("Postgres/results/Patient_Matrix_Pull_Prescriptions_Patient_Type.csv") %>% 
  dplyr::rename(mrn = subject_id) %>% 
  mutate(mrn = paste("pat_", mrn, sep ="")) %>% 
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
  select(mrn, new_drug_cd) %>% 
  unique


#Prescription cluster assignments
My_Prescription_Clusters <- UMAP_Results_Convae %>% 
  left_join(.,
            Prescriptions_Pull, by = "mrn" 
  ) %>% left_join (
    .,
    Descriptions_For_Drug_Codes,
    by = c("new_drug_cd")
  ) 

#Count number of descriptions to match if the match to multiple new_drug_cd
Prescription_Descriptions_Counts <- My_Prescription_Clusters %>%
  group_by(NameForDrugCode) %>% 
  dplyr::summarise(
    Distinct_new_drug_cds = n_distinct(new_drug_cd)
  ) %>% 
  ungroup

#renaming DrugCodeNames with Description code if unclear
My_Prescription_Clusters <- left_join(My_Prescription_Clusters,
                                      Prescription_Descriptions_Counts, 
                                      by = "NameForDrugCode") %>% 
  mutate(
    New_Description_Drug_Code = ifelse(Distinct_new_drug_cds > 1,
                                       paste(NameForDrugCode, "(", new_drug_cd, ")", sep = ""),
                                       NameForDrugCode
    )
  )

#Patient Prescription Counts by Cluster
Patient_Prescription_Counts_By_Cluster <- My_Prescription_Clusters %>%
  select(mrn, Cluster_Num, new_drug_cd, New_Description_Drug_Code) %>% 
  unique() %>% 
  group_by(Cluster_Num, new_drug_cd, New_Description_Drug_Code) %>% 
  dplyr::summarise (
    patient_count_prescription = n_distinct(mrn),
    .groups = 'drop'
  )

Total_Patient_Count_By_Cluster <- My_Prescription_Clusters %>%
  select(mrn, Cluster_Num) %>% 
  group_by(Cluster_Num) %>% 
  dplyr::summarise (
    total_patients_cluster = n_distinct(mrn),
    .groups = 'drop'
  )

Results_Percent_of_Patients <- left_join(Patient_Prescription_Counts_By_Cluster, 
                                         Total_Patient_Count_By_Cluster, 
                                         by = "Cluster_Num") %>% 
  mutate(
    Percent_of_Cluster = (patient_count_prescription/total_patients_cluster)
  ) %>% 
  select(Cluster_Num, new_drug_cd, New_Description_Drug_Code, Percent_of_Cluster) %>% 
  pivot_wider(
    names_from = "Cluster_Num",
    names_prefix = "Cluster_",
    values_from = "Percent_of_Cluster",
    values_fill = 0
  )


Total_Patient_Count_All <- My_Prescription_Clusters %>%
  select(mrn) %>% 
  n_distinct()

Patient_Diagnoses_Counts_All <- My_Prescription_Clusters %>%
  select(mrn, new_drug_cd, New_Description_Drug_Code) %>% 
  group_by(new_drug_cd, New_Description_Drug_Code) %>% 
  dplyr::summarise (
    patient_count_prescription = n_distinct(mrn),
    patient_count_prescription_percent_all = (n_distinct(mrn)/Total_Patient_Count_All),
    .groups = 'drop'
  )


Results_Percent_of_Patients_All <- left_join(Patient_Prescription_Counts_By_Cluster, 
                                             Total_Patient_Count_By_Cluster, 
                                             by = "Cluster_Num") %>% 
  dplyr::mutate(
    Percent_of_Cluster = (patient_count_prescription/total_patients_cluster)
  ) %>% 
  select(Cluster_Num, new_drug_cd, New_Description_Drug_Code, Percent_of_Cluster) %>% 
  pivot_wider(
    names_from = "Cluster_Num",
    names_prefix = "Cluster_",
    values_from = "Percent_of_Cluster",
    values_fill = 0
  ) %>% 
  merge(Patient_Diagnoses_Counts_All, by = c("new_drug_cd", "New_Description_Drug_Code")) %>% 
  select(-c(patient_count_prescription)) %>% 
  dplyr::rename(
    All_Patients = patient_count_prescription_percent_all
  )


Results_Percent_of_Patients_All <- Results_Percent_of_Patients_All %>% 
  dplyr::mutate(
    Change_Clust_0 = All_Patients - Cluster_0,
    Change_Clust_1 = All_Patients - Cluster_1,
    Change_Clust_2 = All_Patients - Cluster_2
  ) %>% 
  rowwise() %>% 
  dplyr::mutate(
    Biggest_Diff_To_Overall = sqrt(max(Change_Clust_0**2, Change_Clust_1**2, Change_Clust_2**2)),
    Biggest_Diff_Between_Clusters = max(Change_Clust_0, Change_Clust_1, Change_Clust_2) -
      min(Change_Clust_0, Change_Clust_1, Change_Clust_2)
  ) %>% 
  dplyr::mutate(
    Biggest_Diff_To_Overall = sqrt(max(Change_Clust_0**2, Change_Clust_1**2, Change_Clust_2**2))
  )

Cluster_Specific <- Results_Percent_of_Patients_All %>% 
  select(Cluster_0, Cluster_1, Cluster_2)

Results_Percent_of_Patients_All$Max_Clust <- colnames(Cluster_Specific)[apply(Cluster_Specific,1,which.max)]



#CLUSTERS BY Prescriptions - BIGGEST DIFFERENCE
data_for_heat <- Results_Percent_of_Patients_All %>% 
  arrange(desc(Biggest_Diff_Between_Clusters)) %>% 
  head(25) %>% 
  select(New_Description_Drug_Code, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  dplyr::rename(
    Prescription = New_Description_Drug_Code
  )

data_for_heat %>% 
  dplyr::mutate(
    Clusters = gsub("_", " " ,
                    gsub("Cluster_", "Cluster C",Clusters)) 
  ) %>% 
  ggplot(aes(x = Clusters, y = fct_reorder(Prescription, Percentage, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 2) * 100)) +
  scale_fill_gradient(low = "white", high = "blue", limits = c(0,1), labels = label_percent()) +
  ggtitle("Prescription Prevalence for Unique Patients by Cluster \n Convae Most Distinct Prescriptions") +
  theme(plot.title = element_text(color="blue", size=12, face="bold", hjust = 0.5)) +
  ylab("Prescription")


#Cluster 0 
data_for_heat <- Results_Percent_of_Patients_All %>% 
  filter(Max_Clust == "Cluster_0") %>% 
  arrange(desc(Biggest_Diff_Between_Clusters)) %>% 
  head(25) %>% 
  select(New_Description_Drug_Code, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  dplyr::rename(
    Prescription = New_Description_Drug_Code
  ) 

data_for_heat_rank <- data_for_heat %>% 
  filter(Clusters == "Cluster_0") %>%
  mutate(Prescription = as.factor(Prescription)) %>% 
  mutate(
    rank = rank(Percentage, ties.method = "first")
  ) %>% 
  select(Prescription, rank)

data_for_heat <- left_join(data_for_heat,
                           data_for_heat_rank,
                           by = "Prescription"
)


data_for_heat %>% 
  dplyr::mutate(
    Clusters = gsub("_", " " ,
                    gsub("Cluster_", "Cluster C",Clusters)), 
    Prescription = gsub("0.9% ", "" ,
                        gsub("\\(Immediate Release\\) ", "",
                             gsub("\\(Dilaudid\\)", "", Prescription)))
  ) %>% 
  ggplot(aes(x = Clusters, y = fct_reorder(Prescription, rank, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 2) * 100)) +
  scale_fill_gradient(low = "white", high = "purple4", limits = c(0,1), labels = label_percent()) +
  ggtitle("Prescription Prevalence for Unique Patients by Cluster \n Convae Most Distinct Prescriptions for Cluster C0") +
  theme(plot.title = element_text(color="purple4", size=14, face="bold", hjust = 0.5)) +
  ylab("Prescription")

#Cluster 1 
data_for_heat <- Results_Percent_of_Patients_All %>% 
  filter(Max_Clust == "Cluster_1") %>% 
  arrange(desc(Biggest_Diff_Between_Clusters)) %>% 
  head(25) %>% 
  select(New_Description_Drug_Code, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  dplyr::rename(
    Prescription = New_Description_Drug_Code
  )

data_for_heat_rank <- data_for_heat %>% 
  filter(Clusters == "Cluster_1") %>%
  mutate(Prescription = as.factor(Prescription)) %>% 
  mutate(
    rank = rank(Percentage, ties.method = "first")
  ) %>% 
  select(Prescription, rank)

data_for_heat <- left_join(data_for_heat,
                           data_for_heat_rank,
                           by = "Prescription"
)

data_for_heat %>% 
  dplyr::mutate(
    Clusters = gsub("_", " " ,
                    gsub("Cluster_", "Cluster C",Clusters))
  ) %>% 
  ggplot(aes(x = Clusters, y = fct_reorder(Prescription, rank, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 3) * 100)) +
  scale_fill_gradient(low = "white", high = "aquamarine4", limits = c(0,.20), labels = label_percent(accuracy = 1)) +
  ggtitle("Prescription Prevalence for Unique Patients by Cluster \n Convae Most Distinct Prescriptions for Cluster C1") +
  theme(plot.title = element_text(color="aquamarine4", size=14, face="bold", hjust = 0.5)) +
  ylab("Prescription")




#Cluster 2
data_for_heat <- Results_Percent_of_Patients_All %>% 
  filter(Max_Clust == "Cluster_2") %>% 
  arrange(desc(Biggest_Diff_Between_Clusters)) %>% 
  head(25) %>% 
  select(New_Description_Drug_Code, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  dplyr::rename(
    Prescription = New_Description_Drug_Code
  )

data_for_heat_rank <- data_for_heat %>% 
  filter(Clusters == "Cluster_2") %>%
  mutate(Prescription = as.factor(Prescription)) %>% 
  mutate(
    rank = rank(Percentage, ties.method = "first")
  ) %>% 
  select(Prescription, rank)

data_for_heat <- left_join(data_for_heat,
                           data_for_heat_rank,
                           by = "Prescription"
)



data_for_heat %>% 
  dplyr::mutate(
    Clusters = gsub("_", " " ,
                    gsub("Cluster_", "Cluster C",Clusters)), 
    Prescription = gsub("0.12% Oral Rinse", "", Prescription)
  ) %>%
  ggplot(aes(x = Clusters, y = fct_reorder(Prescription, rank, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 2) * 100)) +
  scale_fill_gradient(low = "white", high = "darkgoldenrod1", limits = c(0,1), labels = label_percent(accuracy = 1)) +
  ggtitle("Prescription Prevalence for Unique Patients by Cluster \n Convae Most Distinct Prescriptions for Cluster C2") +
  theme(plot.title = element_text(color="darkgoldenrod1", size=14, face="bold", hjust = 0.5)) +
  ylab("Prescription")





###########################################
##  By Top Prescriptions by prevalence   ##
###########################################


#CLUSTERS BY Prescriptions - BIGGEST DIFFERENCE
data_for_heat <- Results_Percent_of_Patients_All %>% 
  arrange(desc(All_Patients)) %>%  
  head(25) %>% 
  select(New_Description_Drug_Code, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  dplyr::rename(
    Prescription = New_Description_Drug_Code
  )


ggplot(data_for_heat, aes(x = Clusters, y = fct_reorder(Prescription, Percentage, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 2) * 100)) +
  scale_fill_gradient(low = "white", high = "blue", limits = c(0,1)) +
  ggtitle("Prescription Prevalence in Patient By Cluster \n Top Prescriptions by Volume") +
  theme(plot.title = element_text(color="blue", size=12, face="bold", hjust = 0.5)) +
  ylab("Prescription") 



#Cluster 0 
data_for_heat <- Results_Percent_of_Patients_All %>% 
  arrange(desc(Cluster_0)) %>%
  head(25) %>% 
  select(New_Description_Drug_Code, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  dplyr::rename(
    Prescription = New_Description_Drug_Code
  ) 


ggplot(data_for_heat, aes(x = Clusters, y = fct_reorder(Prescription, Percentage, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 2) * 100)) +
  scale_fill_gradient(low = "white", high = "purple4", limits = c(0,1)) +
  ggtitle("Prescription Prevalence in Patient By Cluster \n Top Prescriptions by Volume Cluster 0") +
  theme(plot.title = element_text(color="purple4", size=14, face="bold", hjust = 0.5)) +
  ylab("Prescription")


#Cluster 1 
data_for_heat <- Results_Percent_of_Patients_All %>% 
  arrange(desc(Cluster_1)) %>%
  head(25) %>% 
  select(New_Description_Drug_Code, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  dplyr::rename(
    Prescription = New_Description_Drug_Code
  )


ggplot(data_for_heat, aes(x = Clusters, y = fct_reorder(Prescription, Percentage, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 2) * 100)) +
  scale_fill_gradient(low = "white", high = "aquamarine4", limits = c(0,1)) +
  ggtitle("Prescription Prevalence in Patient By Cluster \n Top Prescriptions by Volume Cluster 1") +
  theme(plot.title = element_text(color="aquamarine4", size=14, face="bold", hjust = 0.5)) +
  ylab("Prescription")


#Cluster 2
data_for_heat <- Results_Percent_of_Patients_All %>% 
  arrange(desc(Cluster_2)) %>%
  head(25) %>% 
  select(New_Description_Drug_Code, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  dplyr::rename(
    Prescription = New_Description_Drug_Code
  )


ggplot(data_for_heat, aes(x = Clusters, y = fct_reorder(Prescription, Percentage, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 2) * 100)) +
  scale_fill_gradient(low = "white", high = "darkgoldenrod1", limits = c(0,1)) +
  ggtitle("Prescription Prevalence in Patient By Cluster \n Top Prescriptions by Volume Cluster 2") +
  theme(plot.title = element_text(color="darkgoldenrod1", size=14, face="bold", hjust = 0.5)) +
  ylab("Prescription")


#Cluster 1 vs Cluster 0 - focus Cluster 1

data_for_heat <- Results_Percent_of_Patients_All %>% 
  filter(Cluster_1 >= Cluster_0) %>% 
  arrange(desc(Cluster_1 - Cluster_0)) %>%
  head(25) %>% 
  select(New_Description_Drug_Code, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  dplyr::rename(
    Prescription = New_Description_Drug_Code
  )

Results_Percent_of_Patients_All %>% 
  filter(Cluster_1 >= Cluster_0) %>% 
  arrange(desc(Cluster_1 - Cluster_0)) %>%
  head(25) %>% 
  select(New_Description_Drug_Code, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  filter(Cluster_0 == 0)



ggplot(data_for_heat, aes(x = Clusters, y = fct_reorder(Prescription, Percentage, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 3) * 100)) +
  scale_fill_gradient(low = "white", high = "aquamarine4", limits = c(0,.3)) +
  ggtitle("Prescription Prevalence in Patient By Cluster \n Top Prescription Cluster 0 vs Cluster 1 \n Focus Cluster 1") +
  theme(plot.title = element_text(color="aquamarine4", size=14, face="bold", hjust = 0.5)) +
  ylab("Prescription")







#Cluster 1 vs Cluster 0 - focus Cluster 1

data_for_heat <- Results_Percent_of_Patients_All %>% 
  filter(Cluster_1 <= Cluster_0) %>% 
  arrange(desc(Cluster_0 - Cluster_1)) %>%
  head(25) %>% 
  select(New_Description_Drug_Code, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  dplyr::rename(
    Prescription = New_Description_Drug_Code
  )

Results_Percent_of_Patients_All %>% 
  filter(Cluster_1 <= Cluster_0) %>% 
  arrange(desc(Cluster_0 - Cluster_1)) %>%
  #head(25) %>% 
  select(New_Description_Drug_Code, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  filter(Cluster_1 == 0) %>% 
  view()



ggplot(data_for_heat, aes(x = Clusters, y = fct_reorder(Prescription, Percentage, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 2) * 100)) +
  scale_fill_gradient(low = "white", high = "purple4", limits = c(0,1)) +
  ggtitle("Prescription Prevalence in Patient By Cluster \n Top Prescription Cluster 0 vs Cluster 1 \n Focus Cluster 0") +
  theme(plot.title = element_text(color="purple4", size=14, face="bold", hjust = 0.5)) +
  ylab("Prescription")
