
library(ggplot2)
library(forcats)
library(scales)
library(here)

##############
##Patient Diagnosis Info
##############

#clustering output
cluster_UMAP <- "UMAP/output/patient_representations_clustered.csv"

#Cluster Assignments
UMAP_Results_Convae <- read.csv(cluster_UMAP) %>% 
  select(mrn, cluster) %>% 
  rename(Cluster_Num = cluster)

#Gather Diagnoses
Patient_Diagnoses_Pull <- read.csv("Postgres/results/Patient_Diagnoses_Pull.csv") %>% 
  dplyr::rename(mrn = subject_id) %>% 
  mutate(mrn = paste("pat_", mrn, sep =""))

#Join Diagnoses
UMAP_Diagnoses <- left_join(UMAP_Results_Convae, Patient_Diagnoses_Pull, by = "mrn")

#Get Count of Patients with X_num Diagnoses
Diagnosis_Count <- UMAP_Diagnoses %>% 
                    select(mrn, Cluster_Num, icd9_code) %>% 
  group_by(mrn, Cluster_Num) %>% 
  dplyr::summarise(
    DiagnosesPerPatient = n_distinct(icd9_code),
    .groups = 'drop'
  ) %>% 
  group_by(DiagnosesPerPatient, Cluster_Num) %>% 
    dplyr::summarise(
      Patient_Count = n_distinct(mrn),
      .groups = 'drop'
    ) %>% 
  pivot_wider(
    names_from = "Cluster_Num",
    names_prefix = "Cluster_",
    values_from = "Patient_Count",
    values_fill = 0
  )
  

###########################################################################

#Count of patient diagnoses in cluster
Patient_Diagnoses_Counts_By_Cluster <- UMAP_Diagnoses %>%
  select(mrn, Cluster_Num, icd9_code, short_title) %>% 
  mutate(
    short_title = ifelse(short_title == "NULL", icd9_code, short_title)#change to icd9 if no title
  ) %>% 
  group_by(Cluster_Num, icd9_code, short_title) %>% 
  dplyr::summarise (
    patient_count_diagnosis = n_distinct(mrn),
    .groups = 'drop'
  )

#Get total patient count within each cluster
Total_Patient_Count_By_Cluster <- UMAP_Diagnoses %>%
  select(mrn, Cluster_Num) %>% 
  group_by(Cluster_Num) %>% 
  dplyr::summarise (
    total_patients_cluster = n_distinct(mrn),
    .groups = 'drop'
  )

Total_Patient_Count_By_Cluster

#Calculate proportion of patients with diagnosis in clusters
Results_Percent_of_Patients <- left_join(Patient_Diagnoses_Counts_By_Cluster, 
                                         Total_Patient_Count_By_Cluster, 
                                         by = "Cluster_Num") %>% 
  mutate(
    Percent_of_Cluster = (patient_count_diagnosis/total_patients_cluster)
  ) %>% 
  select(Cluster_Num, icd9_code, short_title, Percent_of_Cluster) %>% 
  pivot_wider(
    names_from = "Cluster_Num",
    names_prefix = "Cluster_",
    values_from = "Percent_of_Cluster",
    values_fill = 0
  )

#All patients in data set
Total_Patient_Count_All <- UMAP_Diagnoses %>%
  select(mrn) %>% 
  n_distinct()

#Count of patient diagnoses in total
Patient_Diagnoses_Counts_All <- UMAP_Diagnoses %>%
  select(mrn, icd9_code, short_title) %>% 
  group_by(icd9_code, short_title) %>% 
  dplyr::summarise (
    patient_count_diagnosis = n_distinct(mrn),
    patient_count_diagnosis_percent_all = (n_distinct(mrn)/Total_Patient_Count_All),
    .groups = 'drop'
  )


#Calculate proportion of patients with diagnosis in total
Results_Percent_of_Patients_All <- left_join(Patient_Diagnoses_Counts_By_Cluster, 
                                         Total_Patient_Count_By_Cluster, 
                                         by = "Cluster_Num") %>% 
  mutate(
    Percent_of_Cluster = (patient_count_diagnosis/total_patients_cluster)
  ) %>% 
  select(Cluster_Num, icd9_code, short_title, Percent_of_Cluster) %>% 
  pivot_wider(
    names_from = "Cluster_Num",
    names_prefix = "Cluster_",
    values_from = "Percent_of_Cluster",
    values_fill = 0
  ) %>% 
  merge(Patient_Diagnoses_Counts_All, by = c("icd9_code", "short_title")) %>% 
  select(-c(patient_count_diagnosis)) %>% 
  dplyr::rename(
    All_Patients = patient_count_diagnosis_percent_all
  )

#Calculate Distinctive Diagnoses metric for Clusters
Results_Percent_of_Patients_All <- Results_Percent_of_Patients_All %>% 
  mutate(
    Change_Clust_0 = All_Patients - Cluster_0,
    Change_Clust_1 = All_Patients - Cluster_1,
    Change_Clust_2 = All_Patients - Cluster_2
  ) %>% 
  rowwise() %>% 
  mutate(
    Biggest_Diff_To_Overall = sqrt(max(Change_Clust_0**2, Change_Clust_1**2, Change_Clust_2**2)),
    Biggest_Diff_Between_Clusters = max(Change_Clust_0, Change_Clust_1, Change_Clust_2) -
                                      min(Change_Clust_0, Change_Clust_1, Change_Clust_2)
  ) %>% 
  mutate(
    Biggest_Diff_To_Overall = sqrt(max(Change_Clust_0**2, Change_Clust_1**2, Change_Clust_2**2))
  )

Cluster_Specific <- Results_Percent_of_Patients_All %>% 
  select(Cluster_0, Cluster_1, Cluster_2)

Results_Percent_of_Patients_All$Max_Clust <- colnames(Cluster_Specific)[apply(Cluster_Specific,1,which.max)]



###############################################
##  By Top Diagnoses by Most Distinct    ##
###############################################

#CLUSTERS BY DISEASE - Most Distinct  
data_for_heat <- Results_Percent_of_Patients_All %>% 
  arrange(desc(Biggest_Diff_Between_Clusters)) %>% 
  head(25) %>% 
  select(short_title, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  rename(
    Diagnosis = short_title
  )

#Get ranking for ordering of heatmap
data_for_heat_rank <- data_for_heat %>% 
  filter(Clusters == "All_Patients") %>%
  mutate(Diagnosis = as.factor(Diagnosis)) %>% 
  mutate(
    rank = rank(Percentage, ties.method = "first")
  ) %>% 
  select(Diagnosis, rank)

#join ranking
data_for_heat <- left_join(data_for_heat,
                           data_for_heat_rank,
                           by = "Diagnosis"
)

#Create plot for total
data_for_heat %>% 
  dplyr::mutate(
    Clusters = gsub("_", " " ,
                    gsub("Cluster_", "Cluster C",Clusters)) 
  ) %>% 
ggplot(aes(x = Clusters, y = fct_reorder(Diagnosis, rank, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 2) * 100)) +
  scale_fill_gradient(low = "white", high = "blue", limits = c(0,1), labels = label_percent()) +
  ggtitle("Diagnoses Prevalence for Unique Patients by Cluster \n Convae Most Distinct Diseases") +
  theme(plot.title = element_text(color="blue", size=12, face="bold", hjust = 0.5)) +
  ylab("Diagnosis")


#####Cluster 0##### 
data_for_heat <- Results_Percent_of_Patients_All %>% 
  filter(Max_Clust == "Cluster_0") %>% 
  arrange(desc(Biggest_Diff_Between_Clusters)) %>% 
  head(25) %>% 
  select(short_title, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  rename(
    Diagnosis = short_title
  )

data_for_heat_rank <- data_for_heat %>% 
  filter(Clusters == "Cluster_0") %>%
  mutate(Diagnosis = as.factor(Diagnosis)) %>% 
  mutate(
    rank = rank(Percentage, ties.method = "first")
  ) %>% 
  select(Diagnosis, rank)
  
#join ranking
data_for_heat <- left_join(data_for_heat,
                           data_for_heat_rank,
                           by = "Diagnosis"
                  )

#Create Graph
data_for_heat %>% 
  dplyr::mutate(
    Clusters = gsub("_", " " ,
                    gsub("Cluster_", "Cluster C",Clusters)) 
  ) %>% 
  ggplot(aes(x = Clusters, y = fct_reorder(Diagnosis, rank, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 2) * 100)) +
  scale_fill_gradient(low = "white", high = "purple4", limits = c(0,.4), labels = label_percent(accuracy = 1)) +
  ggtitle("Diagnoses Prevalence for Unique Patients by Cluster \n Most Distinct Diseases for Cluster C0") +
  theme(plot.title = element_text(color="purple4", size=14, face="bold", hjust = 0.5)) +
  ylab("Diagnosis")


#####Cluster 1##### 
data_for_heat <- Results_Percent_of_Patients_All %>% 
  filter(Max_Clust == "Cluster_1") %>% 
  arrange(desc(Biggest_Diff_Between_Clusters)) %>% 
  head(25) %>% 
  select(short_title, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  rename(
    Diagnosis = short_title
  )

#get ranking
data_for_heat_rank <- data_for_heat %>% 
  filter(Clusters == "Cluster_1") %>%
  mutate(Diagnosis = as.factor(Diagnosis)) %>% 
  mutate(
    rank = rank(Percentage, ties.method = "first")
  ) %>% 
  select(Diagnosis, rank)

#join ranking
data_for_heat <- left_join(data_for_heat,
                           data_for_heat_rank,
                           by = "Diagnosis"
)

#create graph
data_for_heat %>% 
  dplyr::mutate(
    Clusters = gsub("_", " " ,
                    gsub("Cluster_", "Cluster C",Clusters)) 
  ) %>% 
  ggplot(aes(x = Clusters, y = fct_reorder(Diagnosis, rank, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 3) * 100)) +
  scale_fill_gradient(low = "white", high = "aquamarine4", limits = c(0,.2), labels = label_percent(accuracy = 1)) +
  ggtitle("Diagnoses Prevalence for Unique Patients by Cluster \n Most Distinct Diseases for Cluster C1") +
  theme(plot.title = element_text(color="aquamarine4", size=14, face="bold", hjust = 0.5)) +
  ylab("Diagnosis")


#####Cluster 2##### 
data_for_heat <- Results_Percent_of_Patients_All %>% 
  filter(Max_Clust == "Cluster_2") %>% 
  arrange(desc(Biggest_Diff_Between_Clusters)) %>% 
  head(25) %>% 
  select(short_title, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  rename(
    Diagnosis = short_title
  )

#get ranking
data_for_heat_rank <- data_for_heat %>% 
  filter(Clusters == "Cluster_2") %>%
  mutate(Diagnosis = as.factor(Diagnosis)) %>% 
  mutate(
    rank = rank(Percentage, ties.method = "first")
  ) %>% 
  select(Diagnosis, rank)

#join ranking
data_for_heat <- left_join(data_for_heat,
                           data_for_heat_rank,
                           by = "Diagnosis"
)

#create graph
data_for_heat %>% 
  dplyr::mutate(
    Clusters = gsub("_", " " ,
                    gsub("Cluster_", "Cluster C",Clusters)) 
  ) %>% 
ggplot(aes(x = Clusters, y = fct_reorder(Diagnosis, rank, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 2) * 100)) +
  scale_fill_gradient(low = "white", high = "darkgoldenrod1", limits = c(0,1), labels = label_percent(accuracy = 1)) +
  ggtitle("Diagnoses Prevalence for Unique Patients by Cluster \n Most Distinct Diseases for Cluster C2") +
  theme(plot.title = element_text(color="darkgoldenrod1", size=14, face="bold", hjust = 0.5)) +
  ylab("Diagnosis")




#######################################
##  By Top Diagnoses by prevalence   ##
#######################################

#CLUSTERS BY DISEASE - All Patients
data_for_heat <- Results_Percent_of_Patients_All %>% 
  arrange(desc(All_Patients)) %>% 
  head(25) %>% 
  select(short_title, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  rename(
    Diagnosis = short_title
  )


data_for_heat %>% 
  dplyr::mutate(
    Clusters = gsub("_", " " ,
                    gsub("Cluster_", "Cluster C",Clusters)) 
  ) %>% 
  ggplot(aes(x = Clusters, y = fct_reorder(Diagnosis, Percentage, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 2) * 100)) +
  scale_fill_gradient(low = "white", high = "blue", limits = c(0,1), labels = label_percent()) +
  ggtitle("Diagnoses Prevalence for Unique Patients by Cluster \n Convae Most Prevalent Diseases") +
  theme(plot.title = element_text(color="blue", size=12, face="bold", hjust = 0.5)) +
  ylab("Diagnosis")


#Cluster 0 
data_for_heat <- Results_Percent_of_Patients_All %>% 
  arrange(desc(Cluster_0)) %>% 
  head(25) %>% 
  select(short_title, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  rename(
    Diagnosis = short_title
  )

data_for_heat %>% 
  dplyr::mutate(
    Clusters = gsub("_", " " ,
                    gsub("Cluster_", "Cluster C",Clusters)) 
  ) %>% 
  ggplot(aes(x = Clusters, y = fct_reorder(Diagnosis, Percentage, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 2) * 100)) +
  scale_fill_gradient(low = "white", high = "purple4", limits = c(0, 1), labels = label_percent(accuracy = 1)) +
  ggtitle("Diagnoses Prevalence for Unique Patients by Cluster \n Most Prevalent Diseases Cluster C0") +
  theme(plot.title = element_text(color="purple4", size=14, face="bold", hjust = 0.5)) +
  ylab("Diagnosis")


#Cluster 1 
data_for_heat <- Results_Percent_of_Patients_All %>% 
  arrange(desc(Cluster_1)) %>% 
  head(25) %>% 
  select(short_title, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  rename(
    Diagnosis = short_title
  )


data_for_heat %>% 
  dplyr::mutate(
    Clusters = gsub("_", " " ,
                    gsub("Cluster_", "Cluster C",Clusters)) 
  ) %>% 
  ggplot(aes(x = Clusters, y = fct_reorder(Diagnosis, Percentage, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 2) * 100)) +
  scale_fill_gradient(low = "white", high = "aquamarine4", limits = c(0,1), labels = label_percent(accuracy = 1)) +
  ggtitle("Diagnoses Prevalence for Unique Patients by Cluster \n Most Prevalent Diseases Cluster C1") +
  theme(plot.title = element_text(color="aquamarine4", size=14, face="bold", hjust = 0.5)) +
  ylab("Diagnosis")


#Cluster 2
data_for_heat <- Results_Percent_of_Patients_All %>% 
  arrange(desc(Cluster_2)) %>% 
  head(25) %>% 
  select(short_title, Cluster_0, Cluster_1, Cluster_2, All_Patients) %>% 
  pivot_longer(
    cols = c(Cluster_0, Cluster_1, Cluster_2, All_Patients),
    names_to = "Clusters",
    values_to = "Percentage"
  ) %>% 
  rename(
    Diagnosis = short_title
  )

data_for_heat %>% 
  dplyr::mutate(
    Clusters = gsub("_", " " ,
                    gsub("Cluster_", "Cluster C",Clusters)) 
  ) %>% 
  ggplot(aes(x = Clusters, y = fct_reorder(Diagnosis, Percentage, .desc = FALSE), fill = Percentage)) + 
  geom_tile() +
  geom_text(aes(label = round(Percentage, 2) * 100)) +
  scale_fill_gradient(low = "white", high = "darkgoldenrod1", limits = c(0,1), labels = label_percent(accuracy = 1)) +
  ggtitle("Diagnoses Prevalence for Unique Patients by Cluster \n Most Prevalent Diseases Cluster C2") +
  theme(plot.title = element_text(color="darkgoldenrod1", size=14, face="bold", hjust = 0.5)) +
  ylab("Diagnosis")



