#This code creates the final descriptive tables of the 
# clusters developed by UMAP

library(tidyr)
library(dplyr)
library(janitor)

#Get path of clustering method to extract
cluster_UMAP_path_Convae <- "UMAP/output/patient_representations_clustered.csv"
cluster_UMAP_path_Convae <- paste(cluster_UMAP_Folder, "umap_kmeans_k", k, ".csv", sep = "")

#Read in and keep Patients and Clusters
UMAP_Results <- read.csv(cluster_UMAP_path_Convae) %>% 
               select(mrn, cluster)

#Join all Diagnoses
Patient_Diagnoses_Pull <- read.csv("Postgres/results/Patient_Diagnoses_Pull.csv") %>% 
                            dplyr::rename(mrn = subject_id) %>% 
                            mutate(mrn = paste("pat_", mrn, sep =""))

#Get all UMAP Result Diagnoses
UMAP_Diagnoses <- left_join(UMAP_Results, Patient_Diagnoses_Pull, by = "mrn")

#Set Diagnosis Path
UMAP_Diagnoses_Path <- paste(cluster_UMAP_Folder, "Convae_k_of_3_Diagnoses.csv", sep = "")

#Get all Patient Demographic Info
Patient_Master_Info <- read.csv("Postgres/results/Patient_Master_Table.csv") %>% 
                          dplyr::rename(mrn = subject_id) %>% 
                          mutate(mrn = paste("pat_", mrn, sep ="")) 

#Join all Patient Demographic Info
UMAP_Patient_Info <- left_join(UMAP_Results, Patient_Master_Info, by = "mrn")


#Age Strat Calculation by Cluster
Avg_Ages <- UMAP_Patient_Info %>% 
  select(cluster, age_from_last_admit) %>%
  mutate(
    age_bucket = as.numeric(
            substring (
              as.character(
                ifelse(age_from_last_admit < 90,
                    floor(age_from_last_admit),
                    90)
             ), 1, 1)
           ) * 10
  ) %>% 
  mutate(
    age_bucket = paste("Age_",
                       ifelse(age_from_last_admit < 90,
                              paste(age_bucket, age_bucket + 10, sep = "-"),
                              "90+"),
                       sep = ""
    )
  ) %>% 
  select(cluster, age_bucket) %>%
  group_by(cluster, age_bucket) %>%
  dplyr::summarise (
    Age_Count = n(),
    .groups = 'drop'
  ) %>% 
  pivot_wider(
    names_from = age_bucket,
    values_from = Age_Count,
    values_fill = 0
  )

#Age Strat Calculation - All Patients
Avg_Ages_All_Patients <- UMAP_Patient_Info %>% 
  select(age_from_last_admit) %>% 
  mutate(
    age_bucket = as.numeric(
      substring (
        as.character(
          ifelse(age_from_last_admit < 90,
                 floor(age_from_last_admit),
                 90)
        ), 1, 1)
    ) * 10
  ) %>% 
  mutate(
    age_bucket = paste("Age_",
                          ifelse(age_from_last_admit < 90,
                          paste(age_bucket, age_bucket + 10, sep = "-"),
                          "90+"),
                        sep = ""
                       )
  ) %>% 
  select(age_bucket) %>% 
  group_by(age_bucket) %>% 
  dplyr::summarise (
    Age_Count = n(),
    .groups = 'drop'
  ) %>% 
  t() %>% 
  row_to_names(row_number = 1) %>%
  as.data.frame() #%>% 
  #remove_rownames() 



#Counts for Categorical Variables by Cluster 
Categorical_Vars <- 
  UMAP_Patient_Info %>% 
  select(cluster, age_from_last_admit, gender, hospital_expire_flag, expire_flag) %>% 
  group_by(cluster) %>%
  dplyr::summarise(
    Total_Patients = n(),
    Male = sum(gender == 'M'),
    Female = sum(gender == 'F'),
    #OtherGender = sum(gender != 'M' & gender != 'F'),
    MaleOver90 = sum(age_from_last_admit >= 90 & gender == 'M'),
    FemaleOver90 = sum(age_from_last_admit >= 90 & gender == 'F'),
    HospitalDeaths = sum(hospital_expire_flag),
    OverallDeaths = sum(expire_flag)
  )

#Counts for Categorical Variables All Patients
Categorical_Vars_All_Patients <- 
  UMAP_Patient_Info %>% 
  select(age_from_last_admit, gender, hospital_expire_flag, expire_flag) %>% 
  dplyr::summarise(
    Total_Patients = n(),
    Male = sum(gender == 'M'),
    Female = sum(gender == 'F'),
    #OtherGender = sum(gender != 'M' & gender != 'F'),
    MaleOver90 = sum(age_from_last_admit >= 90 & gender == 'M'),
    FemaleOver90 = sum(age_from_last_admit >= 90 & gender == 'F'),
    HospitalDeaths = sum(hospital_expire_flag),
    OverallDeaths = sum(expire_flag)
    
  )

#Join Tables
Categorical_Var_With_Age <- 
  merge(Avg_Ages, Categorical_Vars, by = "cluster")

#Calculate Percentages For Clusters
Percentages <- Categorical_Var_With_Age %>% 
  pivot_longer(
    cols = c("Age_0-10", "Age_10-20", "Age_20-30", "Age_30-40", "Age_40-50", "Age_50-60", 
             "Age_60-70", "Age_70-80", "Age_80-90", "Age_90+",
              Male, Female, MaleOver90, FemaleOver90, HospitalDeaths, OverallDeaths),
    names_to = 'Metrics',
    values_to = "Values"
  ) %>% 
  mutate (
    Percentage = round(Values/Total_Patients,3)
  ) %>% 
  select(-c(Total_Patients, Values)) %>% 
  pivot_wider(
    names_from = Metrics,
    names_prefix = "Percent",
    values_from = Percentage
  )

#Join Tables
Categorical_Var_With_Age_And_Percentage <- merge(Categorical_Var_With_Age,
                                                 Percentages, 
                                                 by = "cluster")

#Make sure character columns are number where they should be
character_columns <- which( sapply( Avg_Ages_All_Patients, class ) == 'character' )
Avg_Ages_All_Patients[character_columns] <- lapply( Avg_Ages_All_Patients[character_columns], function(x) as.numeric(as.character(x)) )

#Columnn Bind Categorical Variables
Categorical_Vars_All_Patients_With_Age <- cbind(Categorical_Vars_All_Patients, Avg_Ages_All_Patients)

#Calculate Percentages For All Patients
Percentages_All_Patients_With_Age <- Categorical_Vars_All_Patients_With_Age %>% 
  pivot_longer(
    cols = c("Age_0-10", "Age_10-20", "Age_20-30", "Age_30-40", "Age_40-50", "Age_50-60", 
             "Age_60-70", "Age_70-80", "Age_80-90", "Age_90+",
             Male, Female, MaleOver90, FemaleOver90, HospitalDeaths, OverallDeaths),
    names_to = 'Metrics',
    values_to = "Values"
  ) %>% 
  mutate (
    Percentage = round(Values/Total_Patients,3)
  ) %>% 
  select(-c(Total_Patients, Values)) %>% 
  pivot_wider(
    names_from = Metrics,
    names_prefix = "Percent",
    values_from = Percentage
  )

  

#Column Bind All Patient Data
Categorical_Var_Patient_Summary_All_Patients <- 
  cbind(
    #cbind(Avg_Ages_All_Patients,
        Categorical_Vars_All_Patients_With_Age,
    #      ), 
        Percentages_All_Patients_With_Age) %>% 
  mutate(
    cluster = "Total"
  )

#Row Bind Cluster Data with All Patient Data
Categorical_Var_Patient_Summary_Everything <- rbind(
  Categorical_Var_With_Age_And_Percentage,
  Categorical_Var_Patient_Summary_All_Patients  
)


#Adjust table and data types for final table
Categorical_Var_Patient_Summary_Everything %>% 
  mutate(
    cluster = paste("Cluster_", cluster, sep = "")
  ) %>% 
  t() %>% 
  row_to_names(row_number = 1) %>%
  as.data.frame() %>% 
  dplyr::rename(
    Total = Cluster_Total
  ) %>% 
  mutate(
    Cluster_0 = as.numeric(Cluster_0),
    Cluster_1 = as.numeric(Cluster_1),
    Cluster_2 = as.numeric(Cluster_2),
    Total = as.numeric(Total)
  )
  


Categorical_Var_Patient_Summary_Path <- "R_Pre-Processing/Results/Convae_Patient_Demographics_k_of_3.csv"

#write.csv(Categorical_Var_Patient_Summary_Everything, file = Categorical_Var_Patient_Summary_Path)



#Deaths Fisher Test
HospitalDeaths <- Categorical_Var_Patient_Summary_Everything %>% 
  filter(cluster != c("2","Total")) %>%
  mutate(
    Survivals = Total_Patients - HospitalDeaths
  ) %>% 
  select(HospitalDeaths, Survivals) 

Fisher_Test_Deaths_Clust_0_1 <- fisher.test(HospitalDeaths)
Fisher_Test_Deaths_Clust_0_1





  
