/*
The goal of this set of queries is to get
patient demographic information, specifically for
particular patients across all of their data. 

In other words, the religion, ethnicity, and insurance
of patients are listed, 
	-"N/A", no entries
	-"value", one single entry for say insurance (i.e. always private health care)
	-"multi", multiple entries for hospital stays (i.e. private and medicare)


*/

WITH first_admission_time AS
(
-- Get all patient information across the patients and admissions tables
 SELECT
      p.subject_id, p.dob, p.gender, p.expire_flag
      , MIN (a.admittime) AS first_admittime
      , MIN( ROUND( (cast(admittime as date) - cast(dob as date)) / 365.242,2) )
          AS first_admit_age
	  , MAX( ROUND( (cast(admittime as date) - cast(dob as date)) / 365.242,2) )
          AS age_from_last_admit,
-- we count the religion, ethnicity, and insurance to see how many distinct
--	entries a patient has in the database
		count(distinct religion) as Religion_Count,
		count(distinct ethnicity) as Ethnicity_Count,
		count(distinct insurance) as Insurance_Count,
		count(distinct diagnosis) as Admission_Diagnosis_Count
	
  FROM patients p 
  INNER JOIN admissions a
  ON p.subject_id = a.subject_id
  GROUP BY p.subject_id, p.dob, p.gender, p.expire_flag
	--HAVING count(distinct religion) <= 1
  ORDER BY Religion_Count desc, p.subject_id
)
, Religion_Complete AS 
(
-- Gather religion specific data information
select fir.subject_id, fir.dob, fir.gender, fir.expire_flag,
      fir.first_admittime, fir.first_admit_age, fir.age_from_last_admit,
	  case when fir.religion_count = 1 then a.religion -- keep religion if only 1 for patient
			when fir.religion_count > 1 then 'Multi' -- relabel 'multi' if more than 1
			when fir.religion_count = 0 then 'N/A' -- relabel 'N/A' if zero total
			END as Religion,
		Ethnicity_Count,
		Insurance_Count,
		Admission_Diagnosis_Count
from first_admission_time fir
left join (select distinct subject_id, religion
		  	from admissions
		    where religion is not null
		  ) a
ON fir.subject_id = a.subject_id and religion_count = 1 -- only join for patients with 1 religion
)
, Ethinicity_Complete as (
select Rel.subject_id, Rel.dob, Rel.gender, Rel.expire_flag,
      Rel.first_admittime, Rel.first_admit_age, Rel.age_from_last_admit,
	  Rel.religion,
	  case when Rel.Ethnicity_Count = 1 then a.ethnicity -- keep ethnicity if only 1 for patient
			when Rel.Ethnicity_Count > 1 then 'Multi' -- relabel 'multi' if more than 1
			when Rel.Ethnicity_Count = 0 then 'N/A' -- relabel 'N/A' if zero total
			END as Ethnicity,
			Insurance_Count,
			Admission_Diagnosis_Count
from Religion_Complete Rel
left join (select distinct subject_id, ethnicity
		  	from admissions
		    where ethnicity is not null
		  ) a
ON Rel.subject_id = a.subject_id and Ethnicity_Count = 1 -- only join for patients with 1 ethnicity
)
, Insurance_Complete as (
select Eth.subject_id, Eth.dob, Eth.gender, Eth.expire_flag, 
      Eth.first_admittime, Eth.first_admit_age, Eth.age_from_last_admit,
	  Eth.religion,
	  Eth.ethnicity,
	  case when Eth.Insurance_Count = 1 then a.insurance -- keep insurance if only 1 for patient
			when Eth.Insurance_Count > 1 then 'Multi' -- relabel 'multi' if more than 1
			when Eth.Insurance_Count = 0 then 'N/A'  -- relabel 'N/A' if zero total
			END as Insurance,
			Admission_Diagnosis_Count
from Ethinicity_Complete Eth
left join (select distinct subject_id, insurance
		  	from admissions
		  ) a
ON Eth.subject_id = a.subject_id and Insurance_Count = 1 -- only join for patients with 1 ethnicity
)
select Ins.subject_id, Ins.dob, Ins.gender, Ins.expire_flag, 
	  case when hospital_death.Hospital_Expiration is NULL then 0
	  		else hospital_death.Hospital_Expiration
			END as hospital_expire_flag,
      Ins.first_admittime, Ins.first_admit_age, Ins.age_from_last_admit,
	  Ins.religion,
	  Ins.ethnicity,
	  Ins.Insurance,
	  case when Ins.Admission_Diagnosis_Count = 1 then a.admission_diagnosis -- keep insurance if only 1 for patient
			when Ins.Admission_Diagnosis_Count > 1 then 'Multi' -- relabel 'multi' if more than 1
			when Ins.Admission_Diagnosis_Count = 0 then 'N/A'  -- relabel 'N/A' if zero total
			END as Admissions,
	  case when newborn.NewBornFlag is NULL then 'NormalPatient'
	  		else newborn.NewBornFlag 
			END as Newborn
from Insurance_Complete Ins
left join (select distinct subject_id, diagnosis as admission_diagnosis
		  	from admissions
		    where diagnosis is not null
		  ) a ON Ins.subject_id = a.subject_id and Admission_Diagnosis_Count = 1 -- only join for patients with 1 admission_diagnosis		  	  
left join (select distinct subject_id, 'Newborn' as NewBornFlag
		  	from admissions
		    where diagnosis = 'NEWBORN'
) newborn ON Ins.subject_id = newborn.subject_id
left join (select distinct subject_id, 1 as Hospital_Expiration
		  	from admissions
		    where hospital_expire_flag = 1
) hospital_death ON Ins.subject_id = hospital_death.subject_id














