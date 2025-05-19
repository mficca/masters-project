
/*
subject_id 
formulary_drug_cd
startdate (of prescription)
hadm_id 
ethnicity (of particular hadm_id)
admission_type (of particular hadm_id)
admissions.diagnosis (of particular hadm_id)
hospital_expire_flag (of particular hadm_id)
*/

select prescriptions.subject_id, 
			prescriptions.formulary_drug_cd,
			prescriptions.drug,
			prescriptions.startdate,
			prescriptions.hadm_id,
			admissions.ethnicity,
			admissions.admission_type,
			admissions.diagnosis,
			admissions.hospital_expire_flag
from prescriptions
	left join admissions
	ON prescriptions.subject_id = admissions.subject_id
		AND prescriptions.hadm_id = admissions.hadm_id


