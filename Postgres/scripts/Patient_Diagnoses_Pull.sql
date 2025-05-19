/*
Checked with the MIMIC files available online,
	144 icd9 codes are not present in d_icd
	and therefore will not have descriptions

They are however real codes
*/

With All_Patient_Diagnoses As (
select distinct diag_icd.subject_id, 
			diag_icd.icd9_code,
			d_icd.short_title,
			d_icd.long_title
from diagnoses_icd diag_icd
left join d_icd_diagnoses d_icd
	ON diag_icd.icd9_code = d_icd.icd9_code
where diag_icd.icd9_code is not null
)
select APD.*,
			min_seq_num_for_Diagnosis
from All_Patient_Diagnoses APD
	left join (
		--Get lowest seq num for diagnosis (most important)
		--   only to be used when filtering unecessary diag codes
		select subject_id, 
				 icd9_code,
				 min(seq_num) as min_seq_num_for_Diagnosis
			from diagnoses_icd 
			group by subject_id, 
				 icd9_code
	) Min_Seq_Pull
	ON APD.subject_id = Min_Seq_Pull.subject_id
		and APD.icd9_code = Min_Seq_Pull.icd9_code
		
	
	
	
	
	
	
	
	







