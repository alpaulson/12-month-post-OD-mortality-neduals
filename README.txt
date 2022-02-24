#### STUDYCOHORT.RMD ####

Description of files used: 
MedPAR (2011-2017): Identify opioid overdoses in inpatient hospital emergency room (https://resdac.org/cms-data/files/medpar)
Outpatient (2011-2017): Identify opioid overdoses in outpatient emergency room (https://resdac.org/cms-data/files/op-ffs)
Outpatient Revenue Center file (2014-2017): Supplement to outpatient file that identifies the revenue center (i.e., where the charge originated). Used to identify emergency room charges.
MBSF Base (2013-2017): Contains demographic information (age, sex, death date if applicable, coverage history, etc.). (https://resdac.org/cms-data/files/mbsf-base)
MBSF Chronic Conditions (2014-2016): Supplement to MBSF that contains flags for 27 chronic conditions (https://resdac.org/cms-data/files/mbsf-cc)
MBSF Other Chronic Conditions (2014-2016): Supplement to MBSF that contains flags for 35 chronic conditions (https://resdac.org/cms-data/files/mbsf-other-conditions)

Files created: 
medparOD_allODs: Records for all opioid overdoses between 2011-2017 in inpatient hospital
outpatientOD_allODs: Records for all opioid overdoses between 2011-2017 in outpatient facility
allODdts_allbenes_complete: File of all inpatient and outpatient overdoses between 2011-2017 (contains basic data only, e.g., date, location, discharge status)
allODdts_studymbsfbenes: File of all inpatient and outpatient overdoses for study cohort only between 2011-2017 (contains basic data only, e.g., date, location, discharge status)
firstODdts_2014-2016: Information for the first overdose between 2014-2016 that fulfilled study criteria
mbsfOD:
deaths2014thru2017: All deaths between 2014-2017 (identified using MBSF base)
mbsf



This script identifies all dually eligible Medicare and Medicaid beneficiaries under the age of 65 who survived an opioid overdose that resulted in an emergency department visit between 2014-2016. 12-month mortality for this population was calculated as deaths occurring within 12 months of the index overdose date (date of discharge from inpatient or outpatient facility). 
* In determining eligibility, the MBSF was used for the years preceding and following the overdose date. Beneficiaries needed to have at least 6 months of full dual status before the index date and for 12 months following the overdose (or until death). 


Also identified in this script are control beneficiaries for this study: beneficiaries who fulfilled all of the study inclusion criteria but did not overdose in 2015. 12-month mortality for this population was calculated as deaths occurring after January 1, 2015. 




