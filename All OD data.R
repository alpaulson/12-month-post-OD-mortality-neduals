## ----SETUP------------------------------------------------------------------------------------------------------

library(dplyr)
library(data.table)
library(pacman)
library(lubridate)
library(zoo)

`%notin%` <- Negate(`%in%`)

knitr::opts_chunk$set(echo = TRUE)





## ----OPEN FILES-------------------------------------------------------------------------------------------------

medpar2011_all <- fread("data/2011/under65/medpar_under65.csv")
medpar2012_all <- fread("data/2012/under65/medpar_under65.csv")
medpar2013_all <- fread("data/2013/under65/medpar_under65.csv")
medpar2014_all <- fread("data/2014/under65/medpar_under65.csv")
medpar2015_all <- fread("data/2015/under65/medpar_under65.csv")
medpar2016_all <- fread("data/2016/under65/medpar_under65.csv")
medpar2017_all <- fread("data/2017/under65/medpar_under65.csv")

outpatient2011_all <- fread("data/2012/under65/outpatient_under65.csv")
outpatient2012_all <- fread("data/2012/under65/outpatient_under65.csv")
outpatient2013_all <- fread("data/2013/under65/outpatient_under65.csv")
outpatient2014_all <- fread("data/2014/under65/outpatient_under65.csv")
outpatient2015_all <- fread("data/2015/under65/outpatient_under65.csv")
outpatient2016_all <- fread("data/2016/under65/outpatient_under65.csv")
outpatient2017_all <- fread("data/2017/under65/outpatient_under65.csv")

# ALL THE REV RECORDS FOR 2014-2017 FILTERED FOR BENES WHO HAD OUTPATIENT OD (file created below) 

outptrev2014thru2017_allODs <- readRDS(file = "outptrev2014thru2017_allODs.Rds")

mbsf_base2013 <- fread("data/2013/under65/mbsf_under65.csv")
mbsf_base2014 <- fread("data/2014/under65/mbsf_under65.csv")
mbsf_base2015 <- fread("data/2015/under65/mbsf_under65.csv")
mbsf_base2016 <- fread("data/2016/under65/mbsf_under65.csv")
mbsf_base2017 <- fread("data/2017/under65/mbsf_under65.csv")

mbsf_cc_summary2014 <- fread("data/2014/under65/cc_under65.csv")
mbsf_oth_cc_summary2014 <- fread("data/2014/under65/othcc_under65.csv")
cca2014 <- merge(mbsf_base2014, mbsf_cc_summary2014, all.x = TRUE, all.y = FALSE)
ccb2014 <- merge(cca2014, mbsf_oth_cc_summary2014, all.x = TRUE, all.y = FALSE)
mbsf2014_full <- ccb2014

mbsf_cc_summary2015 <- fread("data/2015/under65/cc_under65.csv")
mbsf_oth_cc_summary2015 <- fread("data/2015/under65/othcc_under65.csv")
cca2015 <- merge(mbsf_base2015, mbsf_cc_summary2015, all.x = TRUE, all.y = FALSE)
ccb2015 <- merge(cca2015, mbsf_oth_cc_summary2015, all.x = TRUE, all.y = FALSE)
mbsf2015_full <- ccb2015

mbsf_cc_summary2016 <- fread("data/2016/under65/cc_under65.csv")
mbsf_oth_cc_summary2016 <- fread("data/2016/under65/othcc_under65.csv")
cca2016 <- merge(mbsf_base2016, mbsf_cc_summary2016, all.x = TRUE, all.y = FALSE)
ccb2016 <- merge(cca2016, mbsf_oth_cc_summary2016, all.x = TRUE, all.y = FALSE)
mbsf2016_full <- ccb2016

medparOD_allODs <- readRDS(file = "medparOD_allODs.Rds")
outptOD_allODs <- readRDS(file = "outptOD_allODs.Rds")

allODdts_allbenes_complete <- readRDS(file = "allODdts_allbenes_complete.Rds")

allODdts_studymbsfbenes <- readRDS(file = "allODdts_studymbsfbenes.Rds")
firstODdt_2014thru2016 <- readRDS(file = "firstODdt_2014thru2016.Rds")

mbsfOD <- readRDS(file = "mbsfOD.Rds")



## ----IDENTIFY ODs-----------------------------------------------------------------------------------------------

ODs_icd9 <- c('96500','96501','96502','96509','E8500', 'E8501','E8502','E9350','E9351','E9352')
ODs_icd10 <- "T400|T401|T402|T403|T404|T406"

# 2011 ####

####### Medpar #

medparOD2011_all <- medpar2011_all %>%
  filter_at(vars(ADMTG_DGNS_CD:DGNS_E_12_CD), any_vars(. %in% ODs_icd9))

n_distinct(medparOD2011_all$BENE_ID) 

paste0("There were ",n_distinct(medparOD2011_all$BENE_ID)," inpatient ODs in 2011.")


####### Outpatient #

outpatientOD2011_all <- outpatient2011_all %>% filter_at(vars(PRNCPAL_DGNS_CD:RSN_VISIT_CD3),
                                                 any_vars(. %in% ODs_icd9))

paste0("There were ",n_distinct(outpatientOD2011_all$BENE_ID) ," outpatient ODs in 2011.")

# 2012 ####

####### Medpar #

medparOD2012_all <- medpar2012_all %>%
  filter_at(vars(ADMTG_DGNS_CD:DGNS_E_12_CD), any_vars(. %in% ODs_icd9))

paste0("There were ",n_distinct(medparOD2012_all$BENE_ID)," inpatient ODs in 2012.")


####### Outpatient #

outpatientOD2012_all <- outpatient2012_all %>% filter_at(vars(PRNCPAL_DGNS_CD:RSN_VISIT_CD3),
                                                 any_vars(. %in% ODs_icd9))

paste0("There were ",n_distinct(outpatientOD2012_all$BENE_ID) ," outpatient ODs in 2012.")

# 2013 ####

####### Medpar #

medparOD2013_all <- medpar2013_all %>%
  filter_at(vars(ADMTG_DGNS_CD:DGNS_E_12_CD), any_vars(. %in% ODs_icd9))


paste0("There were ",n_distinct(medparOD2013_all$BENE_ID)," inpatient ODs in 2013.")


####### Outpatient #

outpatientOD2013_all <- outpatient2013_all %>% filter_at(vars(PRNCPAL_DGNS_CD:RSN_VISIT_CD3),
                                                 any_vars(. %in% ODs_icd9))

paste0("There were ",n_distinct(outpatientOD2013_all$BENE_ID)," outpatient ODs in 2013.")


# 2014 ####

####### Medpar #

medparOD2014_all <- medpar2014_all %>%
  filter_at(vars(ADMTG_DGNS_CD:DGNS_E_12_CD), any_vars(. %in% ODs_icd9))

paste0("There were ",n_distinct(medparOD2014_all$BENE_ID)," inpatient ODs in 2014.")



####### Outpatient #

outpatientOD2014_all <- outpatient2014_all %>% filter_at(vars(PRNCPAL_DGNS_CD:RSN_VISIT_CD3),
                                                 any_vars(. %in% ODs_icd9))

paste0("There were ",n_distinct(outpatientOD2014_all$BENE_ID)," outpatient ODs in 2014.")


# 2015 ####

# Before Oct 1 used ICD-9; after used ICD-10

# The "T" codes have various endings to indicate intention, whether it is the initial encounter, and other information. Using "grep" captures all the possiblities for each of the dx codes. (see: https://icd.codes/icd10cm/T401)

####### Medpar #

which(colnames(medpar2015_all)=="ADMTG_DGNS_CD") # column 124
which(colnames(medpar2015_all)=="DGNS_25_CD") # column 177

medpar2015_icd10 <- medpar2015_all[,c(124:177)]

medpar2015_icd10 <- unique(unlist(lapply(medpar2015_icd10, grep, pattern=ODs_icd10, value=TRUE)))

medparOD2015_all <- medpar2015_all %>%
  filter_at(vars(ADMTG_DGNS_CD:DGNS_E_12_CD), any_vars(. %in% c(medpar2015_icd10, ODs_icd9)))

paste0("There were ",n_distinct(medparOD2015_all$BENE_ID)," inpatient ODs in 2015.")


####### Outpatient #

which(colnames(outpatient2015_all)=="PRNCPAL_DGNS_CD") # column 41
which(colnames(outpatient2015_all)=="RSN_VISIT_CD3") # column 132

outpt2015_icd10 <- outpatient2015_all[,c(41:132)]

outpt2015_icd10 <- unique(unlist(lapply(outpt2015_icd10, grep, pattern=ODs_icd10, value=TRUE)))

outpatientOD2015_all <- outpatient2015_all %>%
  filter_at(vars(PRNCPAL_DGNS_CD:RSN_VISIT_CD3), any_vars(. %in% c(outpt2015_icd10, ODs_icd9)))

paste0("There were ",n_distinct(outpatientOD2015_all$BENE_ID)," outpatient ODs in 2015.")


# 2016 ####

####### Medpar #

medpar2016_icd10 <- medpar2016_all[,c(124:177)]

medpar2016_icd10 <- unique(unlist(lapply(medpar2016_icd10, grep, pattern=ODs_icd10, value=TRUE)))

medparOD2016_all <- medpar2016_all %>%
  filter_at(vars(ADMTG_DGNS_CD:DGNS_E_12_CD), any_vars(. %in% c(medpar2016_icd10)))

paste0("There were ",n_distinct(medparOD2016_all$BENE_ID)," inpatient ODs in 2016.")


####### Outpatient #

outpt2016_icd10 <- outpatient2016_all[,c(41:132)]

outpt2016_icd10 <- unique(unlist(lapply(outpt2016_icd10, grep, pattern=ODs_icd10, value=TRUE)))

outpatientOD2016_all <- outpatient2016_all %>%
  filter_at(vars(PRNCPAL_DGNS_CD:RSN_VISIT_CD3), any_vars(. %in% c(outpt2016_icd10)))

paste0("There were ",n_distinct(outpatientOD2016_all$BENE_ID)," outpatient ODs in 2016.")


# 2017 ####

medpar2017_icd10 <- medpar2017_all[,c(124:177)]

medpar2017_icd10 <- unique(unlist(lapply(medpar2017_icd10, grep, pattern=ODs_icd10, value=TRUE)))

medparOD2017_all <- medpar2017_all %>%
  filter_at(vars(ADMTG_DGNS_CD:DGNS_E_12_CD), any_vars(. %in% c(medpar2017_icd10)))

paste0("There were ",n_distinct(medparOD2017_all$BENE_ID)," inpatient ODs in 2017.")


####### Outpatient #

outpt2017_icd10 <- outpatient2017_all[,c(41:132)]

outpt2017_icd10 <- unique(unlist(lapply(outpt2017_icd10, grep, pattern=ODs_icd10, value=TRUE)))

outpatientOD2017_all <- outpatient2017_all %>%
  filter_at(vars(PRNCPAL_DGNS_CD:RSN_VISIT_CD3), any_vars(. %in% c(outpt2017_icd10)))

paste0("There were ",n_distinct(outpatientOD2017_all$BENE_ID)," outpatient ODs in 2017.")



## ----Merge each MedPAR OD by yr---------------------------------------------------------------------------------

# Remove extra columns (not necessary for analysis)

medparOD2011_all$V1 <- NULL
medparOD2012_all$V1 <- NULL
medparOD2013_all$V1 <- NULL
medparOD2014_all$V1 <- NULL
medparOD2015_all$V1 <- NULL
medparOD2016_all$V1 <- NULL
medparOD2017_all$V1 <- NULL

medparOD2016_all$ACO_ID_NUM <- NULL
medparOD2017_all$ACO_ID_NUM <- NULL

medparOD2016_all$RC_ALLOGENEIC_STEM_CELL_AMT <- NULL
medparOD2017_all$RC_ALLOGENEIC_STEM_CELL_AMT <- NULL

medparOD2016_all$ISLET_ADD_ON_PYMT_AMT <- NULL
medparOD2017_all$ISLET_ADD_ON_PYMT_AMT <- NULL

medparOD2017_all$CLM_IP_INITL_MS_DRG_CD <- NULL
medparOD2017_all$VAL_CD_Q1_PYMT_RDCTN_AMT <- NULL

medparOD_allODs <- rbind(medparOD2014_all,medparOD2015_all,medparOD2016_all,medparOD2017_all)

paste0("A total of ",n_distinct(medparOD_allODs$BENE_ID)," beneficiaries had inpatient OD records between 2011-2017.")

saveRDS(medparOD_allODs, file = "medparOD_allODs.Rds")

rm(medparOD2011_all,medparOD2012_all,medparOD2013_all,medparOD2014_all,medparOD2015_all,medparOD2016_all,medparOD2017_all) # Remove single year OD files

rm(medpar2011_all,medpar2012_all) # Keep 2013-2017 files for health care utilization 



## ----Merge each Outpt OD by yr----------------------------------------------------------------------------------

# Remove extra columns (not necessary for analysis)

outpatientOD2011_all$V1 <- NULL
outpatientOD2012_all$V1 <- NULL
outpatientOD2013_all$V1 <- NULL
outpatientOD2014_all$V1 <- NULL
outpatientOD2015_all$V1 <- NULL
outpatientOD2016_all$V1 <- NULL
outpatientOD2017_all$V1 <- NULL

outpatientOD2017_all$CLM_RSDL_PYMT_IND_CD <- NULL
outpatientOD2017_all$PRVDR_VLDTN_TYPE_CD <- NULL
outpatientOD2017_all$RR_BRD_EXCLSN_IND_SW <- NULL
outpatientOD2017_all$CLM_MODEL_REIMBRSMT_AMT <- NULL

outptOD_allODs <- rbind(outpatientOD2011_all, outpatientOD2012_all, outpatientOD2013_all, outpatientOD2014_all,outpatientOD2015_all,outpatientOD2016_all,outpatientOD2017_all)

paste0("A total of ",n_distinct(outptOD_allODs$BENE_ID)," beneficiaries had outpatient OD records between 2011-2017.")

saveRDS(outptOD_allODs, file = "outptOD_allODs.Rds")

rm(outpatientOD2011_all,outpatientOD2012_all,outpatientOD2013_all,outpatientOD2014_all,outpatientOD2015_all,outpatientOD2016_all,outpatientOD2017_all) # Remove single year OD files

rm(outpatient2011_all,outpatient2012_all) # Keep 2013-2017 files for health care utilization 



## ----Merge medpar & outpt all ODs-------------------------------------------------------------------------------

# All dates in medpar and outpatient in date format

medparOD_allODs$ADMSN_DT <- as.Date(medparOD_allODs$ADMSN_DT, "%d%b%Y")
medparOD_allODs$DSCHRG_DT <- as.Date(medparOD_allODs$DSCHRG_DT, "%d%b%Y")

outptOD_allODs$CLM_FROM_DT <- as.Date(outptOD_allODs$CLM_FROM_DT, "%d%b%Y")
outptOD_allODs$CLM_THRU_DT <- as.Date(outptOD_allODs$CLM_THRU_DT, "%d%b%Y")

# Subset from both files: Bene ID, claim ID, discharge date, discharge destination code 

medparODs_subset <- medparOD_allODs[,c("BENE_ID","MEDPAR_ID","DSCHRG_DT","DSCHRG_DSTNTN_CD","ADMSN_DT")] 

outptODs_subset <- outptOD_allODs[,c("BENE_ID","CLM_ID","CLM_FROM_DT","PTNT_DSCHRG_STUS_CD","CLM_THRU_DT")]

# Rename columns, add indicator for location, and merge

setnames(medparODs_subset, 
         old = c("MEDPAR_ID","DSCHRG_DT","ADMSN_DT"), 
         new = c("CLM_ID","IP_DSCHRG_DT","IP_ADMSN_DT")) 

medparODs_subset$outpt1 <- rep(0,nrow(medparODs_subset))

setnames(outptODs_subset, 
         old = c("CLM_FROM_DT","CLM_THRU_DT","PTNT_DSCHRG_STUS_CD"), 
         new = c("OP_CLM_FROM_DT","OP_CLM_THRU_DT","DSCHRG_DSTNTN_CD")) 

outptODs_subset$outpt1 <- rep(1,nrow(outptODs_subset))

allODdts_allbenes_complete <- merge(medparODs_subset, outptODs_subset, all.x=TRUE, all.y=TRUE)

allODdts_allbenes_complete$EVENT_DT <- coalesce(allODdts_allbenes_complete$IP_ADMSN_DT, allODdts_allbenes_complete$OP_CLM_THRU_DT)

paste0("A total of ",n_distinct(allODdts_allbenes_complete$BENE_ID)," beneficiaries had at least one inpatient or outpatient OD claim between 2011-2017.")





## ----Outpatient rev-filter--------------------------------------------------------------------------------------

# 2014 ####

# Read in full outpatient rev files

outpatientrev_part1 <- fread("data/2014/under65/outpatientrev_part1_under65.csv")
outpatientrev_part2 <- fread("data/2014/under65/outpatientrev_part2_under65.csv")
outpatientrev_part3 <- fread("data/2014/under65/outpatientrev_part3_under65.csv")
outpatientrev_part4 <- fread("data/2014/under65/outpatientrev_part4_under65.csv")

# Limit to all who OD'd in ER 

outpatientrev_part1_ODs <- filter(outpatientrev_part1, CLM_ID %in% outpatientOD_allODs$CLM_ID)
outpatientrev_part2_ODs <- filter(outpatientrev_part2, CLM_ID %in% outpatientOD_allODs$CLM_ID)
outpatientrev_part3_ODs <- filter(outpatientrev_part3, CLM_ID %in% outpatientOD_allODs$CLM_ID)
outpatientrev_part4_ODs <- filter(outpatientrev_part4, CLM_ID %in% outpatientOD_allODs$CLM_ID)

outpatientrev_part1_ODs$V1 <- NULL
outpatientrev_part2_ODs$V1 <- NULL
outpatientrev_part3_ODs$V1 <- NULL
outpatientrev_part4_ODs$V1 <- NULL

outpatientrev2014_ODs <- rbind(outpatientrev_part1_ODs,
                           outpatientrev_part2_ODs,
                           outpatientrev_part3_ODs,
                           outpatientrev_part4_ODs)

rm(outpatientrev_part1, outpatientrev_part2, outpatientrev_part3, outpatientrev_part4,
   outpatientrev_part1_ODs,outpatientrev_part2_ODs,outpatientrev_part3_ODs,outpatientrev_part4_ODs)


# 2015 ####

# Read in full outpatient rev files

outpatientrev_part1 <- fread("data/2015/under65/outpatientrev_part1_under65.csv")
outpatientrev_part2 <- fread("data/2015/under65/outpatientrev_part2_under65.csv")
outpatientrev_part3 <- fread("data/2015/under65/outpatientrev_part3_under65.csv")
outpatientrev_part4 <- fread("data/2015/under65/outpatientrev_part4_under65.csv")

# Limit to all who OD'd in ER 

outpatientrev_part1_ODs <- filter(outpatientrev_part1, CLM_ID %in% outpatientOD_allODs$CLM_ID)
outpatientrev_part2_ODs <- filter(outpatientrev_part2, CLM_ID %in% outpatientOD_allODs$CLM_ID)
outpatientrev_part3_ODs <- filter(outpatientrev_part3, CLM_ID %in% outpatientOD_allODs$CLM_ID)
outpatientrev_part4_ODs <- filter(outpatientrev_part4, CLM_ID %in% outpatientOD_allODs$CLM_ID)

outpatientrev_part1_ODs$V1 <- NULL
outpatientrev_part2_ODs$V1 <- NULL
outpatientrev_part3_ODs$V1 <- NULL
outpatientrev_part4_ODs$V1 <- NULL

outpatientrev2015_ODs <- rbind(outpatientrev_part1_ODs,
                           outpatientrev_part2_ODs,
                           outpatientrev_part3_ODs,
                           outpatientrev_part4_ODs)

rm(outpatientrev_part1, outpatientrev_part2, outpatientrev_part3, outpatientrev_part4,
   outpatientrev_part1_ODs,outpatientrev_part2_ODs,outpatientrev_part3_ODs,outpatientrev_part4_ODs)


# 2016 ####

# Read in full outpatient rev files

outpatientrev_part1 <- fread("data/2016/under65/outpatientrev_part1_under65.csv")
outpatientrev_part2 <- fread("data/2016/under65/outpatientrev_part2_under65.csv")
outpatientrev_part3 <- fread("data/2016/under65/outpatientrev_part3_under65.csv")
outpatientrev_part4 <- fread("data/2016/under65/outpatientrev_part4_under65.csv")

# Limit to all who OD'd in ER 

outpatientrev_part1_ODs <- filter(outpatientrev_part1, CLM_ID %in% outpatientOD_allODs$CLM_ID)
outpatientrev_part2_ODs <- filter(outpatientrev_part2, CLM_ID %in% outpatientOD_allODs$CLM_ID)
outpatientrev_part3_ODs <- filter(outpatientrev_part3, CLM_ID %in% outpatientOD_allODs$CLM_ID)
outpatientrev_part4_ODs <- filter(outpatientrev_part4, CLM_ID %in% outpatientOD_allODs$CLM_ID)

outpatientrev_part1_ODs$V1 <- NULL
outpatientrev_part2_ODs$V1 <- NULL
outpatientrev_part3_ODs$V1 <- NULL
outpatientrev_part4_ODs$V1 <- NULL

outpatientrev2016_ODs <- rbind(outpatientrev_part1_ODs,
                           outpatientrev_part2_ODs,
                           outpatientrev_part3_ODs,
                           outpatientrev_part4_ODs)

rm(outpatientrev_part1, outpatientrev_part2, outpatientrev_part3, outpatientrev_part4,
   outpatientrev_part1_ODs,outpatientrev_part2_ODs,outpatientrev_part3_ODs,outpatientrev_part4_ODs)


# 2017 ####

# Read in full outpatient rev files

outpatientrev_part1 <- fread("data/2017/under65/outpatientrev_part1_under65.csv")
outpatientrev_part2 <- fread("data/2017/under65/outpatientrev_part2_under65.csv")
outpatientrev_part3 <- fread("data/2017/under65/outpatientrev_part3_under65.csv")
outpatientrev_part4 <- fread("data/2017/under65/outpatientrev_part4_under65.csv")

# Limit to all who OD'd in ER 

outpatientrev_part1_ODs <- filter(outpatientrev_part1, CLM_ID %in% outpatientOD_allODs$CLM_ID)
outpatientrev_part2_ODs <- filter(outpatientrev_part2, CLM_ID %in% outpatientOD_allODs$CLM_ID)
outpatientrev_part3_ODs <- filter(outpatientrev_part3, CLM_ID %in% outpatientOD_allODs$CLM_ID)
outpatientrev_part4_ODs <- filter(outpatientrev_part4, CLM_ID %in% outpatientOD_allODs$CLM_ID)

outpatientrev_part1_ODs$V1 <- NULL
outpatientrev_part2_ODs$V1 <- NULL
outpatientrev_part3_ODs$V1 <- NULL
outpatientrev_part4_ODs$V1 <- NULL

outpatientrev2017_ODs <- rbind(outpatientrev_part1_ODs,
                           outpatientrev_part2_ODs,
                           outpatientrev_part3_ODs,
                           outpatientrev_part4_ODs)

rm(outpatientrev_part1, outpatientrev_part2, outpatientrev_part3, outpatientrev_part4,
   outpatientrev_part1_ODs,outpatientrev_part2_ODs,outpatientrev_part3_ODs,outpatientrev_part4_ODs)

# Merge all outpatient OD rev files

colnames(outpatientrev2016_ODs)
colnames(outpatientrev2017_ODs)

outpatientrev2017_ODs$REV_CNTR_RP_IND_CD <- NULL
outpatientrev2017_ODs$RC_MODEL_REIMBRSMT_AMT <- NULL

x.diff <- list(setdiff(colnames(outpatientrev2016_ODs),colnames(outpatientrev2017_ODs)))
y.diff <- list(setdiff(colnames(outpatientrev2017_ODs),colnames(outpatientrev2016_ODs)))

x.diff[[1]]
y.diff[[1]]

# 2017 file has "SAS short names" for variables instead of long variable names found in every other file. The variables map to each other so replacing colnames for 2017 with colnames for 2016.

colnames(outpatientrev2017_ODs) <- colnames(outpatientrev2016_ODs)

outptrev2014thru2017_allODs <- rbind(outpatientrev2014_ODs,outpatientrev2015_ODs,outpatientrev2016_ODs,outpatientrev2017_ODs)

saveRDS(outptrev2014thru2017_allODs, file = "outptrev2014thru2017_allODs.Rds")
rm(outpatientrev2014_ODs,outpatientrev2015_ODs,outpatientrev2016_ODs,outpatientrev2017_ODs,x.diff, y.diff)



## ----Outpt ODER-------------------------------------------------------------------------------------------------

# Use outpatient rev file to find ERs claims

outpatientER <- outptrev2014thru2017_allODs %>% filter(REV_CNTR == 450 | REV_CNTR == 451 | REV_CNTR == 452 | REV_CNTR == 456 | REV_CNTR == 459 | REV_CNTR == 981) 

# The revenue file has multiple lines for each outpatient claim (one per charge) so only 1 line of the many indicates ER for the claim. Claims in the outpatientrev_ODs file that do not have one of the rev center codes as a line item for the claim occurred elsewhere, but it is not always clear where. I'm excluding these claims.

# 3 - Create outpatient file for duals + nonduals who OD in outpatient ER. 

outpt2014thru2016_allODs <- filter(outptOD_allODs, CLM_THRU_DT > "2013-12-31" & CLM_THRU_DT < "2017-01-01")

outpt2014thru2016_allODERs <- filter(outpt2014thru2016_allODs, CLM_ID %in% outpatientER$CLM_ID)



## ----Medpar ODER------------------------------------------------------------------------------------------------

# Date is already in proper format from all OD dts. 

# NOTE: The index date for the study is the discharge date from the hospital to allow for 12-month follow-up. 

medpar2014thru2016_allODs <- filter(medparOD_allODs, DSCHRG_DT > "2013-12-31" & DSCHRG_DT < "2017-01-01")

medpar2014thru2016_allODERs <- medpar2014thru2016_allODs %>% filter(ER_CHRG_AMT > 0) 



## ----Merge medpar & outpt ODER----------------------------------------------------------------------------------

# Subset from both files: Bene ID, claim ID, discharge date, discharge destination code 

medparODERs_subset <- medpar2014thru2016_allODERs[,c("BENE_ID","MEDPAR_ID","DSCHRG_DT","DSCHRG_DSTNTN_CD","ADMSN_DT")] 

outptODERs_subset <- outpt2014thru2016_allODERs[,c("BENE_ID","CLM_ID","CLM_FROM_DT","PTNT_DSCHRG_STUS_CD","CLM_THRU_DT")]

# Rename columns, add indicator for location, and merge

setnames(medparODERs_subset, 
         old = c("MEDPAR_ID","DSCHRG_DT","ADMSN_DT"), 
         new = c("CLM_ID","IP_DSCHRG_DT","IP_ADMSN_DT")) 

medparODERs_subset$outpt1 <- rep(0,nrow(medparODERs_subset))

setnames(outptODERs_subset, 
         old = c("CLM_FROM_DT","CLM_THRU_DT","PTNT_DSCHRG_STUS_CD"), 
         new = c("OP_CLM_FROM_DT","OP_CLM_THRU_DT","DSCHRG_DSTNTN_CD")) 

outptODERs_subset$outpt1 <- rep(1,nrow(outptODERs_subset))

allODERdts_allbenes_complete <- merge(medparODERs_subset, outptODERs_subset, all.x=TRUE, all.y=TRUE)

allODERdts_allbenes_complete$EVENT_DT <- coalesce(allODERdts_allbenes_complete$IP_ADMSN_DT, allODERdts_allbenes_complete$OP_CLM_THRU_DT)

allODERdts_allbenes_complete$INDEX_DT <- coalesce(allODERdts_allbenes_complete$IP_DSCHRG_DT, allODERdts_allbenes_complete$OP_CLM_THRU_DT)

paste0("A total of ",n_distinct(allODERdts_allbenes_complete$BENE_ID)," beneficiaries had at least one OD in an inpatient or outpatient ER between 2014-2016.")



## ----Subset OD dfs by year--------------------------------------------------------------------------------------

# Remove extra columns that won't be used to determine dual status

allODER_2014thru2016_subset <- allODERdts_allbenes_complete[,c("BENE_ID","CLM_ID","INDEX_DT")]

# 2014 ODs ####

ODs_2014 <- subset(allODER_2014thru2016_subset, INDEX_DT < "2015-01-01")

paste0("In 2014, ",n_distinct(ODs_2014$BENE_ID)," beneficiaries were discharged after an OD in an inpatient or outpatient ER.")

# 2015 ODs ####

ODs_2015 <- subset(allODER_2014thru2016_subset, INDEX_DT >= "2015-01-01" & INDEX_DT < "2016-01-01")


paste0("In 2015, ",n_distinct(ODs_2015$BENE_ID)," beneficiaries were discharged after an OD in an inpatient or outpatient ER.")

# 2016 ODs ####

ODs_2016 <- subset(allODER_2014thru2016_subset, INDEX_DT >= "2016-01-01" & INDEX_DT < "2017-01-01")

paste0("In 2016, ",n_distinct(ODs_2016$BENE_ID)," beneficiaries were discharged after an OD in an inpatient or outpatient ER.")




## ----combine 2014 ODs-dual status-------------------------------------------------------------------------------

##### Filter mbsfs for dual and HMO status of 2014 ODs #

mbsf2013_ODpop14 <- filter(mbsf_base2013, BENE_ID %in% ODs_2014$BENE_ID)
mbsf2014_ODpop14 <- filter(mbsf_base2014, BENE_ID %in% ODs_2014$BENE_ID)
mbsf2015_ODpop14 <- filter(mbsf_base2015, BENE_ID %in% ODs_2014$BENE_ID)

# full dual indicator columns

# colnames(mbsf2013_ODpop14)

mbsf2013_ODpop14 <- mbsf2013_ODpop14 %>%
  mutate(across(c(163:174), ~ ifelse(.x ==2 | .x ==4| .x ==8,1,0), .names = "{col}_2013")) 

mbsf2014_ODpop14 <- mbsf2014_ODpop14 %>%
  mutate(across(c(163:174), ~ ifelse(.x ==2 | .x ==4| .x ==8,1,0), .names = "{col}_2014")) 

mbsf2015_ODpop14 <- mbsf2015_ODpop14 %>%
  mutate(across(c(163:174), ~ ifelse(.x ==2 | .x ==4| .x ==8,1,0), .names = "{col}_2015"))

# HMO NOTE: Using 99 as my 'Yes' indicator so easy to distinguish when I determine eligibility later

mbsf2013_ODpop14b <- mbsf2013_ODpop14 %>%
  mutate(across(c(67:78), ~ ifelse(.x ==0,0,99), .names = "{col}_2013")) 

mbsf2014_ODpop14b <- mbsf2014_ODpop14 %>%
  mutate(across(c(67:78), ~ ifelse(.x ==0,0,99), .names = "{col}_2014"))

mbsf2015_ODpop14b <- mbsf2015_ODpop14 %>%
  mutate(across(c(67:78), ~ ifelse(.x ==0,0,99), .names = "{col}_2015"))

# Dual status dataframe

benestatus_2013 <- mbsf2013_ODpop14b[,c(2,187:210)]
benestatus_2014 <- mbsf2014_ODpop14b[,c(2,187:210)]
benestatus_2015 <- mbsf2015_ODpop14b[,c(2,187:210)]

# Merge all together

# merge in two steps bc multiple dfs combining rows and columns 

benestatus_13.14.15 <- merge(benestatus_2013,benestatus_2014, all.x = TRUE, all.y = TRUE) 
mbsf_ODpop14 <- merge(benestatus_13.14.15,benestatus_2015,all.x = TRUE, all.y = TRUE)

# n_distinct(mbsf_ODpop14$BENE_ID) 



## ----combine 2015 ODs-dual status-------------------------------------------------------------------------------

# Filter mbsfs for 2015 ODs

mbsf2014_ODpop15 <- filter(mbsf_base2014, BENE_ID %in% ODs_2015$BENE_ID)
mbsf2015_ODpop15 <- filter(mbsf_base2015, BENE_ID %in% ODs_2015$BENE_ID)
mbsf2016_ODpop15 <- filter(mbsf_base2016, BENE_ID %in% ODs_2015$BENE_ID)

# full dual indicator columns

mbsf2014_ODpop15 <- mbsf2014_ODpop15 %>%
  mutate(across(c(163:174), ~ ifelse(.x ==2 | .x ==4| .x ==8,1,0), .names = "{col}_2014")) 

mbsf2015_ODpop15 <- mbsf2015_ODpop15 %>%
  mutate(across(c(163:174), ~ ifelse(.x ==2 | .x ==4| .x ==8,1,0), .names = "{col}_2015")) 

mbsf2016_ODpop15 <- mbsf2016_ODpop15 %>%
  mutate(across(c(163:174), ~ ifelse(.x ==2 | .x ==4| .x ==8,1,0), .names = "{col}_2016"))

# HMO

mbsf2014_ODpop15b <- mbsf2014_ODpop15 %>%
  mutate(across(c(67:78), ~ ifelse(.x ==0,0,99), .names = "{col}_2014")) 

mbsf2015_ODpop15b <- mbsf2015_ODpop15 %>%
  mutate(across(c(67:78), ~ ifelse(.x ==0,0,99), .names = "{col}_2015"))

mbsf2016_ODpop15b <- mbsf2016_ODpop15 %>%
  mutate(across(c(67:78), ~ ifelse(.x ==0,0,99), .names = "{col}_2016"))

# Dual status dataframe

benestatus_2014 <- mbsf2014_ODpop15b[,c(2,187:210)]
benestatus_2015 <- mbsf2015_ODpop15b[,c(2,187:210)]
benestatus_2016 <- mbsf2016_ODpop15b[,c(2,187:210)]

# Merge all together

# merge in two steps bc multiple dfs 

benestatus_14.15.16 <- merge(benestatus_2014,benestatus_2015, all.x = TRUE, all.y = TRUE) 
mbsf_ODpop15 <- merge(benestatus_14.15.16,benestatus_2016,all.x = TRUE, all.y = TRUE)

# n_distinct(mbsf_ODpop15$BENE_ID) 



## ----2016 combine ODs-dual status-------------------------------------------------------------------------------

# Filter mbsfs for 2016 ODs

mbsf2015_ODpop16 <- filter(mbsf_base2015, BENE_ID %in% ODs_2016$BENE_ID)
mbsf2016_ODpop16 <- filter(mbsf_base2016, BENE_ID %in% ODs_2016$BENE_ID)
mbsf2017_ODpop16 <- filter(mbsf_base2017, BENE_ID %in% ODs_2016$BENE_ID)

# full dual indicator columns

mbsf2015_ODpop16 <- mbsf2015_ODpop16 %>%
  mutate(across(c(163:174), ~ ifelse(.x ==2 | .x ==4| .x ==8,1,0), .names = "{col}_2015")) 

mbsf2016_ODpop16 <- mbsf2016_ODpop16 %>%
  mutate(across(c(163:174), ~ ifelse(.x ==2 | .x ==4| .x ==8,1,0), .names = "{col}_2016")) 

mbsf2017_ODpop16 <- mbsf2017_ODpop16 %>%
  mutate(across(c(163:174), ~ ifelse(.x ==2 | .x ==4| .x ==8,1,0), .names = "{col}_2017"))

# HMO

mbsf2015_ODpop16b <- mbsf2015_ODpop16 %>%
  mutate(across(c(67:78), ~ ifelse(.x ==0,0,99), .names = "{col}_2015")) 

mbsf2016_ODpop16b <- mbsf2016_ODpop16 %>%
  mutate(across(c(67:78), ~ ifelse(.x ==0,0,99), .names = "{col}_2016"))

mbsf2017_ODpop16b <- mbsf2017_ODpop16 %>%
  mutate(across(c(67:78), ~ ifelse(.x ==0,0,99), .names = "{col}_2017"))

# Dual status dataframe

benestatus_2015 <- mbsf2015_ODpop16b[,c(2,187:210)]
benestatus_2016 <- mbsf2016_ODpop16b[,c(2,187:210)]
benestatus_2017 <- mbsf2017_ODpop16b[,c(2,187:210)]

# Merge all together

# merge in two steps bc multiple dfs 

benestatus_15.16.17 <- merge(benestatus_2015,benestatus_2016, all.x = TRUE, all.y = TRUE) 
mbsf_ODpop16 <- merge(benestatus_15.16.17,benestatus_2017,all.x = TRUE, all.y = TRUE)

# n_distinct(mbsf_ODpop16$BENE_ID)



## ----one MBSF + OD df-------------------------------------------------------------------------------------------

mbsf_ODpop14$YEAR <- rep('2014',nrow(mbsf_ODpop14))
mbsf_ODpop15$YEAR <- rep('2015',nrow(mbsf_ODpop15))
mbsf_ODpop16$YEAR <- rep('2016',nrow(mbsf_ODpop16))

# Merge each mbsf with the corresponding OD df (Bene ID, claim ID, claim date + mbsf eligibility info)

ODs_2014 <- data.frame(ODs_2014)
mbsf_ODpop14 <- data.frame(mbsf_ODpop14)

ODs_2015 <- data.frame(ODs_2015)
mbsf_ODpop15 <- data.frame(mbsf_ODpop15)

ODs_2016 <- data.frame(ODs_2016)
mbsf_ODpop16 <- data.frame(mbsf_ODpop16)

ODs_2014_full <- merge(ODs_2014, mbsf_ODpop14, all.x = TRUE, all.y = TRUE)
ODs_2015_full <- merge(ODs_2015, mbsf_ODpop15, all.x = TRUE, all.y = TRUE)
ODs_2016_full <- merge(ODs_2016, mbsf_ODpop16, all.x = TRUE, all.y = TRUE)

# Merge OD dfs

ODs_2014.2015 <- merge(ODs_2015_full, ODs_2014_full, all.x = TRUE, all.y = TRUE)
ODs_2014thru2016 <- merge(ODs_2014.2015, ODs_2016_full, all.x = TRUE, all.y = TRUE)



## ----Pull death data--------------------------------------------------------------------------------------------

# Add death date

mbsf_base2014$BENE_DEATH_DT <- as.Date(mbsf_base2014$BENE_DEATH_DT, "%d%b%Y")
mbsf_base2015$BENE_DEATH_DT <- as.Date(mbsf_base2015$BENE_DEATH_DT, "%d%b%Y")
mbsf_base2016$BENE_DEATH_DT <- as.Date(mbsf_base2016$BENE_DEATH_DT, "%d%b%Y")
mbsf_base2017$BENE_DEATH_DT <- as.Date(mbsf_base2017$BENE_DEATH_DT, "%d%b%Y")

deaths2014 <- subset(mbsf_base2014,BENE_DEATH_DT > "2013-01-01")
deaths2015 <- subset(mbsf_base2015,BENE_DEATH_DT > "2013-01-01")
deaths2016 <- subset(mbsf_base2016,BENE_DEATH_DT > "2013-01-01")
deaths2017 <- subset(mbsf_base2017,BENE_DEATH_DT > "2013-01-01")

deaths2014 <- deaths2014[,c('BENE_ID', 'BENE_DEATH_DT')]
deaths2015 <- deaths2015[,c('BENE_ID', 'BENE_DEATH_DT')]
deaths2016 <- deaths2016[,c('BENE_ID', 'BENE_DEATH_DT')]
deaths2017 <- deaths2017[,c('BENE_ID', 'BENE_DEATH_DT')]

deaths2014thru2017 <- rbind(deaths2014,deaths2015,deaths2016,deaths2017)

ODs_2014thru2016 <- merge(ODs_2014thru2016,deaths2014thru2017, all.x = TRUE, all.y = FALSE)

ODs_2014thru2016 <- transform(ODs_2014thru2016, BENE_DEATH_DT = as.yearmon(BENE_DEATH_DT))



## ----Reorder columns and add death data-------------------------------------------------------------------------
# Assigning benes who died full dual status & no HMO from their month of death to the end of study.  I am determining eligibility ultimately by summing eligibility within required months, so this will give benes who die and benes who live the same number of eligible months (18). Anyone who dies but did not fulfill the criteria before death will be excluded because the total number of eligible months will not be 18.

colnames(ODs_2014thru2016)

ODs_2014thru2016 <- ODs_2014thru2016[,c("BENE_ID","YEAR","CLM_ID","INDEX_DT","BENE_DEATH_DT",
                                    "DUAL_STUS_CD_01_2013",
                                    "DUAL_STUS_CD_02_2013",
                                    "DUAL_STUS_CD_03_2013",
                                    "DUAL_STUS_CD_04_2013",
                                    "DUAL_STUS_CD_05_2013",
                                    "DUAL_STUS_CD_06_2013",
                                    "DUAL_STUS_CD_07_2013",
                                    "DUAL_STUS_CD_08_2013",
                                    "DUAL_STUS_CD_09_2013",
                                    "DUAL_STUS_CD_10_2013",
                                    "DUAL_STUS_CD_11_2013",
                                    "DUAL_STUS_CD_12_2013",
                                    "DUAL_STUS_CD_01_2014",
                                    "DUAL_STUS_CD_02_2014",
                                    "DUAL_STUS_CD_03_2014",
                                    "DUAL_STUS_CD_04_2014",
                                    "DUAL_STUS_CD_05_2014",
                                    "DUAL_STUS_CD_06_2014",
                                    "DUAL_STUS_CD_07_2014",
                                    "DUAL_STUS_CD_08_2014",
                                    "DUAL_STUS_CD_09_2014",
                                    "DUAL_STUS_CD_10_2014",
                                    "DUAL_STUS_CD_11_2014",
                                    "DUAL_STUS_CD_12_2014",
                                    "DUAL_STUS_CD_01_2015",
                                    "DUAL_STUS_CD_02_2015",
                                    "DUAL_STUS_CD_03_2015",
                                    "DUAL_STUS_CD_04_2015",
                                    "DUAL_STUS_CD_05_2015",
                                    "DUAL_STUS_CD_06_2015",
                                    "DUAL_STUS_CD_07_2015",
                                    "DUAL_STUS_CD_08_2015",
                                    "DUAL_STUS_CD_09_2015",
                                    "DUAL_STUS_CD_10_2015",
                                    "DUAL_STUS_CD_11_2015",
                                    "DUAL_STUS_CD_12_2015",
                                    "DUAL_STUS_CD_01_2016",
                                    "DUAL_STUS_CD_02_2016",
                                    "DUAL_STUS_CD_03_2016",
                                    "DUAL_STUS_CD_04_2016",
                                    "DUAL_STUS_CD_05_2016",
                                    "DUAL_STUS_CD_06_2016",
                                    "DUAL_STUS_CD_07_2016",
                                    "DUAL_STUS_CD_08_2016",
                                    "DUAL_STUS_CD_09_2016",
                                    "DUAL_STUS_CD_10_2016",
                                    "DUAL_STUS_CD_11_2016",
                                    "DUAL_STUS_CD_12_2016",
                                    "DUAL_STUS_CD_01_2017",
                                    "DUAL_STUS_CD_02_2017",
                                    "DUAL_STUS_CD_03_2017",
                                    "DUAL_STUS_CD_04_2017",
                                    "DUAL_STUS_CD_05_2017",
                                    "DUAL_STUS_CD_06_2017",
                                    "DUAL_STUS_CD_07_2017",
                                    "DUAL_STUS_CD_08_2017",
                                    "DUAL_STUS_CD_09_2017",
                                    "DUAL_STUS_CD_10_2017",
                                    "DUAL_STUS_CD_11_2017",
                                    "DUAL_STUS_CD_12_2017",
                                    "HMO_IND_01_2013",
                                    "HMO_IND_02_2013",
                                    "HMO_IND_03_2013",
                                    "HMO_IND_04_2013",
                                    "HMO_IND_05_2013",
                                    "HMO_IND_06_2013",
                                    "HMO_IND_07_2013",
                                    "HMO_IND_08_2013",
                                    "HMO_IND_09_2013",
                                    "HMO_IND_10_2013",
                                    "HMO_IND_11_2013",
                                    "HMO_IND_12_2013",
                                    "HMO_IND_01_2014",
                                    "HMO_IND_02_2014",
                                    "HMO_IND_03_2014",
                                    "HMO_IND_04_2014",
                                    "HMO_IND_05_2014",
                                    "HMO_IND_06_2014",
                                    "HMO_IND_07_2014",
                                    "HMO_IND_08_2014",
                                    "HMO_IND_09_2014",
                                    "HMO_IND_10_2014",
                                    "HMO_IND_11_2014",
                                    "HMO_IND_12_2014",
                                    "HMO_IND_01_2015",
                                    "HMO_IND_02_2015",
                                    "HMO_IND_03_2015",
                                    "HMO_IND_04_2015",
                                    "HMO_IND_05_2015",
                                    "HMO_IND_06_2015",
                                    "HMO_IND_07_2015",
                                    "HMO_IND_08_2015",
                                    "HMO_IND_09_2015",
                                    "HMO_IND_10_2015",
                                    "HMO_IND_11_2015",
                                    "HMO_IND_12_2015",
                                    "HMO_IND_01_2016",
                                    "HMO_IND_02_2016",
                                    "HMO_IND_03_2016",
                                    "HMO_IND_04_2016",
                                    "HMO_IND_05_2016",
                                    "HMO_IND_06_2016",
                                    "HMO_IND_07_2016",
                                    "HMO_IND_08_2016",
                                    "HMO_IND_09_2016",
                                    "HMO_IND_10_2016",
                                    "HMO_IND_11_2016",
                                    "HMO_IND_12_2016",
                                    "HMO_IND_01_2017",
                                    "HMO_IND_02_2017",
                                    "HMO_IND_03_2017",
                                    "HMO_IND_04_2017",
                                    "HMO_IND_05_2017",
                                    "HMO_IND_06_2017",
                                    "HMO_IND_07_2017",
                                    "HMO_IND_08_2017",
                                    "HMO_IND_09_2017",
                                    "HMO_IND_10_2017",
                                    "HMO_IND_11_2017",
                                    "HMO_IND_12_2017"
                                    )]

colnames(ODs_2014thru2016)




## ----All deaths by month-year-----------------------------------------------------------------------------------

# 2014 deaths ####

Jan14 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Jan 2014") %>% 
  mutate(across(c(18:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(78:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Feb14 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Feb 2014") %>% 
  mutate(across(c(19:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(79:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
Mar14 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Mar 2014") %>% 
  mutate(across(c(20:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(80:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
Apr14 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Apr 2014") %>% 
  mutate(across(c(21:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(81:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
May14 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "May 2014") %>% 
  mutate(across(c(22:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(82:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Jun14 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Jun 2014") %>% 
  mutate(across(c(23:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(83:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Jul14 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Jul 2014") %>% 
  mutate(across(c(24:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(84:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Aug14 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Aug 2014") %>% 
  mutate(across(c(25:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(85:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Sep14 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Sep 2014") %>% 
  mutate(across(c(26:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(86:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Oct14 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Oct 2014") %>% 
  mutate(across(c(27:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(87:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Nov14 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Nov 2014") %>% 
  mutate(across(c(28:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(88:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Dec14 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Dec 2014") %>% 
  mutate(across(c(29:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(89:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))

deaths2014_amongOD <- rbind(Jan14,Feb14,Mar14,Apr14,May14,Jun14,Jul14,Aug14,Sep14,Oct14,Nov14,Dec14)
  

# 2015 deaths ####

Jan15 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Jan 2015") %>% 
  mutate(across(c(30:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(90:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
Feb15 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Feb 2015") %>% 
  mutate(across(c(31:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(91:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Mar15 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Mar 2015") %>% 
  mutate(across(c(32:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(92:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Apr15 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Apr 2015") %>% 
  mutate(across(c(33:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(93:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
May15 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "May 2015") %>% 
  mutate(across(c(34:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(94:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Jun15 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Jun 2015") %>% 
  mutate(across(c(35:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(95:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Jul15 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Jul 2015") %>% 
  mutate(across(c(36:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(96:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
Aug15 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Aug 2015") %>% 
  mutate(across(c(37:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(97:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
Sep15 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Sep 2015") %>% 
  mutate(across(c(38:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(98:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Oct15 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Oct 2015") %>% 
  mutate(across(c(39:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(99:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Nov15 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Nov 2015") %>% 
  mutate(across(c(40:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(100:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Dec15 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Dec 2015") %>% 
  mutate(across(c(41:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(101:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))

deaths2015_amongOD <- rbind(Jan15,Feb15,Mar15,Apr15,May15,Jun15,Jul15,Aug15,Sep15,Oct15,Nov15,Dec15)

# 2016 deaths ####

Jan16 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Jan 2016") %>% 
  mutate(across(c(42:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(102:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
Feb16 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Feb 2016") %>% 
  mutate(across(c(43:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(103:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Mar16 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Mar 2016") %>% 
  mutate(across(c(44:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(104:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Apr16 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Apr 2016") %>% 
  mutate(across(c(45:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(105:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
May16 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "May 2016") %>% 
  mutate(across(c(46:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(106:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
Jun16 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Jun 2016") %>% 
  mutate(across(c(47:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(107:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Jul16 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Jul 2016") %>% 
  mutate(across(c(48:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(108:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Aug16 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Aug 2016") %>% 
  mutate(across(c(49:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(109:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
Sep16 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Sep 2016") %>% 
  mutate(across(c(50:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(110:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
Oct16 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Oct 2016") %>% 
  mutate(across(c(51:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(111:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Nov16 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Nov 2016") %>% 
  mutate(across(c(52:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(112:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Dec16 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Dec 2016") %>% 
  mutate(across(c(53:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(113:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))

deaths2016_amongOD <- rbind(Jan16,Feb16,Mar16,Apr16,May16,Jun16,Jul16,Aug16,Sep16,Oct16,Nov16,Dec16)

# 2017 deaths ####

Jan17 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Jan 2017") %>% 
  mutate(across(c(54:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(114:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
Feb17 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Feb 2017") %>% 
  mutate(across(c(55:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(115:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
Mar17 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Mar 2017") %>% 
  mutate(across(c(56:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(116:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Apr17 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Apr 2017") %>% 
  mutate(across(c(57:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(117:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
May17 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "May 2017") %>% 
  mutate(across(c(58:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(118:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
Jun17 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Jun 2017") %>% 
  mutate(across(c(59:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(119:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
Jul17 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Jul 2017") %>% 
  mutate(across(c(60:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(120:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
Aug17 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Aug 2017") %>% 
  mutate(across(c(61:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(121:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 
Sep17 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Sep 2017") %>% 
  mutate(across(c(62:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(122:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Oct17 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Oct 2017") %>% 
  mutate(across(c(63:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(123:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Nov17 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Nov 2017") %>% 
  mutate(across(c(64:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(124:125), ~ ifelse(.x ==99 | is.na(.x),0,0)))
Dec17 <- ODs_2014thru2016 %>% filter(BENE_DEATH_DT == "Dec 2017") %>% 
  mutate(across(c(65:65), ~ ifelse(.x ==1 | .x ==0 | is.na(.x),1,1))) %>%
  mutate(across(c(125:125), ~ ifelse(.x ==99 | is.na(.x),0,0))) 

deaths2017_amongOD <- rbind(Jan17,Feb17,Mar17,Apr17,May17,Jun17,Jul17,Aug17,Sep17,Oct17,Nov17,Dec17)



## ----Add death data to ODs_2014thru2016-------------------------------------------------------------------------

# All ODdts & outcomes ####

paste0("The number of benes in the new dataframe (",n_distinct(ODs_2014thru2016$BENE_ID),") should be the same as the benes in the original df (", n_distinct(allODERdts_allbenes_complete$BENE_ID),").")

ODs_2014thru2016_died <- rbind(deaths2014_amongOD,deaths2015_amongOD,deaths2016_amongOD,deaths2017_amongOD)

paste0(n_distinct(ODs_2014thru2016_died$BENE_ID), " benes in this cohort died by the end of 2017")

ODs_2014thru2016_survived <- subset(ODs_2014thru2016, is.na(BENE_DEATH_DT))

paste0(n_distinct(ODs_2014thru2016_survived$BENE_ID), " benes in this cohort survived at least until the end of 2017")

ODs_2014thru2016 <- rbind(ODs_2014thru2016_died,ODs_2014thru2016_survived)

# Reorganize df

ODs_2014thru2016 <- ODs_2014thru2016[,c("BENE_ID","YEAR","CLM_ID","INDEX_DT",
                                    "DUAL_STUS_CD_01_2013","HMO_IND_01_2013",
                                    "DUAL_STUS_CD_02_2013","HMO_IND_02_2013",
                                    "DUAL_STUS_CD_03_2013","HMO_IND_03_2013",
                                    "DUAL_STUS_CD_04_2013","HMO_IND_04_2013",
                                    "DUAL_STUS_CD_05_2013","HMO_IND_05_2013",
                                    "DUAL_STUS_CD_06_2013","HMO_IND_06_2013",
                                    "DUAL_STUS_CD_07_2013","HMO_IND_07_2013",
                                    "DUAL_STUS_CD_08_2013","HMO_IND_08_2013",
                                    "DUAL_STUS_CD_09_2013","HMO_IND_09_2013",
                                    "DUAL_STUS_CD_10_2013","HMO_IND_10_2013",
                                    "DUAL_STUS_CD_11_2013","HMO_IND_11_2013",
                                    "DUAL_STUS_CD_12_2013","HMO_IND_12_2013",
                                    "DUAL_STUS_CD_01_2014","HMO_IND_01_2014",
                                    "DUAL_STUS_CD_02_2014","HMO_IND_02_2014",
                                    "DUAL_STUS_CD_03_2014","HMO_IND_03_2014",
                                    "DUAL_STUS_CD_04_2014","HMO_IND_04_2014",
                                    "DUAL_STUS_CD_05_2014","HMO_IND_05_2014",
                                    "DUAL_STUS_CD_06_2014","HMO_IND_06_2014",
                                    "DUAL_STUS_CD_07_2014","HMO_IND_07_2014",
                                    "DUAL_STUS_CD_08_2014","HMO_IND_08_2014",
                                    "DUAL_STUS_CD_09_2014","HMO_IND_09_2014",
                                    "DUAL_STUS_CD_10_2014","HMO_IND_10_2014",
                                    "DUAL_STUS_CD_11_2014","HMO_IND_11_2014",
                                    "DUAL_STUS_CD_12_2014","HMO_IND_12_2014",
                                    "DUAL_STUS_CD_01_2015","HMO_IND_01_2015",
                                    "DUAL_STUS_CD_02_2015","HMO_IND_02_2015",
                                    "DUAL_STUS_CD_03_2015","HMO_IND_03_2015",
                                    "DUAL_STUS_CD_04_2015","HMO_IND_04_2015",
                                    "DUAL_STUS_CD_05_2015","HMO_IND_05_2015",
                                    "DUAL_STUS_CD_06_2015","HMO_IND_06_2015",
                                    "DUAL_STUS_CD_07_2015","HMO_IND_07_2015",
                                    "DUAL_STUS_CD_08_2015","HMO_IND_08_2015",
                                    "DUAL_STUS_CD_09_2015","HMO_IND_09_2015",
                                    "DUAL_STUS_CD_10_2015","HMO_IND_10_2015",
                                    "DUAL_STUS_CD_11_2015","HMO_IND_11_2015",
                                    "DUAL_STUS_CD_12_2015","HMO_IND_12_2015",
                                    "DUAL_STUS_CD_01_2016","HMO_IND_01_2016",
                                    "DUAL_STUS_CD_02_2016","HMO_IND_02_2016",
                                    "DUAL_STUS_CD_03_2016","HMO_IND_03_2016",
                                    "DUAL_STUS_CD_04_2016","HMO_IND_04_2016",
                                    "DUAL_STUS_CD_05_2016","HMO_IND_05_2016",
                                    "DUAL_STUS_CD_06_2016","HMO_IND_06_2016",
                                    "DUAL_STUS_CD_07_2016","HMO_IND_07_2016",
                                    "DUAL_STUS_CD_08_2016","HMO_IND_08_2016",
                                    "DUAL_STUS_CD_09_2016","HMO_IND_09_2016",
                                    "DUAL_STUS_CD_10_2016","HMO_IND_10_2016",
                                    "DUAL_STUS_CD_11_2016","HMO_IND_11_2016",
                                    "DUAL_STUS_CD_12_2016","HMO_IND_12_2016",
                                    "DUAL_STUS_CD_01_2017","HMO_IND_01_2017",
                                    "DUAL_STUS_CD_02_2017","HMO_IND_02_2017",
                                    "DUAL_STUS_CD_03_2017","HMO_IND_03_2017",
                                    "DUAL_STUS_CD_04_2017","HMO_IND_04_2017",
                                    "DUAL_STUS_CD_05_2017","HMO_IND_05_2017",
                                    "DUAL_STUS_CD_06_2017","HMO_IND_06_2017",
                                    "DUAL_STUS_CD_07_2017","HMO_IND_07_2017",
                                    "DUAL_STUS_CD_08_2017","HMO_IND_08_2017",
                                    "DUAL_STUS_CD_09_2017","HMO_IND_09_2017",
                                    "DUAL_STUS_CD_10_2017","HMO_IND_10_2017",
                                    "DUAL_STUS_CD_11_2017","HMO_IND_11_2017",
                                    "DUAL_STUS_CD_12_2017","HMO_IND_12_2017",
                                    "BENE_DEATH_DT")]



## ----Create eligibility indicator var by month-year-------------------------------------------------------------

# First date of coverage needed

ODs_2014thru2016$FIRST_DT <- ODs_2014thru2016$INDEX_DT - 180
ODs_2014thru2016 <- ODs_2014thru2016 %>% relocate(FIRST_DT, .before = INDEX_DT)

# End date - 12 months or until death - NOTE: Last date is 1 month prior to actual date because benes can have any status in the month of death. Benes sometimes coded 0, 99, or NA for dual status for the month they died so any of those codes are fine in addition to full dual codes for their death month. Spot checked and never found 1, 3, 6, or 9 (ineligible codes) for death month after all previous months were full duals.  (see above chunk of coding)

ODs_2014thru2016$LAST_DT =  ODs_2014thru2016$INDEX_DT + 366
ODs_2014thru2016 <- ODs_2014thru2016 %>% relocate(LAST_DT, .after = INDEX_DT)

# Transform dates to month-year (easier to read and spot check)

ODs_2014thru2016 <- transform(ODs_2014thru2016, FIRST_DT = as.yearmon(FIRST_DT))
ODs_2014thru2016 <- transform(ODs_2014thru2016, LAST_DT = as.yearmon(LAST_DT))
ODs_2014thru2016 <- transform(ODs_2014thru2016, INDEX_DT = as.yearmon(INDEX_DT))

# Combine dual status columns & HMO columns to one 'eligible' indicator for each month. HMO==99 if "TRUE" for the month. I only am interested in those that == 1 (dual status = yes and HMO status = no)

# IMPORTANT NOTE: Benes that die are indicated as full dual from the month of death until the end of the 12-month period regardless of prior dual status. (see above chunk)

colnames(ODs_2014thru2016)

ODs_2014thru2016$ELIGIBLE_Jan2013 <- rowSums(ODs_2014thru2016[ , c(7,8)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Feb2013 <- rowSums(ODs_2014thru2016[ , c(9,10)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Mar2013 <- rowSums(ODs_2014thru2016[ , c(11,12)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Apr2013 <- rowSums(ODs_2014thru2016[ , c(13,14)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_May2013 <- rowSums(ODs_2014thru2016[ , c(15,16)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Jun2013 <- rowSums(ODs_2014thru2016[ , c(17,18)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Jul2013 <- rowSums(ODs_2014thru2016[ , c(19,20)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Aug2013 <- rowSums(ODs_2014thru2016[ , c(21,22)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Sep2013 <- rowSums(ODs_2014thru2016[ , c(23,24)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Oct2013 <- rowSums(ODs_2014thru2016[ , c(25,26)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Nov2013 <- rowSums(ODs_2014thru2016[ , c(27,28)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Dec2013 <- rowSums(ODs_2014thru2016[ , c(29,30)], na.rm=TRUE)

ODs_2014thru2016$ELIGIBLE_Jan2014 <- rowSums(ODs_2014thru2016[ , c(31,32)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Feb2014 <- rowSums(ODs_2014thru2016[ , c(33,34)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Mar2014 <- rowSums(ODs_2014thru2016[ , c(35,36)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Apr2014 <- rowSums(ODs_2014thru2016[ , c(37,38)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_May2014 <- rowSums(ODs_2014thru2016[ , c(39,40)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Jun2014 <- rowSums(ODs_2014thru2016[ , c(41,42)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Jul2014 <- rowSums(ODs_2014thru2016[ , c(43,44)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Aug2014 <- rowSums(ODs_2014thru2016[ , c(45,46)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Sep2014 <- rowSums(ODs_2014thru2016[ , c(47,48)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Oct2014 <- rowSums(ODs_2014thru2016[ , c(49,50)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Nov2014 <- rowSums(ODs_2014thru2016[ , c(51,52)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Dec2014 <- rowSums(ODs_2014thru2016[ , c(53,54)], na.rm=TRUE)

ODs_2014thru2016$ELIGIBLE_Jan2015 <- rowSums(ODs_2014thru2016[ , c(55,56)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Feb2015 <- rowSums(ODs_2014thru2016[ , c(57,58)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Mar2015 <- rowSums(ODs_2014thru2016[ , c(59,60)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Apr2015 <- rowSums(ODs_2014thru2016[ , c(61,62)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_May2015 <- rowSums(ODs_2014thru2016[ , c(63,64)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Jun2015 <- rowSums(ODs_2014thru2016[ , c(65,66)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Jul2015 <- rowSums(ODs_2014thru2016[ , c(67,68)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Aug2015 <- rowSums(ODs_2014thru2016[ , c(69,70)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Sep2015 <- rowSums(ODs_2014thru2016[ , c(71,72)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Oct2015 <- rowSums(ODs_2014thru2016[ , c(73,74)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Nov2015 <- rowSums(ODs_2014thru2016[ , c(75,76)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Dec2015 <- rowSums(ODs_2014thru2016[ , c(77,78)], na.rm=TRUE)

ODs_2014thru2016$ELIGIBLE_Jan2016 <- rowSums(ODs_2014thru2016[ , c(79,80)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Feb2016 <- rowSums(ODs_2014thru2016[ , c(81,82)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Mar2016 <- rowSums(ODs_2014thru2016[ , c(83,84)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Apr2016 <- rowSums(ODs_2014thru2016[ , c(85,86)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_May2016 <- rowSums(ODs_2014thru2016[ , c(87,88)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Jun2016 <- rowSums(ODs_2014thru2016[ , c(89,90)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Jul2016 <- rowSums(ODs_2014thru2016[ , c(91,92)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Aug2016 <- rowSums(ODs_2014thru2016[ , c(93,94)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Sep2016 <- rowSums(ODs_2014thru2016[ , c(95,96)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Oct2016 <- rowSums(ODs_2014thru2016[ , c(97,98)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Nov2016 <- rowSums(ODs_2014thru2016[ , c(99,100)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Dec2016 <- rowSums(ODs_2014thru2016[ , c(101,102)], na.rm=TRUE)

ODs_2014thru2016$ELIGIBLE_Jan2017 <- rowSums(ODs_2014thru2016[ , c(103,104)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Feb2017 <- rowSums(ODs_2014thru2016[ , c(105,106)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Mar2017 <- rowSums(ODs_2014thru2016[ , c(107,108)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Apr2017 <- rowSums(ODs_2014thru2016[ , c(109,110)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_May2017 <- rowSums(ODs_2014thru2016[ , c(111,112)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Jun2017 <- rowSums(ODs_2014thru2016[ , c(113,114)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Jul2017 <- rowSums(ODs_2014thru2016[ , c(115,116)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Aug2017 <- rowSums(ODs_2014thru2016[ , c(117,118)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Sep2017 <- rowSums(ODs_2014thru2016[ , c(119,120)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Oct2017 <- rowSums(ODs_2014thru2016[ , c(121,122)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Nov2017 <- rowSums(ODs_2014thru2016[ , c(123,124)], na.rm=TRUE)
ODs_2014thru2016$ELIGIBLE_Dec2017 <- rowSums(ODs_2014thru2016[ , c(125,126)], na.rm=TRUE)

# colnames(ODs_2014thru2016)

ODs_2014thru2016 <- ODs_2014thru2016 %>% relocate(BENE_DEATH_DT, .after = LAST_DT)

ODs_2014thru2016_abbrev <- ODs_2014thru2016[,c(1:7,128:187)]

colnames(ODs_2014thru2016_abbrev)

n_distinct(ODs_2014thru2016_abbrev$BENE_ID) # Check to make sure no benes were cut




## ----Determine eligibility by 2014 month-year-------------------------------------------------------------------

Jan2014ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Jan 2014") %>%
    mutate(Total = select(., ELIGIBLE_Jul2013:ELIGIBLE_Jan2015) %>% 
             rowSums(na.rm = TRUE))

Feb2014ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Feb 2014") %>%
    mutate(Total = select(., ELIGIBLE_Aug2013:ELIGIBLE_Feb2015) %>% 
             rowSums(na.rm = TRUE))

Mar2014ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Mar 2014") %>%
    mutate(Total = select(., ELIGIBLE_Sep2013:ELIGIBLE_Mar2015) %>% 
             rowSums(na.rm = TRUE))

Apr2014ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Apr 2014") %>%
    mutate(Total = select(., ELIGIBLE_Oct2013:ELIGIBLE_Apr2015) %>% 
             rowSums(na.rm = TRUE))

May2014ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "May 2014") %>%
    mutate(Total = select(., ELIGIBLE_Nov2013:ELIGIBLE_May2015) %>% 
             rowSums(na.rm = TRUE))

Jun2014ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Jun 2014") %>%
    mutate(Total = select(., ELIGIBLE_Dec2013:ELIGIBLE_Jun2015) %>% 
             rowSums(na.rm = TRUE))

Jul2014ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Jul 2014") %>%
    mutate(Total = select(., ELIGIBLE_Jan2014:ELIGIBLE_Jul2015) %>% 
             rowSums(na.rm = TRUE))

Aug2014ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Aug 2014") %>%
    mutate(Total = select(., ELIGIBLE_Feb2014:ELIGIBLE_Aug2015) %>% 
             rowSums(na.rm = TRUE))

Sep2014ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Sep 2014") %>%
    mutate(Total = select(., ELIGIBLE_Mar2014:ELIGIBLE_Sep2015) %>% 
             rowSums(na.rm = TRUE))

Oct2014ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Oct 2014") %>%
    mutate(Total = select(., ELIGIBLE_Apr2014:ELIGIBLE_Oct2015) %>% 
             rowSums(na.rm = TRUE))

Nov2014ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Nov 2014") %>%
    mutate(Total = select(., ELIGIBLE_May2014:ELIGIBLE_Nov2015) %>% 
             rowSums(na.rm = TRUE))

Dec2014ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Dec 2014") %>%
    mutate(Total = select(., ELIGIBLE_Jun2014:ELIGIBLE_Dec2015) %>% 
             rowSums(na.rm = TRUE))

ODs_eligible2014 <- rbind(Jan2014ODs_eligible,Feb2014ODs_eligible,Mar2014ODs_eligible,Apr2014ODs_eligible,May2014ODs_eligible,Jun2014ODs_eligible,Jul2014ODs_eligible,Aug2014ODs_eligible,Sep2014ODs_eligible,Oct2014ODs_eligible,Nov2014ODs_eligible,Dec2014ODs_eligible)





## ----Determine eligibility by 2015 month-year-------------------------------------------------------------------

Jan2015ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Jan 2015") %>%
    mutate(Total = select(., ELIGIBLE_Jul2014:ELIGIBLE_Jan2016) %>% 
             rowSums(na.rm = TRUE))
Feb2015ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Feb 2015") %>%
    mutate(Total = select(., ELIGIBLE_Aug2014:ELIGIBLE_Feb2016) %>% 
             rowSums(na.rm = TRUE))
Mar2015ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Mar 2015") %>%
    mutate(Total = select(., ELIGIBLE_Sep2014:ELIGIBLE_Mar2016) %>% 
             rowSums(na.rm = TRUE))
Apr2015ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Apr 2015") %>%
    mutate(Total = select(., ELIGIBLE_Oct2014:ELIGIBLE_Apr2016) %>% 
             rowSums(na.rm = TRUE))
May2015ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "May 2015") %>%
    mutate(Total = select(., ELIGIBLE_Nov2014:ELIGIBLE_May2016) %>% 
             rowSums(na.rm = TRUE))
Jun2015ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Jun 2015") %>%
    mutate(Total = select(., ELIGIBLE_Dec2014:ELIGIBLE_Jun2016) %>% 
             rowSums(na.rm = TRUE))
Jul2015ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Jul 2015") %>%
    mutate(Total = select(., ELIGIBLE_Jan2015:ELIGIBLE_Jul2016) %>% 
             rowSums(na.rm = TRUE))
Aug2015ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Aug 2015") %>%
    mutate(Total = select(., ELIGIBLE_Feb2015:ELIGIBLE_Aug2016) %>% 
             rowSums(na.rm = TRUE))
Sep2015ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Sep 2015") %>%
    mutate(Total = select(., ELIGIBLE_Mar2015:ELIGIBLE_Sep2016) %>% 
             rowSums(na.rm = TRUE))
Oct2015ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Oct 2015") %>%
    mutate(Total = select(., ELIGIBLE_Apr2015:ELIGIBLE_Oct2016) %>% 
             rowSums(na.rm = TRUE))
Nov2015ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Nov 2015") %>%
    mutate(Total = select(., ELIGIBLE_May2015:ELIGIBLE_Nov2016) %>% 
             rowSums(na.rm = TRUE))
Dec2015ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Dec 2015") %>%
    mutate(Total = select(., ELIGIBLE_Jun2015:ELIGIBLE_Dec2016) %>% 
             rowSums(na.rm = TRUE))

ODs_eligible2015 <- rbind(Jan2015ODs_eligible,Feb2015ODs_eligible,Mar2015ODs_eligible,Apr2015ODs_eligible,May2015ODs_eligible,Jun2015ODs_eligible,Jul2015ODs_eligible,Aug2015ODs_eligible,Sep2015ODs_eligible,Oct2015ODs_eligible,Nov2015ODs_eligible,Dec2015ODs_eligible)



## ----Determine eligibility by 2016 month-year-------------------------------------------------------------------
Jan2016ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Jan 2016") %>%
    mutate(Total = select(., ELIGIBLE_Jul2015:ELIGIBLE_Jan2017) %>% 
             rowSums(na.rm = TRUE))
Feb2016ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Feb 2016") %>%
    mutate(Total = select(., ELIGIBLE_Aug2015:ELIGIBLE_Feb2017) %>% 
             rowSums(na.rm = TRUE))
Mar2016ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Mar 2016") %>%
    mutate(Total = select(., ELIGIBLE_Sep2015:ELIGIBLE_Mar2017) %>% 
             rowSums(na.rm = TRUE))
Apr2016ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Apr 2016") %>%
    mutate(Total = select(., ELIGIBLE_Oct2015:ELIGIBLE_Apr2017) %>% 
             rowSums(na.rm = TRUE))
May2016ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "May 2016") %>%
    mutate(Total = select(., ELIGIBLE_Nov2015:ELIGIBLE_May2017) %>% 
             rowSums(na.rm = TRUE))
Jun2016ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Jun 2016") %>%
    mutate(Total = select(., ELIGIBLE_Dec2015:ELIGIBLE_Jun2017) %>% 
             rowSums(na.rm = TRUE))
Jul2016ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Jul 2016") %>%
    mutate(Total = select(., ELIGIBLE_Jan2016:ELIGIBLE_Jul2017) %>% 
             rowSums(na.rm = TRUE))
Aug2016ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Aug 2016") %>%
    mutate(Total = select(., ELIGIBLE_Feb2016:ELIGIBLE_Aug2017) %>% 
             rowSums(na.rm = TRUE))
Sep2016ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Sep 2016") %>%
    mutate(Total = select(., ELIGIBLE_Mar2016:ELIGIBLE_Sep2017) %>% 
             rowSums(na.rm = TRUE))
Oct2016ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Oct 2016") %>%
    mutate(Total = select(., ELIGIBLE_Apr2016:ELIGIBLE_Oct2017) %>% 
             rowSums(na.rm = TRUE))
Nov2016ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Nov 2016") %>%
    mutate(Total = select(., ELIGIBLE_May2016:ELIGIBLE_Nov2017) %>% 
             rowSums(na.rm = TRUE))
Dec2016ODs_eligible <- ODs_2014thru2016_abbrev %>%
  filter(INDEX_DT == "Dec 2016") %>%
    mutate(Total = select(., ELIGIBLE_Jun2016:ELIGIBLE_Dec2017) %>% 
             rowSums(na.rm = TRUE))

ODs_eligible2016 <- rbind(Jan2016ODs_eligible,Feb2016ODs_eligible,Mar2016ODs_eligible,Apr2016ODs_eligible,May2016ODs_eligible,Jun2016ODs_eligible,Jul2016ODs_eligible,Aug2016ODs_eligible,Sep2016ODs_eligible,Oct2016ODs_eligible,Nov2016ODs_eligible,Dec2016ODs_eligible)



## ----One df for eligibility by all years------------------------------------------------------------------------

ODs_2014thru2016_abbrev <- rbind(ODs_eligible2014,ODs_eligible2015,ODs_eligible2016)

n_distinct(ODs_2014thru2016_abbrev$BENE_ID)

table(ODs_2014thru2016_abbrev$Total)




## ----Final df of all eligible ODs-------------------------------------------------------------------------------

# Benes whose total = 19 are eligible for this study. They had 6 months coverage prior to OD, were eligible during the OD month, and had 12 months coverage after the event. Those who died before 12 months were designated as fully eligible for the months after their deaths.

ODs_2014thru2016_eligible <- subset(ODs_2014thru2016_abbrev, Total == 19)

# visually inspect/spot check whether criteria were fulfilled in full dual status + hmo df using original variables (DUAL_STUS_CD_01_2013, etc.)

paste0("A total of ",n_distinct(ODs_2014thru2016_eligible$BENE_ID)," beneficiaries who fulfilled the full dual/no HMO eligibility requirement had ",n_distinct(ODs_2014thru2016_eligible$CLM_ID)," eligible claims  in an inpatient or outpatient ER between 2014-2016.")



## ----Divide eligible ODs by year--------------------------------------------------------------------------------

# First divide eligible claims for nedual ODs into the year of OD 

ODs2014_neduals <- subset(ODs_2014thru2016_eligible, INDEX_DT >= "Jan 2014" & INDEX_DT < "Jan 2015") 
n_distinct(ODs2014_neduals$BENE_ID) # 9107
ODs2015_neduals <- subset(ODs_2014thru2016_eligible, INDEX_DT >= "Jan 2015" & INDEX_DT < "Jan 2016")
n_distinct(ODs2015_neduals$BENE_ID) # 10375
ODs2016_neduals <- subset(ODs_2014thru2016_eligible, INDEX_DT >= "Jan 2016" & INDEX_DT < "Jan 2017")
n_distinct(ODs2016_neduals$BENE_ID) # 14204



## ----ESRD exclusion---------------------------------------------------------------------------------------------

esrd2014 <- mbsf2014_full %>% filter(BENE_ID %in% ODs2014_neduals$BENE_ID & ESRD_IND == 0)
esrd2015 <- mbsf2015_full %>% filter(BENE_ID %in% ODs2015_neduals$BENE_ID & ESRD_IND == 0)
esrd2016 <- mbsf2016_full %>% filter(BENE_ID %in% ODs2016_neduals$BENE_ID & ESRD_IND == 0)

mbsf_esrd <- unique(c(esrd2014$BENE_ID,esrd2015$BENE_ID,esrd2016$BENE_ID))

paste0("A total of ",n_distinct(unique(mbsf_esrd))," beneficiaries who fulfilled the dual/HMO criteria did not have ESRD at the time of the OD.")

paste0(n_distinct(ODs_2014thru2016_eligible$BENE_ID)-n_distinct(unique(mbsf_esrd))," beneficiaries were eliminated because they had ESRD at the time of the OD.")



## ----Cancer dx exclusion----------------------------------------------------------------------------------------

mbsf2014_cancer <- esrd2014 %>% filter(CANCER_BREAST != 3 & CANCER_LUNG != 3 & CANCER_COLORECTAL != 3 & CANCER_PROSTATE != 3 & CANCER_ENDOMETRIAL != 3 & LEUKLYMPH_MEDICARE != 3 & CANCER_BREAST != 1 & CANCER_LUNG != 1 & CANCER_COLORECTAL != 1 & CANCER_PROSTATE != 1 & CANCER_ENDOMETRIAL != 1 & LEUKLYMPH_MEDICARE != 1)

mbsf2015_cancer <- esrd2015 %>% filter(CANCER_BREAST != 3 & CANCER_LUNG != 3 & CANCER_COLORECTAL != 3 & CANCER_PROSTATE != 3 & CANCER_ENDOMETRIAL != 3 & LEUKLYMPH_MEDICARE != 3 & CANCER_BREAST != 1 & CANCER_LUNG != 1 & CANCER_COLORECTAL != 1 & CANCER_PROSTATE != 1 & CANCER_ENDOMETRIAL != 1 & LEUKLYMPH_MEDICARE != 1)

mbsf2016_cancer <- esrd2016 %>% filter(CANCER_BREAST != 3 & CANCER_LUNG != 3 & CANCER_COLORECTAL != 3 & CANCER_PROSTATE != 3 & CANCER_ENDOMETRIAL != 3 & LEUKLYMPH_MEDICARE != 3 & CANCER_BREAST != 1 & CANCER_LUNG != 1 & CANCER_COLORECTAL != 1 & CANCER_PROSTATE != 1 & CANCER_ENDOMETRIAL != 1 & LEUKLYMPH_MEDICARE != 1)

mbsf_cancer <- unique(c(mbsf2014_cancer$BENE_ID,mbsf2015_cancer$BENE_ID,mbsf2016_cancer$BENE_ID))  

paste0("A total of ",n_distinct(unique(mbsf_cancer))," beneficiaries who fulfilled the dual/HMO criteria/no ESRD also did not have one of the cancer dx in MBSF at the time of the OD.")

paste0(n_distinct(unique(mbsf_esrd))-n_distinct(unique(mbsf_cancer))," beneficiaries were eliminated because they had cancer at the time of the OD.")



## ----Entitlement exclusion--------------------------------------------------------------------------------------

entitlement2014 <- mbsf2014_cancer %>% filter(ENTLMT_RSN_ORIG == 1)
entitlement2015 <- mbsf2015_cancer %>% filter(ENTLMT_RSN_ORIG == 1)
entitlement2016 <- mbsf2016_cancer %>% filter(ENTLMT_RSN_ORIG == 1)

mbsf_entitlement <- unique(c(entitlement2014$BENE_ID,entitlement2015$BENE_ID,entitlement2016$BENE_ID)) 

paste0("A total of ",n_distinct(unique(mbsf_entitlement))," beneficiaries who fulfilled the dual/HMO criteria/no cancer in MBSF also were eligible for benefits due to a disability.")

paste0(n_distinct(unique(mbsf_cancer))-n_distinct(unique(mbsf_entitlement))," beneficiaries were eliminated because they were not eligible for benefits based on a disability.")



## ----Location exclusion-----------------------------------------------------------------------------------------

# Location - 50 states and DC 

mbsf2014_location <- entitlement2014 %>% filter(STATE_CODE != 40 & STATE_CODE != 48 & STATE_CODE < 54) 
n_distinct(mbsf2014_location$BENE_ID) # 7596

mbsf2015_location <- entitlement2015 %>% filter(STATE_CODE != 40 & STATE_CODE != 48 & STATE_CODE < 54)
n_distinct(mbsf2015_location$BENE_ID) # 8648

mbsf2016_location <- entitlement2016 %>% filter(STATE_CODE != 40 & STATE_CODE != 48 & STATE_CODE < 54)
n_distinct(mbsf2016_location$BENE_ID) # 11655

mbsf_location <- unique(c(mbsf2014_location$BENE_ID,mbsf2015_location$BENE_ID,mbsf2016_location$BENE_ID))  

paste0("A total of ",n_distinct(unique(mbsf_location))," beneficiaries who fulfilled the dual/HMO criteria/no cancer/eligibility criteria lived in the 50 US states or DC at the time of the OD.")

paste0(n_distinct(unique(mbsf_entitlement))-n_distinct(unique(mbsf_location))," beneficiaries were eliminated because they did not live in a US state or DC at the time of the OD.")



## ----Benes with multiple OD claims-index event------------------------------------------------------------------

#  The "mbsf-year" file contains the bene IDs of all benes who had an ODER in the study years and fulfilled the mbsf criteria. I need to identify the benes who had more than 1 claim on an index date. 

# Because I am mostly concerned about the date, it does not matter which claim is selected when there is more than one on a date. When I get into the nitty gritty (such as checking to see if multiple substances were used in an index overdose event), I will include all overdoses on the index date. 

# Filter the whole ODER dataframe that contains all ODERs from 2011-2017 for benes that satisfied the full dual criteria and fulfilled the other MBSF criteria in the year of the ODER 

allODERdts_allmbsfbenes_2014 <- allODERdts_allbenes_complete %>% filter(CLM_ID %in% ODs_2014thru2016_eligible$CLM_ID & BENE_ID %in% mbsf2014_location$BENE_ID & INDEX_DT > "2013-12-31")

allODERdts_allmbsfbenes_2015 <- allODERdts_allbenes_complete %>% filter(CLM_ID %in% ODs_2014thru2016_eligible$CLM_ID & BENE_ID %in% mbsf2015_location$BENE_ID & INDEX_DT > "2014-12-31" & INDEX_DT < "2016-01-01")

allODERdts_allmbsfbenes_2016 <- allODERdts_allbenes_complete %>% filter(CLM_ID %in% ODs_2014thru2016_eligible$CLM_ID & BENE_ID %in% mbsf2016_location$BENE_ID & INDEX_DT > "2015-12-31" & INDEX_DT < "2017-01-01")

allODERdts_allmbsfbenes <- rbind(allODERdts_allmbsfbenes_2014,allODERdts_allmbsfbenes_2015,allODERdts_allmbsfbenes_2016)

# Identify first claims on index date during which the person fulfilled the mbsf criteria as well (date from which to calculate 12-month mortality)

firstODdt_2014thru2016_all <- allODERdts_allmbsfbenes %>%
  group_by(BENE_ID) %>%
  slice(which.min(INDEX_DT)) 

firstODdt_2014thru2016 <- firstODdt_2014thru2016_all %>% 
  filter(DSCHRG_DSTNTN_CD !=20) 

paste0(n_distinct(firstODdt_2014thru2016_all$BENE_ID)," benes fulfilled the study criteria. ",n_distinct(firstODdt_2014thru2016_all$BENE_ID)-n_distinct(firstODdt_2014thru2016$BENE_ID)," died during the index overdose and were excluded, leaving ",n_distinct(firstODdt_2014thru2016$BENE_ID)," in the study cohort.")

# All OD dates for study cohort 

allODdts_studymbsfbenes <- allODdts_allbenes_complete %>% filter(BENE_ID %in% firstODdt_2014thru2016$BENE_ID)



## ----Final mbsf-------------------------------------------------------------------------------------------------

# use first OD dt to identify mbsf record to use for the beneficiary

firstODdt_2014 <- subset(firstODdt_2014thru2016, INDEX_DT > "2013-12-31" & INDEX_DT < "2015-01-01")
mbsf_firstODdt_2014 <- subset(mbsf_base2014, BENE_ID %in% firstODdt_2014$BENE_ID)

firstODdt_2015 <- subset(firstODdt_2014thru2016, INDEX_DT > "2014-12-31" & INDEX_DT < "2016-01-01")
mbsf_firstODdt_2015 <- subset(mbsf_base2015, BENE_ID %in% firstODdt_2015$BENE_ID)

firstODdt_2016 <- subset(firstODdt_2014thru2016, INDEX_DT > "2015-12-31" & INDEX_DT < "2017-01-01")
mbsf_firstODdt_2016 <- subset(mbsf_base2016, BENE_ID %in% firstODdt_2016$BENE_ID)

mbsfOD <- rbind(mbsf_firstODdt_2014,mbsf_firstODdt_2015,mbsf_firstODdt_2016)



## ----SAVE-------------------------------------------------------------------------------------------------------

saveRDS(medparOD_allODs, file = "medparOD_allODs.Rds")
saveRDS(outptOD_allODs, file = "outptOD_allODs.Rds")

saveRDS(allODdts_allbenes_complete, file = "allODdts_allbenes_complete.Rds")

saveRDS(allODdts_studymbsfbenes, file = "allODdts_studymbsfbenes.Rds")
saveRDS(firstODdt_2014thru2016, file = "firstODdt_2014thru2016.Rds")

saveRDS(mbsfOD, file = "mbsfOD.Rds")

knitr::purl("All OD data.Rmd")



## ----clean up---------------------------------------------------------------------------------------------------

# 
# CLEAN UP #################################################

# Clear environment
rm(list = ls())

# Clear packages
p_unload(all)  # Remove all add-ons

# Clear plots
# dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)


