###############################################################################
#'  Created on 21-Jan-2023
#'  ------------------------------------------------------------
#'  Copyright (c) 2023 Diabetes Epidemiology Group.
#'  All Right Reserved.
#'  ------------------------------------------------------------
#'  Author:  anwagbata
#'  Project topic: Extracting T1D patients with other autoimmune conditions.
#'  Project info: 
#'   Extract T1D patients with other autoimmune conditions (>100 cases)
#'   Using the following case ascerntainment methods
#'    1. Diagnoses self-reported on questionnaire 
#'    2. ICD-10 diagnoses recorded on hospital discharge records 
#'    3. Drugs used in autoimmune disease combined with outpatient attendance 
#'      For instance someone on sulfasalazine has either inflammatory bowel disease
#'      if attending a gastroenterology clinic or rheumatoid arthritis if 
#'      attending a rheumatology clinic. 
#'
#'
#'
#'  This is the base script for this analysis, it establishes the database 
#'  connection and gets in the relevant dataset - self reported questionnaire,
#'  medication history and specialty attendance records.
#' 
#'


setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
library(data.table)



##------------------------------------------------------------------------------
# Self-reported autoimmune conditions from patient questionnaire
##------------------------------------------------------------------------------

## Establish data base connection
con <- dbConn("T1B_2019")

## ============== Getting the questionnaire data table ==============
ques <- dbGetQueryMap(con, "SELECT serialno, otherConditions FROM o_patient_questionnaire ")

ques.long <- tidyr::separate_rows(ques, otherconditions, sep=",")
ques.long <- setDT(ques.long)
aid.long <- subset(ques.long, otherconditions %in%  c(1,10,11,12,14,15,3,4,5,7,8))
aid.long[, traitname := otherconditions]; head(aid.long)
aid.long$traitname<- dplyr::recode(aid.long$traitname,
                            "1" = "Addison's Disease", "10" = "Inflammatory bowel disease",
                            "11" = "Pernicious anemia", "12"= "Vitiligo",
                            "14" = "Lupus/SLE/Scleroderma", "15" = "Psoriasis",
                            "3" = "Multiple Sclerosis", "4" = "Rheumatoid arthritis",
                            "5" = "Celiac disease", "7" = "Myasthenia gravis",
                            "8" = "Thyroid disease")
aid.con <- as.data.table(table(aid.long$otherconditions))
aid.con <- setNames(aid.con, c("Trait","Freq"))
aid.con$Trait <- dplyr::recode(aid.con$Trait,
                            "1" = "Addison's Disease", "10" = "Inflammatory bowel disease",
                            "11" = "Pernicious anemia", "12"= "Vitiligo",
                            "14" = "Lupus/SLE/Scleroderma", "15" = "Psoriasis",
                            "3" = "Multiple Sclerosis", "4" = "Rheumatoid arthritis",
                            "5" = "Celiac disease", "7" = "Myasthenia gravis",
                            "8" = "Thyroid disease")

## ============== Getting the ICD data table ==============
icd10 <- dbGetQueryMap(con, "SELECT code, description FROM c_icd10 ")
setnames(icd10, c("diag.code", "description"))
icd9 <- dbGetQueryMap(con, "SELECT code, description FROM c_icd9 ")
setnames(icd9, c("diag.code", "description"))
icd9n10 <- rbind(icd9,icd10)

## ============== Getting the medication data table ==============
drugs <- dbGetQueryMap(con, "select serialno, encounterdate, drugname from s_prescriptions")

# ============== Speciality ==============
smr00 <- dbGetQueryMap(con, "select serialno, spec from i_smr00")
smr00.gt.long <- smr00[smr00$spec == "A9",]
smr00.dr.long <- smr00[smr00$spec == "A7",]
smr00.rh.long <- smr00[smr00$spec == "AR",]
smr00.ed.long <- smr00[smr00$spec == "A8",]
smr00.hm.long <- smr00[smr00$spec == "J4",]
