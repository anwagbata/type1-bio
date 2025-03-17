###############################################################################
#'  Created on 21-Jan-2023
#'  ------------------------------------------------------------
#'  Copyright (c) 2023 Diabetes Epidemiology Group.
#'  All Right Reserved.
#'  ------------------------------------------------------------
#'  Author: anwagbata
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
#' This is the R script for extracting Hypothyroidism (HTD) cases in the type1bio
#' cohort. The cases for HTD were defined based on their self-reported questionnaire,
#' hospital discharge ICD codes, related medication record * >1 specialty visit.
#'
#' The extraction for HTD is slightly different from other traits because the
#' self-reported term for HTD on the questionnarie is "Thyroid disease" and
#' we are only interested in HTD, therefore we only included self-reported/ICD
#' cases who are on LEVOTHYROXINE used in treatment of HTD.


setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
source("base.extract.R")


##------------------------------------------------------------------------------
## Hypothyroidism
##------------------------------------------------------------------------------

## ============== Self reported ==============
htds <- aid.con[aid.con$Trait == "Thyroid disease" ,]
htd <- aid.long[aid.long$otherconditions == 8,]

# ============== Hospital discharges ==============
htd.diag <- dbGetSMR01Diag(con, icd10.list=c("E06\\.*"), icd10.match.type="RLIKE",cohort.h=NULL)

## Remove multiple recordings of hospital discharge under the same diag.code
htd.diag.unq <- htd.diag[!duplicated(htd.diag[ , c("serialno","diag.code")]),]

## Get counts and description of ICD codes used
htd.codes <- as.data.table(table(htd.diag.unq$diag.code))
htd.codes <- setNames(htd.codes, c("diag.code","Freq"))
setkey(htd.codes,diag.code); setkey(icd9n10,diag.code)
htd.codes.des <- merge(htd.codes,icd9n10, all.x=TRUE)
htd.codes.des <- setcolorder(htd.codes.des, c("diag.code", "description", "Freq" ))

## Get unique id of patients with hospital discharge records
htd.diag.unq.id <- htd.diag.unq[!duplicated(htd.diag.unq[ , c("serialno")]),]

# ============== Medication ==============
htd.drugs <- drugs[ with(drugs, grepl("LEVOTHYROXINE*.",drugname)),]

htd.drugs$drug.new <- htd.drugs$drugname
htd.drugs$drug.new <- sub("LEVOTHYROXINE.*", "LEVOTHYROXINE", htd.drugs$drug.new)

## Unique serial no of individuals on any of these medications
htd.drugs.unq.id <- htd.drugs[!duplicated(htd.drugs[ , c("serialno")]),]

## Self-reported thyroid patients who are on HTD meds
htd.srmed <- htd[htd$serialno %in% htd.drugs$serialno]

## Hospital discharge ICD codes thyroid patients who are on HTD meds
htd.cdmed <- htd.diag.unq.id[htd.diag.unq.id$serialno %in% htd.drugs$serialno]

# ============== AR = Endocrinology outpatients ==============
smr00.ed.id <- smr00.ed.long[!duplicated(smr00.ed.long[ , c("serialno")]),]

## Getting people with more than one specialty visit
dup.ed <- smr00.ed.long[duplicated(smr00.ed.long[ , c("serialno")]),]
dup.ed.id <- dup.ed[!duplicated(dup.ed[ , c("serialno")]),]

## On meds and had more than one specialty visit
edd.drugs.unq.id <- dup.ed.id[htd.drugs.unq.id,on="serialno", nomatch=0]

# ============== Case definition ==============
setkey(htd.srmed,serialno); setkey(htd.cdmed,serialno);
setkey(edd.drugs.unq.id,serialno)

## Probable cases 1 - atd + ICD
prob.htd <- merge(htd.srmed, htd.cdmed, all=TRUE)

## Possible cases 2 - atd + ICD + med*specialty>1
htd.cases <- merge(prob.htd, edd.drugs.unq.id, all=TRUE)
dim(htd.cases)

#  ============== Venn Diagram ==============
# sr.htd <- htd[, serialno]
# icd.htd <- htd.diag.unq.id[, serialno]
# ed.htd <- dup.ed.id[, serialno]
# drg.htd <- htd.drugs.unq.id[, serialno]
#
# VennDiagram::venn.diagram(x=list(sr.htd,icd.htd, ed.htd, drg.htd) ,
#                           category.names = c("Self-reported" , "ICD codes","Endocrinology","Medication"),
#                           filename = "htd.venn.png", output=TRUE,
#                           lwd = 2, lty = "solid",
#                           fill = c("red", "blue", "green", "yellow"))
