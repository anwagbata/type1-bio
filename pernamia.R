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
#'  This is the R script for extracting Pernicious anemia (PA) cases in the type1bio
#'  cohort. The cases for IBD were defined based on their self-reported questionnaire,
#'  hospital discharge ICD codes, related medication record * >1 specialty visit.
#'
#'


setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
source("base.extract.R")

##------------------------------------------------------------------------------
## Pernicious anaemia
##------------------------------------------------------------------------------

# ============== Self reported ==============
# Getting self reported IBD cases from diabaids.R
pas <- aid.long[aid.long$otherconditions == 11,]
pa <- aid.con[aid.con$Trait == "Pernicious anemia" ,]

# ============== Hospital discharges ==============
pa.diag <- dbGetSMR01Diag(con, icd9.list="^281\\.*", icd9.match.type="RLIKE",
                          icd10.list=c("D51\\.*"), icd10.match.type="RLIKE",
                          cohort.h=NULL)

## Remove multiple recordings of hospital discharge under the same diagnosis code
pa.diag.unq <- pa.diag[!duplicated(pa.diag[ , c("serialno","diag.code")]),]

## Get counts and description of ICD codes used
pa.codes <- as.data.table(table(pa.diag.unq$diag.code))
pa.codes <- setNames(pa.codes, c("diag.code","Freq"))
setkey(pa.codes,diag.code); setkey(icd9n10,diag.code)
pa.codes.des <- merge(pa.codes,icd9n10, all.x=TRUE)
pa.codes.des <- setcolorder(pa.codes.des, c("diag.code", "description", "Freq" ))

## Get unique id of patients with hospital discharge records
pa.diag.unq.id <- pa.diag.unq[!duplicated(pa.diag.unq[ , c("serialno")]),]

# ============== Medication ==============
pa.drugs <- drugs[ with(drugs,
                      grepl("CYANOCOBALAMIN*.|HYDROXOCOBALAMIN*.",
                            drugname)),]

## Assigns new drug name for easier filtering
pa.drugs$drug.new <- pa.drugs$drugname
pa.drugs$drug.new <- gsub("CYANOCOBALAMIN.*", "CYANOCOBALAMIN",
                          gsub("HYDROXOCOBALAMIN.*", "HYDROXOCOBALAMIN",
                               pa.drugs$drug.new))

## Medication taken by self-reported PA patients
pa.srmed <- merge(pas,pa.drugs, all.x=TRUE)
ps.srmed.id <- pa.srmed[!duplicated(pa.srmed[ , c("serialno","drug.new")]),]

## Medication taken by hospital discharge (icd) PA patients
pa.cdmed <- merge(pa.diag.unq.id,pa.drugs, all.x=TRUE)
pa.cdmed.id <- pa.cdmed[!duplicated(pa.cdmed[ , c("serialno", "drug.new")]),]

## Getting unique drugnames as one patient may have multiple drug entries
pa.drugs.unq <- pa.drugs[!duplicated(pa.drugs[ , c("serialno","drugname")]),]

## Unique no of individuals on any of these medications
pa.drugs.unq.id <- pa.drugs.unq[!duplicated(pa.drugs.unq[ , c("serialno")]),]

# ============== GT = gastroenterology, J4 = Haematology outpatients ==============
## From base.extract.R
smr00.hm.long <- setNames(smr00.hm.long, c("serialno","haema"))
smr00.gt.long <- setNames(smr00.gt.long, c("serialno","gastro"))
smr00.hm.id <- smr00.hm.long[!duplicated(smr00.hm.long[ , c("serialno")]),]
smr00.gt.id <- smr00.gt.long[!duplicated(smr00.gt.long[ , c("serialno")]),]

## Getting people with more than one specialty visit
dup.hm <- smr00.hm.long[duplicated(smr00.hm.long[ , c("serialno")]),]
dup.hm.id <- dup.hm[!duplicated(dup.hm[ , c("serialno")]),]
dup.gt <- smr00.gt.long[duplicated(smr00.gt.long[ , c("serialno")]),]
dup.gt.id <- dup.gt[!duplicated(dup.gt[ , c("serialno")]),]

## On meds and had more than one specialty visit
hmm.drugs.unq.id <- dup.hm.id[pa.drugs.unq.id, on="serialno", nomatch=0]
gtt.drugs.unq.id <- dup.gt.id[pa.drugs.unq.id, on="serialno", nomatch=0]
spegh.drugs.unq.id <- merge(hmm.drugs.unq.id,gtt.drugs.unq.id, all=TRUE)

#  ============== Case definition ==============
setkey(pas,serialno); setkey(pa.diag.unq.id,serialno)
setkey(spegh.drugs.unq.id,serialno)

## Probable cases - psr + ICD
prob.pa <- merge(pas, pa.diag.unq.id, all=TRUE)

## Possible cases - psr + ICD + med*specialty>1
pa.cases <- merge(prob.pa, spegh.drugs.unq.id, all=TRUE)
dim(pa.cases)

#  ============== Venn Diagram ==============
# sr.pa <- pas[, serialno]
# icd.pa <- pa.diag.unq.id[, serialno]
# sp.pa <- dup.hmgt.id[, serialno]
# drg.pa <- pa.drugs.unq.id[, serialno]
#
# VennDiagram::venn.diagram(x=list(sr.pa,icd.pa, sp.pa, drg.pa ) ,
#                           category.names = c("Self reported" , "ICD codes","Specialties","Medication"),
#                           filename = "pa.venn.png", output=TRUE,
#                           lwd = 2, lty = "solid",
#                           fill = c("red", "blue", "green", "yellow"))
