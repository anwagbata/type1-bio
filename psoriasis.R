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
#'  This is the R script for extracting Psoriasis cases in the type1bio cohort
#'  The cases for Psoriasis were defined based on their self-reported questionnaire,
#'  hospital discharge ICD codes, related medication record * >1 specialty visit.
#'
#'


setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
source("base.extract.R")


##------------------------------------------------------------------------------
## Psoriasis
##------------------------------------------------------------------------------

# ============== Self reported ==============
# Getting self reported psoriasis cases from diabaids.R
ps <-aid.con[aid.con$Trait == "Psoriasis" ,]
psr <- aid.long[aid.long$otherconditions == 15,]

# ============== Hospital discharges ==============
ps.diag <- dbGetSMR01Diag(con, icd9.list="^696\\.*", icd9.match.type="RLIKE",
                          icd10.list=c("L40\\.*"), icd10.match.type="RLIKE",
                          cohort.h=NULL)

## Remove multiple recordings of hospital discharge under the same diagnosis code
ps.diag.unq <- ps.diag[!duplicated(ps.diag[ , c("serialno","diag.code")]),]

## Get counts and description of ICD codes used
ps.codes <- as.data.table(table(ps.diag.unq$diag.code))
ps.codes <- setNames(ps.codes, c("diag.code","Freq"))
setkey(ps.codes,diag.code); setkey(icd9n10,diag.code)
ps.codes.des <- merge(ps.codes,icd9n10, all.x=TRUE)
ps.codes.des <- setcolorder(ps.codes.des, c("diag.code", "description", "Freq" ))

## Get unique id of patients with hospital discharge records
ps.diag.unq.id <- ps.diag.unq[!duplicated(ps.diag.unq[ , c("serialno")]),]

# ============== Medication ==============
## Selected psoriasis medication
ps.med <- drugs[ with(drugs,
                      grepl("CALCITRIOL*.|CALCIPOTRIOL*.|DITHRANOL*.|TACALCITOL*.|TAZAROTENE*.",
                            drugname)),]

## Removing CALCITRIOL  records for uses we are not intereseted in
ps.drugs <- ps.med[ with(ps.med,
                         !grepl("((CALCITRIOL).*(CAPSULES))|((CALCITRIOL).*(CAPS))",
                                drugname)),]

## Medication taken by self-reported psoriasis patients
ps.srmed <- merge(psr,ps.drugs, all.x=TRUE)
ps.srmed.id <- ps.srmed[!duplicated(ps.srmed[ , c("serialno","drugname")]),]

## Medication taken by hospital discharge (icd) psoriasis patients
ps.cdmed <- merge(ps.diag.unq.id,ps.drugs, all.x=TRUE)
ps.cdmed.id <- ps.cdmed[!duplicated(ps.cdmed[ , c("serialno", "drugname")]),]

## Getting unique drugnames as one patient may have multiple drug entries
ps.drugs.unq <- ps.drugs[!duplicated(ps.drugs[ , c("serialno","drugname")]),]

## Unique no of individuals on any of these medications
ps.drugs.unq.id <- ps.drugs.unq[!duplicated(ps.drugs.unq[ , c("serialno")]),]

# ============== A7 = Dermatology (DR), AR = Rheumatology (RH) outpatients specialty
smr00.dr.long <- smr00[smr00$spec == "A7",]
smr00.rh.long <- smr00[smr00$spec == "AR",]
smr00.dr.long <- setNames(smr00.dr.long, c("serialno","derma"))
smr00.rh.long <- setNames(smr00.rh.long, c("serialno","rheu"))
smr00.dr.id <- smr00.dr.long[!duplicated(smr00.dr.long[ , c("serialno")]),]
smr00.rh.id <- smr00.rh.long[!duplicated(smr00.rh.long[ , c("serialno")]),]

## Getting people with more than one specialty visit
dup.dr <- smr00.dr.long[duplicated(smr00.dr.long[ , c("serialno")]),]
dup.dr.id <- dup.dr[!duplicated(dup.dr[ , c("serialno")]),]
dup.rh <- smr00.rh.long[duplicated(smr00.rh.long[ , c("serialno")]),]
dup.rh.id <- dup.rh[!duplicated(dup.rh[ , c("serialno")]),]

## On meds and had more than one specialty visit
drr.drugs.unq.id <- dup.dr.id[ps.drugs.unq.id, on="serialno", nomatch=0]
rhh.drugs.unq.id <- dup.rh.id[ps.drugs.unq.id, on="serialno", nomatch=0]
spes.drugs.unq.id <- merge(drr.drugs.unq.id,rhh.drugs.unq.id, all=TRUE)

#  ============== Case definition ==============
setkey(psr,serialno); setkey(ps.diag.unq.id,serialno)
setkey(spes.drugs.unq.id,serialno)

## Probable cases - psr + ICD
prob.psr <- merge(psr, ps.diag.unq.id, all=TRUE)

## Possible cases - psr + ICD + med*specialty>1
ps.cases <- merge(prob.psr, spes.drugs.unq.id, all=TRUE)
dim(ps.cases)

# ============== Venn diagram ==============
# sr.psr <- psr[, serialno]
# icd.psr <- ps.diag.unq.id[, serialno]
# drrh.psr <- dup.drrh.id[, serialno]
# drg.psr <- ps.drugs.unq.id[, serialno]
#
# VennDiagram::venn.diagram(x=list(sr.psr,icd.psr,drrh.psr,drg.psr) ,
#                           category.names = c("Self reported","ICD codes","Specialty","Medication"),
#                           filename = "ps.venn.png", output=TRUE,
#                           lwd = 2, lty = "solid",
#                           fill = c("red", "blue", "green", "yellow"))
