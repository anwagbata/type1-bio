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
#'  This is the R script for extracting Rheumatoid arthritis (RA) cases in the type1bio 
#'  cohort. The cases for RA were defined based on their self-reported questionnaire, 
#'  hospital discharge ICD codes, related medication record * >1 specialty visit.
#' 
#'


setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
source("base.extract.R")
source("psoriasis.R")
source("ibd.R")


##------------------------------------------------------------------------------
## Rheumatoid arthritis
##------------------------------------------------------------------------------

# ============== Self reported ==============
ras <-aid.con[aid.con$Trait == "Rheumatoid arthritis" ,]
ra <- aid.long[aid.long$otherconditions == 4,]

# ============== Hospital discharges ==============
ra.diag <- dbGetSMR01Diag(con, icd9.list="^714\\.*", icd9.match.type="RLIKE",
                          icd10.list=c("M05\\.*", "M06\\.*"), icd10.match.type="RLIKE",
                          cohort.h=NULL)

## Remove multiple recordings of hospital discharge under the same diag.code
ra.diag.unq <- ra.diag[!duplicated(ra.diag[ , c("serialno","diag.code")]),]

## Get counts and description of ICD codes used
ra.codes <- as.data.table(table(ra.diag.unq$diag.code))
ra.codes <- setNames(ra.codes, c("diag.code","Freq"))
setkey(ra.codes,diag.code); setkey(icd9n10,diag.code)
ra.codes.des <- merge(ra.codes,icd9n10, all.x=TRUE)
ra.codes.des <- setcolorder(ra.codes.des, c("diag.code", "description", "Freq" ))

## Get unique id of patients with hospital discharge records
ra.diag.unq.id <- ra.diag.unq[!duplicated(ra.diag.unq[ , c("serialno")]),]
dim(ra.diag); dim(ra.diag.unq); dim(ra.diag.unq.id)
nrow(ra.diag.unq.id) 

# ============== Medication ==============
ra.drugs <- drugs[ with(drugs, 
                        grepl("HYDROXYCHLOROQUINE*.|LEFLUNOMIDE*.|METHOTREXATE.*.|SULFASALAZINE*.",
                              drugname)),]

## Assigns new drug name for easier filtering
ra.drugs$drug.new <- ra.drugs$drugname
ra.drugs$drug.new <- gsub("HYDROXYCHLOROQUINE.*", "HYDROXYCHLOROQUINE", 
                           gsub("LEFLUNOMIDE.*", "LEFLUNOMIDE", 
                                gsub("METHOTREXATE.*", "METHOTREXATE", 
                                     gsub("SULFASALAZINE.*", "SULFASALAZINE",   
                                        ra.drugs$drug.new))))

## Medication taken by self-reported ibd patients
ra.srmed <- merge(ra,ra.drugs, all.x=TRUE)
ra.srmed.id <- ra.srmed[!duplicated(ra.srmed[ , c("serialno","drug.new")]),]

## Medication taken by icd10 ibd patients
ra.cdmed <- merge(ra.diag.unq.id,ra.drugs, all.x=TRUE)
ra.cdmed.id <- ra.cdmed[!duplicated(ra.cdmed[ , c("serialno", "drug.new")]),]

## Removed duplicated based on the two columns 
ra.drugs.unq <- ra.drugs[!duplicated(ra.drugs[ , c("serialno","drug.new")]),]

## Unique serial no of individuals on any of these medications
ra.drugs.unq.id <- ra.drugs.unq[!duplicated(ra.drugs.unq[ , c("serialno")]),]

# ============== AR = Rheumatology outpatients ============== 
smr00.rh.long <- smr00[smr00$spec == "AR",]
smr00.rh.id <- smr00.rh.long[!duplicated(smr00.rh.long[ , c("serialno")]),]

## Getting people with more than one specialty visit
dup.rh <- smr00.rh.long[duplicated(smr00.rh.long[ , c("serialno")]),]
dup.rh.id <- dup.rh[!duplicated(dup.rh[ , c("serialno")]),]

## On meds and had more than one specialty visit
rhrr.drugs.unq.id <- dup.rh.id[ra.drugs.unq.id,on="serialno", nomatch=0]
dim(rhrr.drugs.unq.id)

# ============== Additional checks for each drugs ============== 
#' HYDROXYCHLOROQUINE Also taken by SLE***
#' LEFLUNOMIDE also taken by PsA. Have to have PS before PsA, so must be at dermatology first 
#' METHOTREXATE also taken by IBD, JIA**, PsA, SLE** 
#' SULFASALAZINE also taken by  IBD, PsA 
#' As the drugs are also taken by RA and psoriasis patients we extracted people who are on any of 
#' these drugs and are RA/PS self reported or have been to the RH or DT specialty
#'
#' As these drugs (SULF,LEFL,METH) identified for RA treatment are also taken by 
#' IBD (attending gastroenterology) and PsA (attending Dermatology)patients, we only included 
#' people who are on any of these drugs and are not IBD/PS self-reported/ICD or have been 
#' to the gastroenterology or Dermatology specialty
#'
#' Additionally, these drugs (HCLQ,METH) are also taken by SLE and JIA who happens to 
#' attend the same specialty as RA patients therefore we excluded people taking only 
#' HCLQ or only METH or only a combination of both drugs.

## Long data - On meds and had more than one specialty visit. 
ra.drugs.unq.lg <- dup.rh.id[ra.drugs.unq, on="serialno", nomatch=0]

## Getting people on any of the meds above
check.drugs <- ra.drugs.unq.lg[ra.drugs.unq.lg$drug.new %in% c("LEFLUNOMIDE","METHOTREXATE","SULFASALAZINE"),]

## Checking if they are IBD/PS self reported or have been to the GT or DT specialty
dr.rh.drugs <- check.drugs[check.drugs$serialno %in% dup.dr.id$serialno]
gt.rh.drugs  <- check.drugs[check.drugs$serialno %in% dup.gt.id$serialno]
psr.rh.drugs  <- check.drugs[check.drugs$serialno %in% prob.psr$serialno] 
ibd.rh.drugs  <- check.drugs[check.drugs$serialno %in% prob.ibd$serialno]

## Joining them and collapsing data to just individual ID
list.dt <- list(dr.rh.drugs, gt.rh.drugs, psr.rh.drugs, ibd.rh.drugs)
rh.other <- Reduce(function(x, y) merge(x, y, on ="serialno", all = TRUE), list.dt)
checked <- rh.other[!duplicated(rh.other[ , c("serialno")]),]  

## People that had any of the IBD meds and attended >1 GT specialty where ones taking 
## LEFL, METH, SULFZ are not are IBD/PS self reported/ICD or have been to the GT or DT specialty
rhrr.drugs.unq.idlms <- rhrr.drugs.unq.id[!(serialno %in% checked$serialno),] 

## Excluding people that had only HCLQ or only had METH or a combination of both as they could be SLE
## Check for HYDROXYCHLOROQUINE and METHOTREXATE both used for SLE 
#' People that had only HCLQ with no other meds could be SLE, likewise 
#' people that had only METHOTREXATE. However a combination of both may still be SLE
#' Thus removed people taking only HCLQ or only METH or only a combination of both drugs.

hqmth <- ra.drugs.unq.lg[ra.drugs.unq.lg$drug.new %in% c("HYDROXYCHLOROQUINE","METHOTREXATE"),]
hqmth.id <- hqmth[!duplicated(hqmth[ , c("serialno")]),]  
rhrr.drugs.unq.idchkd <- rhrr.drugs.unq.idlms[!(serialno %in% hqmth.id$serialno),]

# ============== Case definition ============== 
setkey(ra,serialno); setkey(ra.diag.unq.id,serialno);
setkey(rhrr.drugs.unq.idchkd,serialno)

## Probable cases  - ra + ICD
prob.ra <- merge(ra,ra.diag.unq.id, all=TRUE)

## Possible cases - ra + ICD + med*specialty>1
ra.cases <- merge(prob.ra, rhrr.drugs.unq.idchkd, all=TRUE)
dim(ra.cases)

#  ============== Venn Diagram ==============
# sr.ra <- ra[, serialno]
# icd.ra <- ra.diag.unq.id[, serialno]
# rh.ra <- dup.rh.id[, serialno]
# drg.ra <- ra.drugs.unq.id[, serialno]
# 
# VennDiagram::venn.diagram(x=list(sr.ra,icd.ra, rh.ra, drg.ra ) ,
#                           category.names = c("Self reported" , "ICD codes","rheumatology","Medication"),
#                           filename = "ra.venn.png", output=TRUE,
#                           lwd = 2, lty = "solid",
#                           fill = c("red", "blue", "green", "yellow"))
