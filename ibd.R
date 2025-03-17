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
#'  This is the R script for extracting inflammatory bowel disease (IBD) cases in the type1bio 
#'  cohort. The cases for IBD were defined based on their self-reported questionnaire, 
#'  hospital discharge ICD codes, related medication record * >1 specialty visit.
#' 
#'


setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
source("base.extract.R")
source("psoriasis.R")


##------------------------------------------------------------------------------
## Inflammatory Bowel Disease
##------------------------------------------------------------------------------

# ============== Self reported ============== 
## Getting self reported IBD cases from diabaids.R
ibds <- aid.long[aid.long$otherconditions == 10,]
ibd <- aid.con[aid.con$Trait == "Inflammatory bowel disease" ,]

# ============== Hospital discharges ============== 
cd.diag <- dbGetSMR01Diag(con, icd9.list="^555\\.9*", icd9.match.type="RLIKE",
                          icd10.list=c("K50\\.*"), icd10.match.type="RLIKE",
                          cohort.h=NULL)

uc.diag <- dbGetSMR01Diag(con, icd9.list="^556\\.9*", icd9.match.type="RLIKE",
                          icd10.list=c("K51\\.*"), icd10.match.type="RLIKE",
                          cohort.h=NULL)

## Remove multiple recordings of hospital discharge under the same diagnosis code
cd.diag.unq <- cd.diag[!duplicated(cd.diag[ , c("serialno","diag.code")]),]
uc.diag.unq <- uc.diag[!duplicated(uc.diag[ , c("serialno","diag.code")]),]

## Get counts and description of ICD codes used
cd.codes <- as.data.table(table(cd.diag.unq$diag.code))
uc.codes <- as.data.table(table(uc.diag.unq$diag.code))
ibd.codes <- rbind(cd.codes,uc.codes)
ibd.codes <- setNames(ibd.codes, c("diag.code","Freq"))
setkey(ibd.codes,diag.code); setkey(icd9n10,diag.code)
ibd.codes.des <- merge(ibd.codes,icd9n10, all.x=TRUE)
ibd.codes.des <- setcolorder(ibd.codes.des, c("diag.code", "description", "Freq" ))

## Get unique id of patients with hospital discharge records
cd.diag.unq.id <- cd.diag.unq[!duplicated(cd.diag.unq[ , c("serialno")]),]
uc.diag.unq.id <- uc.diag.unq[!duplicated(uc.diag.unq[ , c("serialno")]),]

## Joining into IBD
setkey(cd.diag.unq.id,serialno); setkey(uc.diag.unq.id,serialno)
ibd.diag.unq.id <- merge(cd.diag.unq.id, uc.diag.unq.id, all=TRUE)

# ============== Medication ============== 
ibd.drugs0 <- drugs[ with(drugs, 
                          grepl("AZATHIOPRINE*.|BUDESONIDE*.|CICLOSPORIN*.|MESALAZINE*.|MERCAPTOPURINE*.|METHOTREXATE*.|
                                OLSALAZINE*.|((PREDNISOLONE).*(ENEMA))|((ENEMA).*(PREDNISOLONE))|((PREDNISOLONE).*(FOAM))|SULFASALAZINE*.", 
                                drugname)),]

## Removing BUDESONIDE records for uses we are not interested in
ibd.drugs <- ibd.drugs0[ with(ibd.drugs0, 
                              !grepl("((BUDESONIDE.).*(INHALER))|((BUDESONIDE.).*(TURBOHALER))|
                              ((BUDESONIDE.).*(EASYHALER))|((BUDESONIDE.).*(OINTMENT))|((BUDESONIDE.).*(INH))|
                              ((BUDESONIDE.).*(NASAL))|((BUDESONIDE.).*(SPRAY))|((BUDESONIDE.).*(INHAL))|
                              ((BUDESONIDE.).*(NOVOLIZER))|((BUDESONIDE.).*(FORMOTEROL))|((BUDESONIDE.).*(CANISTER))|
                              ((BUDESONIDE.).*(ACTUATION))|((BUDESONIDE.).*(EFORMOTEROL))|((EASYHALER).*(BUDESONIDE))|((NOVOLIZER).*(BUDESONIDE))", 
                                     drugname)),]

## Assigns new drug name for easier filtering
ibd.drugs$drug.new <- ibd.drugs$drugname
ibd.drugs$drug.new <- gsub("AZATHIOPRINE.*", "AZATHIOPRINE", 
                           gsub("BUDESONIDE.*", "BUDESONIDE", 
                                   gsub("CICLOSPORIN.*", "CICLOSPORIN", 
                                       gsub("MESALAZINE.*", "MESALAZINE",   
                                               gsub("MERCAPTOPURINE.*", "MERCAPTOPURINE",
                                                   gsub("METHOTREXATE.*", "METHOTREXATE", 
                                                       gsub("OLSALAZINE.*", "OLSALAZINE",
                                                           gsub("PREDNISOLONE.*", "PREDNISOLONE", 
                                                               gsub("SULFASALAZINE.*", "SULFASALAZINE", 
                                                                   ibd.drugs$drug.new)))))))))

## Medication taken by self-reported ibd patients
ibd.srmed <- merge(ibds,ibd.drugs, all.x=TRUE)
ibd.srmed.id <- ibd.srmed[!duplicated(ibd.srmed[ , c("serialno","drug.new")]),]

## Medication taken by icd10 ibd patients
ibd.cdmed <- merge(ibd.diag.unq.id,ibd.drugs, all.x=TRUE)
ibd.cdmed.id <- ibd.cdmed[!duplicated(ibd.cdmed[ , c("serialno", "drug.new")]),]

## Getting unique drugnames as one patient may have multiple entries
ibd.drugs.unq <- ibd.drugs[!duplicated(ibd.drugs[ , c("serialno","drug.new")]),]

## Unique no of individuals on any of these medications
ibd.drugs.unq.id <- ibd.drugs.unq[!duplicated(ibd.drugs.unq[ , c("serialno")]),]

# ============== A9 = gastroenterology (GT) ==============
smr00.gt.long <- smr00[smr00$spec == "A9",]
smr00.gt.id <- smr00.gt.long[!duplicated(smr00.gt.long[ , c("serialno")]),]

## Getting people with more than one specialty visit
dup.gt <- smr00.gt.long[duplicated(smr00.gt.long[ , c("serialno")]),]
dup.gt.id <- dup.gt[!duplicated(dup.gt[ , c("serialno")]),]

## On meds and had more than one specialty visit
gtt.drugs.unq.id <- dup.gt.id[ibd.drugs.unq.id,on="serialno", nomatch=0]

# ============== Additional checks for each drugs ============== 
#' AZATHIOPRINE also taken by RA, SLE, polymyositis, Eczema and Myasthenia gravis patients. 
#' CICLOSPORIN also taken by RA, Eczema, Psoriasis and SLE patients
#' METHOTREXATE also taken by RA, JIA, PsA, SLE 
#' SULFASALAZINE also taken by  RA, PsA 
#' As these drugs identified for IBD treatment are also taken by 
#' RA,SLE,PsA,JIA,polymyositis,Myasthenia gravis (all attending Rheumatology specialty) 
#' and psoriasis,Eczema (all attending Dermatology specialty) patients, we only included 
#' people who are on any of these drugs and are not RA/PS self-reported/ICD or have been 
#' to the Rheumatology or Dermatology specialty

## Long data - On meds and had more than one specialty visit. 
ibdd.drugs.unq <- dup.gt.id[ibd.drugs.unq, on="serialno", nomatch=0]

## Getting people on any of the meds above
check.drugs <- ibdd.drugs.unq[ibdd.drugs.unq$drug.new %in% c("AZATHIOPRINE","CICLOSPORIN","METHOTREXATE","SULFASALAZINE"),]

# ************ Bit of RA self-reported/ICD needed for IBD script to run *****************
ra <- aid.long[aid.long$otherconditions == 4,]
ra.diag <- dbGetSMR01Diag(con, icd9.list="^714\\.*", icd9.match.type="RLIKE",icd10.list=c("M05\\.*", "M06\\.*"), icd10.match.type="RLIKE",cohort.h=NULL)
ra.diag.unq.id <- ra.diag[!duplicated(ra.diag[ , c("serialno")]),]
setkey(ra,serialno); setkey(ra.diag.unq.id,serialno);
prob.ra <- merge(ra,ra.diag.unq.id, all=TRUE)

dup.rh <- smr00.rh.long[duplicated(smr00.rh.long[ , c("serialno")]),]
dup.rh.id <- dup.rh[!duplicated(dup.rh[ , c("serialno")]),]
# *****************************

## Checking if they are RA/PS self-reported/ICD or have been to the RH or DT specialty
dr.gt.drugs <- check.drugs[check.drugs$serialno %in% dup.dr.id$serialno]
rh.gt.drugs  <- check.drugs[check.drugs$serialno %in% dup.rh.id$serialno]
psr.gt.drugs  <- check.drugs[check.drugs$serialno %in% prob.psr$serialno] 
ra.gt.drugs  <- check.drugs[check.drugs$serialno %in% prob.ra$serialno]

## Joining them and collapsing data to just individual ID
list.dt <- list(dr.gt.drugs, rh.gt.drugs, psr.gt.drugs, ra.gt.drugs)
gt.other <- Reduce(function(x, y) merge(x, y, on ="serialno", all = TRUE), list.dt)
checked <- gt.other[!duplicated(gt.other[ , c("serialno")]),]

## People that had any of the IBD meds and attended >1 GT specialty where ones taking 
## AZTH, CICL, METH, SULFZ are not are RA/PS self-reported/ICD or have been to the RH or DT specialty
ibdd.drugs.unq.id.chkd <- gtt.drugs.unq.id[!(serialno %in% checked$serialno),]

# ============== Case definition ============== 
setkey(ibds,serialno);setkey(ibd.diag.unq.id,serialno)
setkey(gtt.drugs.unq.id,serialno)

## Probable cases  - ibd + ICD
prob.ibd <- merge(ibds,ibd.diag.unq.id, all=TRUE)

## Possible cases  - ibd + ICD + med*specialty>1
ibd.cases <- merge(prob.ibd, ibdd.drugs.unq.id.chkd, all=TRUE)
dim(ibd.cases)

# # ============== Venn diagram ============== 
# sr.ibd <- ibds[, serialno]
# icd.ibd <- ibd.diag.unq.id[, serialno]
# gt.ibd <- dup.gt.id[, serialno]
# drg.ibd <- ibd.drugs.unq.id[, serialno]
# 
# VennDiagram::venn.diagram(x=list(sr.ibd,icd.ibd,gt.ibd, drg.ibd ) ,
#                           category.names = c("Self reported" , "ICD codes","Gastroenterology","Medication"),
#                           filename = "ibd.venn.png", output=TRUE,
#                           lwd = 2, lty = "solid",
#                           fill = c("red", "blue", "green", "yellow"))
