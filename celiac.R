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
#'   3. Drugs used in autoimmune disease combined with outpatient attendance 
#'      For instance someone on sulfasalazine has either inflammatory bowel disease
#'      if attending a gastroenterology clinic or rheumatoid arthritis if 
#'      attending a rheumatology clinic. 
#'
#'
#'
#'  This is the R script for extracting celiac disease cases in the type1bio cohort
#'  There is no specific medication for celiac disease so the cases for celiac 
#'  disease are defined based on self-reported questionnaire and hospital discharge ICD codes
#' 
#'


setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
source("base.extract.R")


##------------------------------------------------------------------------------
##  Celiac disease
##------------------------------------------------------------------------------

## ============== Self reported ==============
ced <- aid.con[aid.con$Trait == "Celiac disease" ,]
ceds <- aid.long[aid.long$otherconditions == 5,]


## ============== Hospital discharges (ICD codes) ==============
ced.diag <- dbGetSMR01Diag(con, icd9.list="^579\\.0*", icd9.match.type="RLIKE",
                           icd10.list=c("K90\\.0*"), icd10.match.type="RLIKE",
                           cohort.h=NULL)

## Remove multiple recordings of hospital discharge under the same diagnosis code
ced.diag.unq <- ced.diag[!duplicated(ced.diag[ , c("serialno","diag.code")]),]

## Get counts and description of ICD codes used
ced.codes <- as.data.table(table(ced.diag.unq$diag.code))
ced.codes <- setNames(ced.codes, c("diag.code","Freq"))
setkey(ced.codes,diag.code); setkey(icd9n10,diag.code)
ced.codes.des <- merge(ced.codes,icd9n10, all.x=TRUE)
ced.codes.des <- setcolorder(ced.codes.des, c("diag.code", "description", "Freq" ))

## Get unique id of patients with hospital discharge records
ced.diag.unq.id <- ced.diag.unq[!duplicated(ced.diag.unq[ , c("serialno")]),]


# ============== Case definition ==============   
## Possible cases - sr + ICD
ced.cases <- merge(ceds, ced.diag.unq.id, on = "serialno", all=TRUE)

# ============== Venn diagram ============== 
# sr.ced <- ceds[, serialno]
# icd.ced <- ced.diag.unq.id[, serialno]
# VennDiagram::venn.diagram(x=list(sr.ced,icd.ced) ,
#                           category.names = c("Self reported" , "ICD codes "),
#                           filename = "ced.venn.png", output=TRUE,
#                           lwd = 2, lty = "solid",
#                           fill = c("red", "blue"))
