###############################################################################
#'  Created on 21-Jan-2023
#'  ------------------------------------------------------------
#'  Copyright (c) 2023 Diabetes Epidemiology Group.
#'  All Right Reserved.
#'  ------------------------------------------------------------
#'  Author: anwagbata
#'  Project topic: Are core genes of type 1 diabetes also associated with other autoimmune disorders 
#'                 in patients with type 1 diabetes?
#'  Project info: 
#'   1. Basic sample characteristics of type 1 diabetes patients with other autoimmune disorders
#'   2. Processing and including scores of T1D core genes
#'   3. Performing association analysis for these core genes and case-control status of each autoimmune trait.
#'     
#'
#'
#' This is the base script for this analysis, it establishes the database connection 
#'  and gets in the relevant sample characteristics dataset. It processes the eQTL and 
#'  pQTL core gene scores and joins them to the sample characteristiscs data table

setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
library(data.table)
library(dplyr)
con <- dbConn("T1B_2019")

##------------------------------------------------------------------------------
# All individuals
##------------------------------------------------------------------------------

# ============== Basic sample baseline characteristics ==============
crf <- dbGetQueryMap(con,"SELECT serialno, firstDiagnosed,Height, Weight FROM o_crf ")
person <- dbGetQueryMap(con, "SELECT serialno, date_of_birth,gender,date_of_consent,
                        earliest_mention AS date_of_diag FROM o_person ")
ques <- dbGetQueryMap(con, "SELECT serialno, smokingStatus, drinkAlcohol FROM o_patient_questionnaire")

## Merge characteristics
setkey(crf,serialno); setkey(person,serialno); setkey(ques,serialno)
list.cc <- list(crf, person, ques)
charac <- Reduce(function(x, y) merge(x, y, on="serialno", all=TRUE), list.cc)

# ============== data processing ==============
charac$height <- replace(charac$height, charac$height==0, NA) #cm
charac$height <- replace(charac$height, charac$height==-1, NA)
charac$weight <- replace(charac$weight, charac$weight==0, NA) #kg
charac$weight <- replace(charac$weight, charac$weight==-1, NA)

charac[, bmi := weight/((height)/100)^2] #kg/m2. 1cm to m = 1cm/100
charac[, age := eeptools::age_calc(date.of.birth, units='years')]
charac[, ageofonset := eeptools::age_calc(date.of.birth, as.Date(date.of.diag), units='years')]
charac[, age.onstudyday := eeptools::age_calc(date.of.birth, as.Date(date.of.consent), units='years')]
charac[, duration := eeptools::age_calc(date.of.diag, as.Date('2023-02-02'), units='years')]

## Recode variables
charac$smokingstatus[charac$smokingstatus == 1] <- "Current Smoker"
charac$smokingstatus[charac$smokingstatus == 2] <- "Ex-Smoker"
charac$smokingstatus[charac$smokingstatus == 3] <- "Never Smoked"
charac$smokingstatus[charac$smokingstatus == -1] <- "unknown"

charac$gender[charac$gender== 1] <- "Male"
charac$gender[charac$gender== 2] <- "Female"

charac$drinkalcohol[charac$drinkalcohol==1] <- "Yes"
charac$drinkalcohol[charac$drinkalcohol==2] <- "No"
charac$drinkalcohol[charac$drinkalcohol== -1] <- "unknown"

charac[ageofonset <16, ageofonsetCat := "16below"]
charac[ageofonset >= 16 & ageofonset < 25, ageofonsetCat := "16to25"]
charac[ageofonset >= 25 & ageofonset < 40, ageofonsetCat := "25to40"]
charac[ageofonset >= 40 & ageofonset <= 150, ageofonsetCat := "40above"]

charac[age.onstudyday <25, age.onstudydayCat := "25below"]
charac[age.onstudyday >= 25 & age.onstudyday < 40, age.onstudydayCat := "25to40"]
charac[age.onstudyday >= 40 & age.onstudyday <= 150, age.onstudydayCat := "40above"]

## deselect irrelevant columns
charac[, c("firstdiagnosed","height","weight","date.of.birth","date.of.diag","date.of.consent"):=NULL]

# ============== other sample characterisitics ==============
keto <- dbGetSMR01Diag(con, icd10.list=c("E08.1","E09.1","E10.1","E13.1","E14.1"),
                       icd10.match.type="RLIKE",cohort.h=NULL)

nephro <- dbGetSMR01Diag(con, icd10.list=c("E08.2$","E09.2$","E10.2$","E13.2$","E14.2$"),
                         icd10.match.type="RLIKE",cohort.h=NULL)

retin <- dbGetSMR01Diag(con, icd10.list=c("E08.3$","E09.3$*","E10.3$*","E13.3$","E14.3$"),
                        icd10.match.type="RLIKE",cohort.h=NULL)

cad <- dbGetSMR01Diag(con, icd10.list=c("^I20","^121","^I22","^I23","^I24","^I25",
                                        "^I61","^I63","^I64","^G45","^I70\\.*",
                                        "^I73\\.*","^I73\\.*","^E10\\.*","^E11\\.*",
                                       "^E13\\.*","E14\\.*"),
                        icd10.match.type="RLIKE",cohort.h=NULL)

keto.unq.id <- keto[!duplicated(keto[ , c("serialno")]),]
keto.unq.id[, keto := 1]
keto.unq.id[, c("diag.code","startdate","enddate","diag.path"):=NULL]; dim(keto.unq.id)

nephro.unq.id <- nephro[!duplicated(nephro[ , c("serialno")]),]
nephro.unq.id[, nephro := 1]
nephro.unq.id[, c("diag.code","startdate","enddate","diag.path"):=NULL]; dim(nephro.unq.id)

retin.unq.id <- retin[!duplicated(retin[ , c("serialno")]),]
retin.unq.id[, retin := 1]
retin.unq.id[, c("diag.code","startdate","enddate","diag.path"):=NULL]; dim(retin.unq.id)

cad.unq.id <- cad[!duplicated(cad[ , c("serialno")]),]
cad.unq.id[, cad := 1]
cad.unq.id[, c("diag.code","startdate","enddate","diag.path"):=NULL]; dim(cad.unq.id)

## Joining basic and other sample characteristics
list.dt <- list(charac, keto.unq.id, nephro.unq.id, retin.unq.id, cad.unq.id)
samp.cc <- Reduce(function(x, y) merge(x, y, on="serialno", all=TRUE), list.dt)

samp.cc$keto <- replace(samp.cc$keto, is.na(samp.cc$keto), 0)
samp.cc$nephro <- replace(samp.cc$nephro, is.na(samp.cc$nephro), 0)
samp.cc$retin <- replace(samp.cc$retin, is.na(samp.cc$retin), 0)
samp.cc$cad <- replace(samp.cc$cad, is.na(samp.cc$cad), 0)

##------------------------------------------------------------------------------
## Including autoimmune conditions
##------------------------------------------------------------------------------
source("base.extract.R")
source("celiac.R")
source("psoriasis.R")
source("ibd.R")
source("rheumatoid.R")
source("hypothyroid.R")
source("pern.anaemia.R")

samp.cc[serialno %in% ced.cases$serialno, celiac := 1]
samp.cc[serialno %in% ps.cases$serialno, psoriasis := 1]
samp.cc[serialno %in% ibd.cases$serialno, ibd := 1]
samp.cc[serialno %in% ra.cases$serialno, rheumatoid := 1]
samp.cc[serialno %in% htd.cases$serialno, htd := 1]
samp.cc[serialno %in% pa.cases$serialno, pernamia := 1]

samp.cc$celiac <- replace(samp.cc$celiac, is.na(samp.cc$celiac), 0)
samp.cc$psoriasis <- replace(samp.cc$psoriasis, is.na(samp.cc$psoriasis), 0)
samp.cc$ibd <- replace(samp.cc$ibd, is.na(samp.cc$ibd), 0)
samp.cc$rheumatoid <- replace(samp.cc$rheumatoid, is.na(samp.cc$rheumatoid), 0)
samp.cc$htd <- replace(samp.cc$htd, is.na(samp.cc$htd), 0)
samp.cc$pernamia <- replace(samp.cc$pernamia, is.na(samp.cc$pernamia), 0)

##------------------------------------------------------------------------------
## Including scores
##------------------------------------------------------------------------------
#' The database sample ID and genotype ID's are different thus we use the lookup
#' file to match the ID's of these individuals in both data table. The lookup file
#' contains both the database ID and the corresponding genotype ID.
#'
#' The pQTL and eQTL scores computed for the samples are als obtained from the
#' relevant directory with the relevant scores for core genes merged to the sample
#' data table.
#'

# ============== Phenotype processing ==============
phen <- fread("/opt/shared/project/type1bio/data/gwas_analysis/gen_scotland/PHENO_T1DM_GST1B_AS_NOREL_NOMODY_NOPT2_NOCPEPAB_NONEO_NORELPCA.samples")
pheno <- phen[PHENO==1]
pheno[, geno.id:= paste0(ID_1,"_",ID_2)]
pheno <- pheno[, .(geno.id, ID_1,PC1,PC2,PC3)]

#### Lookup file
lookup <- read.csv("/opt/shared/project/type1bio/data/2014/2016-04-27_V13/uvasampleid_serialno_lookup.csv")
setDT(lookup)[, paste0("UVASampleID.serialno", 1:2) := tstrsplit(UVASampleID.serialno, " ")]
lookup[, UVASampleID:= paste0(UVASampleID.serialno1,"_",UVASampleID.serialno1)]
lookup <- lookup[, .(UVASampleID, UVASampleID.serialno2)]
lookup <- setNames(lookup, c("geno.id","serialno"))

#### Merge
setkey(lookup,geno.id);setkey(pheno,geno.id)
samp.id <- lookup[pheno, on ="geno.id", nomatch=0]
samp.id <- samp.id[!duplicated(samp.id[ , c("geno.id")]),]

# ============== eQTL scores ==============
load("/opt/shared/project/type1bio/data/gwas_analysis/mrc_canada/trans_scores/t1b/trans.genotypicscore.1e-6.Rdata.gz")
load("/opt/shared/project/type1bio/data/gwas_analysis/mrc_canada/trans_scores/t1b/trans.scoresinfo.1e-6.Rdata.gz")
eqtl.scores <- genome.wide.scores
eqtl.scoresinfo <- trans.genome.wide.scoresinfo

## subsetting scores of core genes
core.genes.eqtl  <- c("CD247","CD1E","CTLA4","CD5","IL10RA","MEOX1","LGALS3BP","FOXP3","STAT1")
ecore <- eqtl.scores[, colnames(eqtl.scores) %in% core.genes.eqtl]

x <- matrix(rownames(ecore), dim(ecore)[1], 1)
ecore.scores <- ecore %>%  as.data.frame() %>%  mutate(geno.id=x)
ecore.scores <- as.data.table(ecore.scores)

# ============== pQTL scores ==============
load("/opt/shared/project/type1bio/data/gwas_analysis/mrc_canada/trans_scores/t1b_gs_pqtls_icelanders/trans.genotypicscore.1e-6.Rdata.gz")
load("/opt/shared/project/type1bio/data/gwas_analysis/mrc_canada/trans_scores/t1b_gs_pqtls_icelanders/trans.scoresinfo.1e-6.Rdata.gz")
pqtl.scores <- genome.wide.scores
pqtl.scoresinfo <- trans.genome.wide.scoresinfo

## subsetting scores of core genes
core.genes.pqtl  <- c("CCL15","CXCL9","EIF4G3","ICAM2","LAG3","LIN7B","CCL19","CRTAM","NCR1","CD5L","CD48","BPIFA2","FCGR3B","GCG")
pqtl.scoresinfo <- pqtl.scoresinfo[pqtl.scoresinfo$gene_symbol %in% core.genes.pqtl ,]
pqtl.scoresinfo <- pqtl.scoresinfo[pqtl.scoresinfo$qtl_type == "trans",]
pqtl.scoresinfo <- pqtl.scoresinfo[!duplicated(pqtl.scoresinfo[ , c("scoreid")]),]
pcore <- pqtl.scores[, colnames(pqtl.scores) %in% pqtl.scoresinfo$scoreid]

## make rowname of scores into a column
x <- matrix(rownames(pcore), dim(pcore)[1], 1)
pcore.scores <- pcore %>%  as.data.frame() %>%  mutate(geno.id=x)
pcore.scores <- as.data.table(pcore.scores)

## Renaming scoreid's of core genes to gene symbols
old.names <- c(pqtl.scoresinfo$scoreid)
new.names <- c(pqtl.scoresinfo$gene_symbol)
pcore.scores <- setnames(pcore.scores, old = old.names, new = new.names)

# ============== Merging sample characterisitcs and scores ==============
colnames(pcore.scores)[2] <- "CXCL9b"#to allow duplicated score CXCL9, calling the second CXCL9b
list.dt <- list(samp.id, ecore.scores, pcore.scores)
all.scores <- Reduce(function(x, y) merge(x, y, on ="geno.id", nomatch=0), list.dt)

## Setting to numeric and scaling all scores
all.scores$serialno <- as.integer(all.scores$serialno)
cols <- c("PC1","PC2","PC3", core.genes.pqtl,core.genes.eqtl)
all.scores <- all.scores %>% mutate_at(cols, as.numeric)
all.scores <- all.scores %>% mutate_at(cols, funs(c(scale(.))))

## Merging sample characteristics and scores
setkey(samp.cc,serialno);setkey(all.scores,serialno)
samp.scores.all <- merge(samp.cc, all.scores, all=TRUE)
samp.scores <- samp.cc[all.scores, on="serialno", nomatch=0]

## Adding c-peptide data
cpep <- fread("/opt/shared/project/type1bio/data/gwas_analysis/mrc_canada/cpeptide/phenotypes/PHENO_T1DM_T1B_SODCPEP_NOREL_NOMODY_NOPT2_NOCPEPAB_NONEO_NORELPCA.samples")
cpep <- cpep[, .(ID_1,age_diag,duration.cpeptide)]
samp.scores <- samp.scores[cpep, on ="ID_1", nomatch=0]
samp.scores <- samp.scores %>% mutate_at(c("duration.cpeptide","age_diag"), as.numeric)
