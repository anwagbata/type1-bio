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
#'  This is the analysis script for Rheumatoid arthritis, it produces summary statistics 
#'  for Rheumatoid cases and controls. It performs case-control association 
#'  analysis for Rheumatoid and eQTL and pQTL core genes
#'

setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
source("base.analysis.R")
source("assoc.functions.R")
table(samp.scores$rheumatoid)

# ============== Rheumatoid arthritis ============== 
ra.cases <- samp.scores[rheumatoid==1]
ra.ctrls <- samp.scores[rheumatoid==0]

## case
smk.cs <- tab.stat(data=ra.cases, tab=c("smokingstatus"))
gen.cs <- tab.stat(data=ra.cases, tab=c("gender","keto","cad"))
alc.cs <- tab.stat(data=ra.cases, tab=c("drinkalcohol"))
agct.cs <- tab.stat(data=ra.cases, tab=c("ageofonsetCat"))

smkp.cs <- perc.stat(data=ra.cases, tab=c("smokingstatus"))
genp.cs <- perc.stat(data=ra.cases, tab=c("gender","keto","nephro","retin","cad"))
alcp.cs <- perc.stat(data=ra.cases, tab=c("drinkalcohol"))
agctp.cs <- perc.stat(data=ra.cases, tab=c("ageofonsetCat"))

cont.cs <- sum.stat(data=ra.cases, cols=c("age","ageofonset","duration"))
bmiav.cs <- round(mean(ra.cases$bmi, na.rm=TRUE),1)
bmisd.cs <- round(sd(ra.cases$bmi, na.rm=TRUE),1)
cpepav.cs <- round(mean(ra.cases$duration.cpeptide, na.rm=TRUE),1)
cpepsd.cs <- round(sd(ra.cases$duration.cpeptide, na.rm=TRUE),1)

## controls
smk.ct <- tab.stat(data=ra.ctrls, tab=c("smokingstatus"))
gen.ct <- tab.stat(data=ra.ctrls, tab=c("gender","keto","nephro","retin","cad"))
alc.ct <- tab.stat(data=ra.ctrls, tab=c("drinkalcohol"))
agct.ct <- tab.stat(data=ra.ctrls, tab=c("ageofonsetCat"))

smkp.ct <- perc.stat(data=ra.ctrls, tab=c("smokingstatus"))
genp.ct <- perc.stat(data=ra.ctrls, tab=c("gender","keto","nephro","retin","cad"))
alcp.ct <- perc.stat(data=ra.ctrls, tab=c("drinkalcohol"))
agctp.ct <- perc.stat(data=ra.ctrls, tab=c("ageofonsetCat"))

cont.ct <- sum.stat(data=ra.ctrls, cols=c("age","ageofonset","duration"))
bmiav.ct <- round(mean(ra.ctrls$bmi, na.rm=TRUE),1)
bmisd.ct <- round(sd(ra.ctrls$bmi, na.rm=TRUE),1)
cpepav.ct <- round(mean(ra.ctrls$duration.cpeptide, na.rm=TRUE),1)
cpepsd.ct <- round(sd(ra.ctrls$duration.cpeptide, na.rm=TRUE),1)

##------------------------------------------------------------------------------
# Checking association for basic sample charac
##------------------------------------------------------------------------------

mod.ag <- round(assoc.var(samp.scores, samp.scores$rheumatoid, samp.scores$age),3)
mod.ageon <- round(assoc.var(samp.scores, samp.scores$rheumatoid, samp.scores$ageofonset),3)
mod.dud <- round(assoc.var(samp.scores, samp.scores$rheumatoid, samp.scores$duration),3)
mod.bmi <- round(assoc.var(samp.scores, samp.scores$rheumatoid, samp.scores$bmi),3)
mod.cpep <- round(assoc.var(samp.scores, samp.scores$celiac, samp.scores$duration.cpeptide),3)

mod.agct <-  round(assoc.var(samp.scores, samp.scores$rheumatoid, samp.scores$ageofonsetCat),3)
mod.smk <- round(assoc.var(samp.scores, samp.scores$rheumatoid, samp.scores$smokingstatus),3)
mod.alc <- round(assoc.var(samp.scores, samp.scores$rheumatoid, samp.scores$drinkalcohol),3)
mod.sex <- round(assoc.var(samp.scores, samp.scores$rheumatoid, samp.scores$gender),3)
mod.ket <- round(assoc.var(samp.scores, samp.scores$rheumatoid, samp.scores$keto),3)
mod.cvd <- round(assoc.var(samp.scores, samp.scores$rheumatoid, samp.scores$cad),3)

##------------------------------------------------------------------------------
## Core genes association analysis
##------------------------------------------------------------------------------

## cols.eqtl and cols.pqtl are column names of core genes
## data$p <- gt::vec_fmt_scientific(data$p, decimals = 2)

## eQTL scores
ra.eqtl <- assoc.genes(samp.scores, samp.scores$rheumatoid, cols.eqtl)
table12ax <- assoc.process(ra.eqtl)
table12a <- table12ax[eqtls, on="genes", nomatch=0]
table12a <- table12a[order(table12a$p), ]
table12a$p <- round(table12a$p,3)

## pQTL scores
ra.pqtl <- assoc.genes(samp.scores, samp.scores$rheumatoid, cols.pqtl)
table12bx <- assoc.process(ra.pqtl)
table12b <- table12bx[pqtls, on="genes", nomatch=0]
table12b <- table12b[order(table12b$p), ]
table12b$p <- round(table12b$p,3)
