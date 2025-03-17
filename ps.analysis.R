###############################################################################
#'  Created on 21-Jan-2023
#'  ------------------------------------------------------------
#'  Copyright (c) 2023 Diabetes Epidemiology Group.
#'  All Right Reserved.
#'  ------------------------------------------------------------
#'  Author: anwagbata
#'  Project topic: Are core genes of type 1 diabetes also associated with other autoimmune disorders 
#'                in patients with type 1 diabetes?
#'  Project info: 
#'   1. Basic sample characteristics of type 1 diabetes patients with other autoimmune disorders
#'   2. Processing and including scores of T1D core genes
#'   3. Performing association analysis for these core genes and case-control status of each autoimmune trait.
#'     
#'
#'
#'  This is the analysis script for psoriasis, it produces summary statistics 
#'  for psoriasis cases and controls. It performs case-control association 
#'  analysis for psoriasis and eQTL and pQTL core genes
#'

setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
source("base.analysis.R")
source("assoc.functions.R")
table(samp.scores$psoriasis)

# ============== Psoriasis ============== 
ps.cases <- samp.scores[psoriasis==1]
ps.ctrls <- samp.scores[psoriasis==0]

## case
smk.cs <- tab.stat(data=ps.cases, tab=c("smokingstatus"))
gen.cs <- tab.stat(data=ps.cases, tab=c("gender","keto","cad"))
alc.cs <- tab.stat(data=ps.cases, tab=c("drinkalcohol"))
agct.cs <- tab.stat(data=ps.cases, tab=c("ageofonsetCat"))

smkp.cs <- perc.stat(data=ps.cases, tab=c("smokingstatus"))
genp.cs <- perc.stat(data=ps.cases, tab=c("gender","keto","nephro","retin","cad"))
alcp.cs <- perc.stat(data=ps.cases, tab=c("drinkalcohol"))
agctp.cs <- perc.stat(data=ps.cases, tab=c("ageofonsetCat"))

cont.cs <- sum.stat(data=ps.cases, cols=c("age","ageofonset","duration"))
bmiav.cs <- round(mean(ps.cases$bmi, na.rm=TRUE),1)
bmisd.cs <- round(sd(ps.cases$bmi, na.rm=TRUE),1)
cpepav.cs <- round(mean(ps.cases$duration.cpeptide, na.rm=TRUE),1)
cpepsd.cs <- round(sd(ps.cases$duration.cpeptide, na.rm=TRUE),1)

## controls
smk.ct <- tab.stat(data=ps.ctrls, tab=c("smokingstatus"))
gen.ct <- tab.stat(data=ps.ctrls, tab=c("gender","keto","nephro","retin","cad"))
alc.ct <- tab.stat(data=ps.ctrls, tab=c("drinkalcohol"))
agct.ct <- tab.stat(data=ps.ctrls, tab=c("ageofonsetCat"))

smkp.ct <- perc.stat(data=ps.ctrls, tab=c("smokingstatus"))
genp.ct <- perc.stat(data=ps.ctrls, tab=c("gender","keto","nephro","retin","cad"))
alcp.ct <- perc.stat(data=ps.ctrls, tab=c("drinkalcohol"))
agctp.ct <- perc.stat(data=ps.ctrls, tab=c("ageofonsetCat"))

cont.ct <- sum.stat(data=ps.ctrls, cols=c("age","ageofonset","duration"))
bmiav.ct <- round(mean(ps.ctrls$bmi, na.rm=TRUE),1)
bmisd.ct <- round(sd(ps.ctrls$bmi, na.rm=TRUE),1)
cpepav.ct <- round(mean(ps.ctrls$duration.cpeptide, na.rm=TRUE),1)
cpepsd.ct <- round(sd(ps.ctrls$duration.cpeptide, na.rm=TRUE),1)

##------------------------------------------------------------------------------
# Checking association for basic sample charac
##------------------------------------------------------------------------------

mod.ag <- round(assoc.var(samp.scores, samp.scores$psoriasis, samp.scores$age),3)
mod.ageon <- round(assoc.var(samp.scores, samp.scores$psoriasis, samp.scores$ageofonset),3)
mod.dud <- round(assoc.var(samp.scores, samp.scores$psoriasis, samp.scores$duration),3)
mod.bmi <- round(assoc.var(samp.scores, samp.scores$psoriasis, samp.scores$bmi),3)
mod.cpep <- round(assoc.var(samp.scores, samp.scores$celiac, samp.scores$duration.cpeptide),3)

mod.agct <-  round(assoc.var(samp.scores, samp.scores$psoriasis, samp.scores$ageofonsetCat),3)
mod.smk <- round(assoc.var(samp.scores, samp.scores$psoriasis, samp.scores$smokingstatus),3)
mod.alc <- round(assoc.var(samp.scores, samp.scores$psoriasis, samp.scores$drinkalcohol),3)
mod.sex <- round(assoc.var(samp.scores, samp.scores$psoriasis, samp.scores$gender),3)
mod.ket <- round(assoc.var(samp.scores, samp.scores$psoriasis, samp.scores$keto),3)
mod.cvd <- round(assoc.var(samp.scores, samp.scores$psoriasis, samp.scores$cad),3)

##------------------------------------------------------------------------------
## Core genes association analysis
##------------------------------------------------------------------------------

## cols.eqtl and cols.pqtl are column names of core genes
## data$p <- gt::vec_fmt_scientific(data$p, decimals = 2)

## eQTL scores
ps.eqtl <- assoc.genes(samp.scores, samp.scores$psoriasis, cols.eqtl)
table8ax <- assoc.process(ps.eqtl)
table8a <- table8ax[eqtls, on="genes", nomatch=0]
table8a <- table8a[order(table8a$p), ]
table8a$p <- round(table8a$p,3)

## pQTL scores
ps.pqtl <- assoc.genes(samp.scores, samp.scores$psoriasis, cols.pqtl)
table8bx <- assoc.process(ps.pqtl)
table8b <- table8bx[pqtls, on="genes", nomatch=0]
table8b <- table8b[order(table8b$p), ]
table8b$p <- round(table8b$p,3)
