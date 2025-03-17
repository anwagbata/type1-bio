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
#'  This is the analysis script for Pernicious anaemia, it produces summary statistics 
#' for Pernicious anaemia cases and controls. It performs case-control association 
#' analysis for Pernicious anaemia and eQTL and pQTL core genes
#'

setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
source("base.analysis.R")
source("assoc.functions.R")
table(samp.scores$pernemia)

# ============== pernicious anaemia ============== 
pa.cases <- samp.scores[pernamia==1]
pa.ctrls <- samp.scores[pernamia==0]

## case
smk.cs <- tab.stat(data=pa.cases, tab=c("smokingstatus"))
gen.cs <- tab.stat(data=pa.cases, tab=c("gender","keto","cad"))
alc.cs <- tab.stat(data=pa.cases, tab=c("drinkalcohol"))
agct.cs <- tab.stat(data=pa.cases, tab=c("ageofonsetCat"))

smkp.cs <- perc.stat(data=pa.cases, tab=c("smokingstatus"))
genp.cs <- perc.stat(data=pa.cases, tab=c("gender","keto","nephro","retin","cad"))
alcp.cs <- perc.stat(data=pa.cases, tab=c("drinkalcohol"))
agctp.cs <- perc.stat(data=pa.cases, tab=c("ageofonsetCat"))

cont.cs <- sum.stat(data=pa.cases, cols=c("age","ageofonset","duration"))
bmiav.cs <- round(mean(pa.cases$bmi, na.rm=TRUE),1)
bmisd.cs <- round(sd(pa.cases$bmi, na.rm=TRUE),1)
cpepav.cs <- round(mean(pa.cases$duration.cpeptide, na.rm=TRUE),1)
cpepsd.cs <- round(sd(pa.cases$duration.cpeptide, na.rm=TRUE),1)

## controls
smk.ct <- tab.stat(data=pa.ctrls, tab=c("smokingstatus"))
gen.ct <- tab.stat(data=pa.ctrls, tab=c("gender","keto","nephro","retin","cad"))
alc.ct <- tab.stat(data=pa.ctrls, tab=c("drinkalcohol"))
agct.ct <- tab.stat(data=pa.ctrls, tab=c("ageofonsetCat"))

smkp.ct <- perc.stat(data=pa.ctrls, tab=c("smokingstatus"))
genp.ct <- perc.stat(data=pa.ctrls, tab=c("gender","keto","nephro","retin","cad"))
alcp.ct <- perc.stat(data=pa.ctrls, tab=c("drinkalcohol"))
agctp.ct <- perc.stat(data=pa.ctrls, tab=c("ageofonsetCat"))

cont.ct <- sum.stat(data=pa.ctrls, cols=c("age","ageofonset","duration"))
bmiav.ct <- round(mean(pa.ctrls$bmi, na.rm=TRUE),1)
bmisd.ct <- round(sd(pa.ctrls$bmi, na.rm=TRUE),1)
cpepav.ct <- round(mean(pa.ctrls$duration.cpeptide, na.rm=TRUE),1)
cpepsd.ct <- round(sd(pa.ctrls$duration.cpeptide, na.rm=TRUE),1)

##------------------------------------------------------------------------------
# Checking association for basic sample charac
##------------------------------------------------------------------------------

mod.ag <- round(assoc.var(samp.scores, samp.scores$pernamia, samp.scores$age),3)
mod.ageon <- round(assoc.var(samp.scores, samp.scores$pernamia, samp.scores$ageofonset),3)
mod.dud <- round(assoc.var(samp.scores, samp.scores$pernamia, samp.scores$duration),3)
mod.bmi <- round(assoc.var(samp.scores, samp.scores$pernamia, samp.scores$bmi),3)
mod.cpep <- round(assoc.var(samp.scores, samp.scores$htd, samp.scores$duration.cpeptide),3)

mod.agct <-  round(assoc.var(samp.scores, samp.scores$pernamia, samp.scores$ageofonsetCat),3)
mod.smk <- round(assoc.var(samp.scores, samp.scores$pernamia, samp.scores$smokingstatus),3)
mod.alc <- round(assoc.var(samp.scores, samp.scores$pernamia, samp.scores$drinkalcohol),3)
mod.sex <- round(assoc.var(samp.scores, samp.scores$pernamia, samp.scores$gender),3)
mod.ket <- round(assoc.var(samp.scores, samp.scores$pernamia, samp.scores$keto),3)
mod.cvd <- round(assoc.var(samp.scores, samp.scores$pernamia, samp.scores$cad),3)

##------------------------------------------------------------------------------
## Core genes association analysis
##------------------------------------------------------------------------------

## cols.eqtl and cols.pqtl are column names of core genes
## data$p <- gt::vec_fmt_scientific(data$p, decimals = 2)

## eQTL scores
pa.eqtl <- assoc.genes(samp.scores, samp.scores$pernamia,cols.eqtl)
table16ax <- assoc.process(pa.eqtl)
table16a <- table16ax[eqtls, on="genes", nomatch=0]
table16a <- table16a[order(table16a$p), ]
table16a$p <- gt::vec_fmt_scientific(table16a$p, decimals = 2)

## pQTL scores
pa.pqtl <- assoc.genes(samp.scores, samp.scores$pernamia,cols.pqtl)
table16bx <- assoc.process(pa.pqtl)
table16b <- table16bx[pqtls, on="genes", nomatch=0]
table16b <- table16b[order(table16b$p), ]
table16b$p <- round(table16b$p,3)
