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
#'  This is the analysis script for Hypothyroidism, it produces summary statistics 
#'  for Hypothyroidism cases and controls. It performs case-control association 
#'  analysis for Hypothyroidism and eQTL and pQTL core genes
#'

setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
source("base.analysis.R")
source("assoc.functions.R")
table(samp.scores$htd)

# ============== Hypothyroidism ============== 
htd.cases <- samp.scores[htd==1]
htd.ctrls <- samp.scores[htd==0]

## case
smk.cs <- tab.stat(data=htd.cases, tab=c("smokingstatus"))
gen.cs <- tab.stat(data=htd.cases, tab=c("gender","keto","cad"))
alc.cs <- tab.stat(data=htd.cases, tab=c("drinkalcohol"))
agct.cs <- tab.stat(data=htd.cases, tab=c("ageofonsetCat"))

smkp.cs <- perc.stat(data=htd.cases, tab=c("smokingstatus"))
genp.cs <- perc.stat(data=htd.cases, tab=c("gender","keto","nephro","retin","cad"))
alcp.cs <- perc.stat(data=htd.cases, tab=c("drinkalcohol"))
agctp.cs <- perc.stat(data=htd.cases, tab=c("ageofonsetCat"))

cont.cs <- sum.stat(data=htd.cases, cols=c("age","ageofonset","duration"))
bmiav.cs <- round(mean(htd.cases$bmi, na.rm=TRUE),1)
bmisd.cs <- round(sd(htd.cases$bmi, na.rm=TRUE),1)
cpepav.cs <- round(mean(htd.cases$duration.cpeptide, na.rm=TRUE),1)
cpepsd.cs <- round(sd(htd.cases$duration.cpeptide, na.rm=TRUE),1)

## controls
smk.ct <- tab.stat(data=htd.ctrls, tab=c("smokingstatus"))
gen.ct <- tab.stat(data=htd.ctrls, tab=c("gender","keto","nephro","retin","cad"))
alc.ct <- tab.stat(data=htd.ctrls, tab=c("drinkalcohol"))
agct.ct <- tab.stat(data=htd.ctrls, tab=c("ageofonsetCat"))

smkp.ct <- perc.stat(data=htd.ctrls, tab=c("smokingstatus"))
genp.ct <- perc.stat(data=htd.ctrls, tab=c("gender","keto","nephro","retin","cad"))
alcp.ct <- perc.stat(data=htd.ctrls, tab=c("drinkalcohol"))
agctp.ct <- perc.stat(data=htd.ctrls, tab=c("ageofonsetCat"))

cont.ct <- sum.stat(data=htd.ctrls, cols=c("age","ageofonset","duration"))
bmiav.ct <- round(mean(htd.ctrls$bmi, na.rm=TRUE),1)
bmisd.ct <- round(sd(htd.ctrls$bmi, na.rm=TRUE),1)
cpepav.ct <- round(mean(htd.ctrls$duration.cpeptide, na.rm=TRUE),1)
cpepsd.ct <- round(sd(htd.ctrls$duration.cpeptide, na.rm=TRUE),1)

##------------------------------------------------------------------------------
# Checking association for basic sample charac
##------------------------------------------------------------------------------

mod.ag <- round(assoc.var(samp.scores, samp.scores$htd, samp.scores$age),3)
mod.ageon <- round(assoc.var(samp.scores, samp.scores$htd, samp.scores$ageofonset),3)
mod.dud <- round(assoc.var(samp.scores, samp.scores$htd, samp.scores$duration),3)
mod.bmi <- round(assoc.var(samp.scores, samp.scores$htd, samp.scores$bmi),3)
mod.cpep <- round(assoc.var(samp.scores, samp.scores$htd, samp.scores$duration.cpeptide),3)

mod.agct <-  round(assoc.var(samp.scores, samp.scores$htd, samp.scores$ageofonsetCat),3)
mod.smk <- round(assoc.var(samp.scores, samp.scores$htd, samp.scores$smokingstatus),3)
mod.alc <- round(assoc.var(samp.scores, samp.scores$htd, samp.scores$drinkalcohol),3)
mod.sex <- round(assoc.var(samp.scores, samp.scores$htd, samp.scores$gender),3)
mod.ket <- round(assoc.var(samp.scores, samp.scores$htd, samp.scores$keto),3)
mod.cvd <- round(assoc.var(samp.scores, samp.scores$htd, samp.scores$cad),3)

##------------------------------------------------------------------------------
## Core genes association analysis
##------------------------------------------------------------------------------

## cols.eqtl and cols.pqtl are column names of core genes
## data$p <- gt::vec_fmt_scientific(data$p, decimals = 2)

## eQTL scores
htd.eqtl <- assoc.genes(samp.scores, samp.scores$htd, cols.eqtl)
table14ax <- assoc.process(htd.eqtl)
table14a <- table14ax[eqtls, on="genes", nomatch=0]
table14a <- table14a[order(table14a$p), ]
table14a$p <- gt::vec_fmt_scientific(table14a$p, decimals = 2)

## pQTL scores
htd.pqtl <- assoc.genes(samp.scores, samp.scores$htd,cols.pqtl)
table14bx <- assoc.process(htd.pqtl)
table14b <- table14bx[pqtls, on="genes", nomatch=0]
table14b <- table14b[order(table14b$p), ]
table14b$p <- gt::vec_fmt_scientific(table14b$p, decimals = 2)
