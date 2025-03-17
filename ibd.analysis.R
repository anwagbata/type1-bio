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
#'  This is the analysis script for inflammatory bowel disease (IBD), it produces 
#' summary statistics for inflammatory bowel disease cases and controls. It performs
#' case-control association analysis for IBD and eQTL and pQTL core genes
#'

setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
source("base.analysis.R")
source("assoc.functions.R")
table(samp.scores$ibd)

# ============== IBD ============== 
ibd.cases <- samp.scores[ibd==1]
ibd.ctrls <- samp.scores[ibd==0]

## case
smk.cs <- tab.stat(data=ibd.cases, tab=c("smokingstatus"))
gen.cs <- tab.stat(data=ibd.cases, tab=c("gender","keto","cad"))
alc.cs <- tab.stat(data=ibd.cases, tab=c("drinkalcohol"))
agct.cs <- tab.stat(data=ibd.cases, tab=c("ageofonsetCat"))

smkp.cs <- perc.stat(data=ibd.cases, tab=c("smokingstatus"))
genp.cs <- perc.stat(data=ibd.cases, tab=c("gender","keto","nephro","retin","cad"))
alcp.cs <- perc.stat(data=ibd.cases, tab=c("drinkalcohol"))
agctp.cs <- perc.stat(data=ibd.cases, tab=c("ageofonsetCat"))

cont.cs <- sum.stat(data=ibd.cases, cols=c("age","ageofonset","duration"))
bmiav.cs <- round(mean(ibd.cases$bmi, na.rm=TRUE),1)
bmisd.cs <- round(sd(ibd.cases$bmi, na.rm=TRUE),1)
cpepav.cs <- round(mean(ibd.cases$duration.cpeptide, na.rm=TRUE),1)
cpepsd.cs <- round(sd(ibd.cases$duration.cpeptide, na.rm=TRUE),1)

## controls
smk.ct <- tab.stat(data=ibd.ctrls, tab=c("smokingstatus"))
gen.ct <- tab.stat(data=ibd.ctrls, tab=c("gender","keto","nephro","retin","cad"))
alc.ct <- tab.stat(data=ibd.ctrls, tab=c("drinkalcohol"))
agct.ct <- tab.stat(data=ibd.ctrls, tab=c("ageofonsetCat"))

smkp.ct <- perc.stat(data=ibd.ctrls, tab=c("smokingstatus"))
genp.ct <- perc.stat(data=ibd.ctrls, tab=c("gender","keto","nephro","retin","cad"))
alcp.ct <- perc.stat(data=ibd.ctrls, tab=c("drinkalcohol"))
agctp.ct <- perc.stat(data=ibd.ctrls, tab=c("ageofonsetCat"))

cont.ct <- sum.stat(data=ibd.ctrls, cols=c("age","ageofonset","duration"))
bmiav.ct <- round(mean(ibd.ctrls$bmi, na.rm=TRUE),1)
bmisd.ct <- round(sd(ibd.ctrls$bmi, na.rm=TRUE),1)
cpepav.ct <- round(mean(ibd.ctrls$duration.cpeptide, na.rm=TRUE),1)
cpepsd.ct <- round(sd(ibd.ctrls$duration.cpeptide, na.rm=TRUE),1)

##------------------------------------------------------------------------------
## Checking association for basic sample charac
##------------------------------------------------------------------------------

mod.ag <- round(assoc.var(samp.scores, samp.scores$ibd, samp.scores$age),3)
mod.ageon <- round(assoc.var(samp.scores, samp.scores$ibd, samp.scores$ageofonset),3)
mod.dud <- round(assoc.var(samp.scores, samp.scores$ibd, samp.scores$duration),3)
mod.bmi <- round(assoc.var(samp.scores, samp.scores$ibd, samp.scores$bmi),3)
mod.cpep <- round(assoc.var(samp.scores, samp.scores$celiac, samp.scores$duration.cpeptide),3)

mod.agct <-  round(assoc.var(samp.scores, samp.scores$ibd, samp.scores$ageofonsetCat),3)
mod.smk <- round(assoc.var(samp.scores, samp.scores$ibd, samp.scores$smokingstatus),3)
mod.alc <- round(assoc.var(samp.scores, samp.scores$ibd, samp.scores$drinkalcohol),3)
mod.sex <- round(assoc.var(samp.scores, samp.scores$ibd, samp.scores$gender),3)
mod.ket <- round(assoc.var(samp.scores, samp.scores$ibd, samp.scores$keto),3)
mod.cvd <- round(assoc.var(samp.scores, samp.scores$ibd, samp.scores$cad),3)

##------------------------------------------------------------------------------
## Core genes association analysis
##------------------------------------------------------------------------------

## cols.eqtl and cols.pqtl are column names of core genes
## data$p <- gt::vec_fmt_scientific(data$p, decimals = 2)

## eQTL scores
ibd.eqtl <- assoc.genes(samp.scores, samp.scores$ibd, cols.eqtl)
table10ax <- assoc.process(ibd.eqtl)
table10a <- table10ax[eqtls, on="genes", nomatch=0]
table10a <- table10a[order(table10a$p), ]
table10a$p <- round(table10a$p,3)

## pQTL scores
ibd.pqtl <- assoc.genes(samp.scores, samp.scores$ibd, cols.pqtl)
table10bx <- assoc.process(ibd.pqtl)
table10b <- table10bx[pqtls, on="genes", nomatch=0]
table10b <- table10b[order(table10b$p), ]
table10b$p <- round(table10b$p,3)
