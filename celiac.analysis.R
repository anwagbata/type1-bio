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
#'  This is the analysis script for celiac disease, it produces summary statistics 
#' for celiac disease cases and controls. It performs case-control association 
#' analysis for celiac disease and eQTL and pQTL core genes
#'
#' 

setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
source("base.analysis.R")
source("assoc.functions.R")
table(samp.scores$celiac)

##------------------------------------------------------------------------------
## Sample characteristics summaries
##------------------------------------------------------------------------------
ced.cases <- samp.scores[celiac==1]
ced.ctrls <- samp.scores[celiac==0]

## case
smk.cs <- tab.stat(data=ced.cases, tab=c("smokingstatus"))
gen.cs <- tab.stat(data=ced.cases, tab=c("gender","keto","cad"))
alc.cs <- tab.stat(data=ced.cases, tab=c("drinkalcohol"))
agct.cs <- tab.stat(data=ced.cases, tab=c("ageofonsetCat"))

smkp.cs <- perc.stat(data=ced.cases, tab=c("smokingstatus"))
genp.cs <- perc.stat(data=ced.cases, tab=c("gender","keto","nephro","retin","cad"))
alcp.cs <- perc.stat(data=ced.cases, tab=c("drinkalcohol"))
agctp.cs <- perc.stat(data=ced.cases, tab=c("ageofonsetCat"))

cont.cs <- sum.stat(data=ced.cases, cols=c("age","ageofonset","duration"))
bmiav.cs <- round(mean(ced.cases$bmi, na.rm=TRUE),1)
bmisd.cs <- round(sd(ced.cases$bmi, na.rm=TRUE),1)
cpepav.cs <- round(mean(ced.cases$duration.cpeptide, na.rm=TRUE),1)
cpepsd.cs <- round(sd(ced.cases$duration.cpeptide, na.rm=TRUE),1)

## controls
smk.ct <- tab.stat(data=ced.ctrls, tab=c("smokingstatus"))
gen.ct <- tab.stat(data=ced.ctrls, tab=c("gender","keto","Hi nephro","retin","cad"))
alc.ct <- tab.stat(data=ced.ctrls, tab=c("drinkalcohol"))
agct.ct <- tab.stat(data=ced.ctrls, tab=c("ageofonsetCat"))

smkp.ct <- perc.stat(data=ced.ctrls, tab=c("smokingstatus"))
genp.ct <- perc.stat(data=ced.ctrls, tab=c("gender","keto","nephro","retin","cad"))
alcp.ct <- perc.stat(data=ced.ctrls, tab=c("drinkalcohol"))
agctp.ct <- perc.stat(data=ced.ctrls, tab=c("ageofonsetCat"))

cont.ct <- sum.stat(data=ced.ctrls, cols=c("age","ageofonset","duration"))
bmiav.ct <- round(mean(ced.ctrls$bmi, na.rm=TRUE),1)
bmisd.ct <- round(sd(ced.ctrls$bmi, na.rm=TRUE),1)
cpepav.ct <- round(mean(ced.ctrls$duration.cpeptide, na.rm=TRUE),1)
cpepsd.ct <- round(sd(ced.ctrls$duration.cpeptide, na.rm=TRUE),1)

##------------------------------------------------------------------------------
# Checking association for basic sample charac
##------------------------------------------------------------------------------

mod.ag <- round(assoc.var(samp.scores, samp.scores$celiac, samp.scores$age),3)
mod.ageon <- round(assoc.var(samp.scores, samp.scores$celiac, samp.scores$ageofonset),3)
mod.dud <- round(assoc.var(samp.scores, samp.scores$celiac, samp.scores$duration),3)
mod.bmi <- round(assoc.var(samp.scores, samp.scores$celiac, samp.scores$bmi),3)
mod.cpep <- round(assoc.var(samp.scores, samp.scores$celiac, samp.scores$duration.cpeptide),3)

mod.agct <-  round(assoc.var(samp.scores, samp.scores$celiac, samp.scores$ageofonsetCat),3)
mod.smk <- round(assoc.var(samp.scores, samp.scores$celiac, samp.scores$smokingstatus),3)
mod.alc <- round(assoc.var(samp.scores, samp.scores$celiac, samp.scores$drinkalcohol),3)
mod.sex <- round(assoc.var(samp.scores, samp.scores$celiac, samp.scores$gender),3)
mod.ket <- round(assoc.var(samp.scores, samp.scores$celiac, samp.scores$keto),3)
mod.cvd <- round(assoc.var(samp.scores, samp.scores$celiac, samp.scores$cad),3)

xx <- glm(celiac ~ ageofonsetCat, family="binomial", data=samp.scores)

##------------------------------------------------------------------------------
## Core genes association analysis
##------------------------------------------------------------------------------

## cols.eqtl and cols.pqtl are column names of core genes
## data$p <- gt::vec_fmt_scientific(data$p, decimals = 2)

# eQTL scores
ced.eqtl <- assoc.genes(samp.scores, samp.scores$celiac, cols.eqtl)
table6ax <- assoc.process(ced.eqtl)
table6a <- table6ax[eqtls, on="genes", nomatch=0]
table6a <- table6a[order(table6a$p), ]
table6a$p <- round(table6a$p,3)

# pQTL scores
ced.pqtl <- assoc.genes(samp.scores, samp.scores$celiac, cols.pqtl)
table6bx <- assoc.process(ced.pqtl)
table6b <- table6bx[pqtls, on="genes", nomatch=0]
table6b <- table6b[order(table6b$p), ]
table6b$p <- round(table6b$p,3)
