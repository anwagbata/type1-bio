#' Created on 21-Mar-2023
##------------------------------------------------------------
#' Copyright (c) 2023 Diabetes Epidemiology Group.
#' All Right Reserved.
##------------------------------------------------------------
#' Author: anwagbata
#' Project topic: Are core genes of type 1 diabetes also associated with other
#' autoimmune disorders in patients with type 1 diabetes?
#' Project info:
#'  1. Basic sample characteristics of type 1 diabetes patients with other
#'     autoimmune disorders
#'  2. Processing and including scores of T1D core genes
#'  3. Performing association analysis for these core genes and case-control
#'     status of each autoimmune trait.
#'
#' This script is for additional analysis for the age of onset. It contains two
#' analysis:
#'  1. Adjusted age of onset analysis
#'  2. Using significant core genes for prediction of C-peptide levels and age
#'     of onset
setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
source("base.analysis.R")
source("assoc.functions.R")

##------------------------------------------------------------------------------
## Celiac - Adjusting age of onset analysis by age at study recruitment day
##------------------------------------------------------------------------------
#' Hypothesis 1: Younger people on study date are less likely to report celiac.
#' We look at age categories of patient that reported celiac on study date. This
#' hypothesis expects higher percentage of people reporting Celiac to be older.
#' This percentage slightly increased over the age category but might not be
#' significantly different.

perc.stat(data=samp.scores[celiac==1], tab=c("age.onstudydayCat"))
perc.stat(data=samp.scores[celiac==0], tab=c("age.onstudydayCat"))

#' "60% of people that reported having celiac on study day were above 40 years.
#' They have had the time to have celiac although we can't say what age"

#' The younger people are on the study recruitment date the less chances they
#' have not developed their celiac disease as Celiac sets in at any time in life
#' Therefore, we control for age on study recruitment day and then see if the
#' age of onset of T1D is younger for those who reported Celiac on study day
#'
#' Otherwise, you have this problem where the older they are at entry, the more
#' chances that they have Celiac disease because Celiac comes any time in life
#' but more common in adult in their twenties and thirties.
#' Bias in ascertainment of celiac.
#'
#' Hypothesis 2:: Is age of T1D onset younger in people with celiac disease on
#' study date younger T1D age of onset = more likely to have celiac?
#' older T1D age of onset  = Less likely to have celiac?

mod.ced.rc1 <- glm(celiac ~ ageofonset, family="binomial", data=samp.scores)
mod.ced2 <- glm(celiac ~ ageofonset + age.onstudyday, family="binomial",
                data=samp.scores)

ced.rc1 <- process.model(mod.ced.rc1, 2)
ced.rc2 <- process.model(mod.ced.rc2, 2)

## As age of T1D onset increases, the odds of celiac decreases by 1.5%
## Does this mean people with earlier onset of T1D are more likely to have celiac?

##------------------------------------------------------------------------------
## Including age at last prescription
##------------------------------------------------------------------------------

## including date of birth to drugs data table
dob <- dbGetQueryMap(con, "SELECT serialno,date_of_birth FROM o_person ")
drugs.dob <- drugs[dob, on="serialno", nomatch=0]

## remove rows with NA encounter date
length(drugs.dob$encounterdate)
length(na.omit(drugs.dob$encounterdate))
drugs.dob.nona <- drugs.dob[!is.na(drugs.dob$encounterdate),]

## Checking that encounter dates makes sense
drugs.dob.nona <- drugs.dob.nona[order(drugs.dob.nona$encounterdate), ]

## Remove row if encounter date is earlier than date of birth or later than
drugs.dob.nona <- drugs.dob.nona %>%
    dplyr::filter(as.Date(encounterdate) >= as.Date(date.of.birth))
drugs.dob.nona <- drugs.dob.nona[!(encounterdate >= "2023-01-01" ),]

## Age at last drug prescription for each patient. This chunk takes about
## 20mins to run
## drugs.dob.nona[, last.presc := eeptools::age_calc(date.of.birth, as.Date(encounterdate), units='years')] # takes so much time to run
## drugs.last.presc.age <- drugs.dob.nona[, .(last.presc.age = max(last.presc)), by = "serialno"]
## drugs.last.presc.age <- drugs.last.presc.age[, .(serialno,last.presc.age)]
## save(drugs.last.presc.age, file="drugs.last.presc.age.Rdata")

load("drugs.last.presc.age.Rdata")
samp.scores <- samp.scores[drugs.last.presc.age, on ="serialno", nomatch=0]

## Categorising age at last prescription
samp.scores[last.presc.age <25, last.presc.ageCat := "25below"]
samp.scores[last.presc.age >= 25 & last.presc.age < 40,
            last.presc.ageCat := "25to40"]
samp.scores[last.presc.age >= 40 & last.presc.age <= 150,
            last.presc.ageCat := "40above"]

##------------------------------------------------------------------------------
## Adjusting age of onset analysis by age at last prescription
##------------------------------------------------------------------------------

## Celiac
mod.ced1 <- glm(celiac ~ ageofonset, family="binomial", data=samp.scores)
mod.ced2 <- glm(celiac ~ ageofonset + last.presc.age, family="binomial",
                data=samp.scores)
ced1 <- process.model(mod.ced1, 2)
ced2 <- process.model(mod.ced2, 2)

## psoriasis
mod.ps1 <- glm(psoriasis ~ ageofonset, family="binomial", data=samp.scores)
mod.ps2 <- glm(psoriasis ~ ageofonset + last.presc.age, family="binomial",
               data=samp.scores)
ps1 <- process.model(mod.ps1, 2)
ps2 <- process.model(mod.ps2, 2)

## ibd
mod.ibd1 <- glm(ibd ~ ageofonset, family="binomial", data=samp.scores)
mod.ibd2 <- glm(ibd ~ ageofonset + last.presc.age, family="binomial",
                data=samp.scores)
ibd1 <- process.model(mod.ibd1, 2)
ibd2 <- process.model(mod.ibd1, 2)

## rheumatoid
mod.ra1 <- glm(rheumatoid ~ ageofonset, family="binomial", data=samp.scores)
mod.ra2 <- glm(rheumatoid ~ ageofonset + last.presc.age, family="binomial",
               data=samp.scores)
ra1 <- process.model(mod.ra1, 2)
ra2 <- process.model(mod.ra2, 2)
## As age of T1D onset increases, the odds of rheumatoid decreases by 2%
## Late onset of T1D are less likely to develop RA while early onset are more
## likely to develop RA? Can this be said for the categories?
racases <- perc.stat(data=samp.scores[rheumatoid==1], tab=c("last.presc.ageCat"))
ractrls <- perc.stat(data=samp.scores[rheumatoid==0], tab=c("last.presc.ageCat"))

## htd
mod.htd1 <- glm(htd ~ ageofonset, family="binomial", data=samp.scores)
mod.htd2 <- glm(htd ~ ageofonset + last.presc.age, family="binomial",
                data=samp.scores)
htd1 <- process.model(mod.htd1, 2)
htd2 <- process.model(mod.htd2, 2)
## As age of T1D onset increases, the odds of HTD decreases by 29%
## Late onset of T1D are less likely to develop HTD while early onset are more
## likely to develop HTD? Can this be said for the categories?
htdcases <- perc.stat(data=samp.scores[htd==1], tab=c("last.presc.ageCat"))
htdctrls <- perc.stat(data=samp.scores[htd==0], tab=c("last.presc.ageCat"))

## pernamia
mod.pa1 <- glm(pernamia ~ ageofonset, family="binomial", data=samp.scores)
mod.pa2 <- glm(pernamia ~ ageofonset + last.presc.age, family="binomial",
               data=samp.scores)
pa1 <- process.model(mod.pa1, 2)
pa2 <- process.model(mod.pa2, 2)

##------------------------------------------------------------------------------
## Predicting age of onset and c-peptide levels and using predicted values of significant core genes for HTD
##------------------------------------------------------------------------------
##
## Stepwise regression analysis for the most predictive core genes for HTD
fullMod.eqtl <- glm(htd ~ CD247+CD1E+CTLA4+CD5+IL10RA+MEOX1+LGALS3BP+FOXP3+STAT1,
                    family='binomial',data=samp.scores)
step(fullMod.eqtl)

mod.eqtl <- glm(htd ~ LGALS3BP+FOXP3+STAT1+PC1+PC2+PC3, family="binomial",
                data=samp.scores)
pred.eqtl <- data.table(predict(mod.eqtl, interval="prediction", type = "response"))
pred.eqtl <- setNames(pred.eqtl,"pred.eqtl")

fullMod.pqtl <- glm(htd ~ CCL15+CXCL9+EIF4G3+ICAM2+LAG3+LIN7B+CCL19+CRTAM+NCR1+CD5L+CD48+BPIFA2+FCGR3B+GCG,family='binomial',data=samp.scores)
step(fullMod.pqtl)

mod.pqtl <- glm(htd ~ CCL19+CRTAM+CD5L+LAG3+GCG+PC1+PC2+PC3, family="binomial",
                data=samp.scores)
pred.pqtl  <- data.table(predict(mod.pqtl, interval="prediction",
                         type="response"))
pred.pqtl <- setNames(pred.pqtl,"pred.pqtl")

## Joining to the dataset
samp.scores <- cbind(samp.scores,pred.eqtl,pred.pqtl)

## Predicting age of onset and c-peptide
mod3 <- lm(ageofonset ~ pred.eqtl, data=samp.scores)
mod3.ci <- confint(mod3)
mod4 <- lm(duration.cpeptide ~ pred.eqtl, data=samp.scores)
mod4.ci <- confint(mod4)

mod5 <- lm(ageofonset ~ pred.pqtl, data=samp.scores)
mod5.ci <- confint(mod5)
mod6 <- lm(duration.cpeptide ~ pred.pqtl, data=samp.scores)
mod6.ci <- confint(mod6)
