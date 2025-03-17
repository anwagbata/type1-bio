#' Created on 21-Jan-2023
#' ------------------------------------------------------------
#' Copyright (c) 2023 Diabetes Epidemiology Group.
#' All Right Reserved.
#' ------------------------------------------------------------
#' Author: anwagbata
#' Project topic: Are core genes of type 1 diabetes also associated with other
#' autoimmune disorders  in patients with type 1 diabetes?
#' Project info:
#'  1. Basic sample characteristics of type 1 diabetes patients with other
#'     autoimmune disorders
#'  2. Processing and including scores of T1D core genes
#'  3. Performing association analysis for these core genes and case-control
#'     status of each autoimmune trait.
#'
#' This is the association function script for this analysis, it contains the
#' functions required to
#'   1. Make basic tabulations and summary statistics for both categorical and
#'      continious variables for cases and controls sample characteristics
#'      table.
#'   2. Perform association analysis for core genes
setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
library(data.table)

##------------------------------------------------------------------------------
## Sample characteristics summaries
##------------------------------------------------------------------------------

# ============== Basic summary statistics functions ==============
## Categorical summary statistics for count
tab.stat <- function(data, tab ){
  dt.cc <- data[, lapply(.SD, function(x) table(x)), .SDcols = tab]
  return(dt.cc)
} # Note: factors (tab) needs to be the same level

## Categorical summary statistics for percentage
perc.stat <- function(data, tab ){
dt.per <- data[, lapply(.SD, function(x) (table(x)/nrow(data))*100), .SDcols = tab]
return(dt.per)
} # Note: factors (tab) needs to be the same level

## Continuous data to write into function
sum.stat <- function(data, cols){
  stats <- c('mean', 'sd')
  dt.samp.cc <- data[, lapply(.SD, function(x) c(mean(x), sd(x))), .SDcols = cols]
  sum.dt <- as.data.table(t(dt.samp.cc))[] # transpose and provide meaningful names
  setnames(sum.dt, names(sum.dt), stats)
  sum.dt[, var := cols]
  setcolorder(sum.dt, c("var", stats))
  print(sum.dt)
}

## Functions for checking association for sample charac
##------------------------------------------------------

## single variable
assoc.var <- function(data, outcome, var){
  mod.agect <- glm(outcome ~ var, family="binomial",data=data)
  coeff <- data.table(summary(mod.agect)$coefficients[-1, , drop=FALSE])
  colnames(coeff) <- c("OR", "stderr", "zvalue", "p")
  coeff$L95 <- coeff$OR - (1.96 * coeff$stderr)
  coeff$U95 <- coeff$OR + (1.96 * coeff$stderr)
  coeff$OR <-  exp(coeff$OR)
  coeff$L95 <-  exp(coeff$L95)
  coeff$L95 <-  exp(coeff$L95)
  return(coeff)
}

##------------------------------------------------------------------------------
## Association analysis function
##------------------------------------------------------------------------------

assoc.genes <- function(data, outcome, genes){
  newrun <- TRUE
  if(newrun) {
    coeffs <- NULL
    for(gene in genes) {
      formula <-as.formula(paste("outcome ~", gene, "+ PC1 + PC2 + PC3"))
      score.model <- glm(formula=formula,family="binomial", data=data)
      coeff <- summary(score.model)$coefficients[2, , drop=FALSE]
      rownames(coeff) <- gene
      coeffs <- rbind(coeffs, coeff)
    }
    coeffs <- data.table(matrix.colname=rownames(coeffs), coeffs)
    colnames(coeffs) <- c("genes","OR", "stderr", "zvalue", "p")
    coeffs[, L95 := OR - (1.96 * stderr)]
    coeffs[, U95 := OR + (1.96 * stderr)]
    coeffs[, OR := exp(OR)]
    coeffs[, L95 := exp(L95)]
    coeffs[, U95 := exp(U95)]
    coeffs <- coeffs[order(coeffs$p), ]
    save(coeffs, file = "coeffs.RData")
    return(coeffs)

  } else {
    load("coeffs.RData")
  }
}

assoc.process <- function(data){
  cols <- names(data)[c(2,3,4,6,7)]
  data[,(cols) := round(.SD,2), .SDcols=cols]
 #data$p <- round(data$p,3)
  data$CI <- paste(data$L95, sep=", ",data$U95)
  data$CI  <- paste0("(", data$CI , ")")
  data$ORC1 <- paste(data$OR,data$CI)
  #data$p <- gt::vec_fmt_scientific(data$p, decimals = 2)
  data.f <- data[, c("genes","ORC1","p")]
  return(data.f)
}

##------------------------------------------------------------------------------
## Effect directions
##------------------------------------------------------------------------------

cols.eqtl <- c("CTLA4","LGALS3BP","STAT1","MEOX1","CD1E","CD5","FOXP3","IL10RA","CD247")
cols.pqtl <- c("CCL15","CXCL9","EIF4G3","ICAM2","LAG3","LIN7B","CCL19","CRTAM","NCR1","CD5L","CD48","BPIFA2","FCGR3B","GCG")

eqtls <- data.table(
  genes = c("CTLA4","LGALS3BP","STAT1","MEOX1","CD1E","CD5","FOXP3","IL10RA","CD247"),
  effect.direction =  c("+","+","+","-","-","+","+","+","-"))

pqtls <- data.table(
	genes = c("CCL15","CXCL9","EIF4G3","ICAM2","LAG3","LIN7B","CCL19","CRTAM","NCR1","CD5L","CD48","BPIFA2","FCGR3B","GCG"),
	effect.direction =  c("+","-","-","+","+","+","+","-","+","+","+","+","+","+"))

##------------------------------------------------------------------------------
## Age of onset analysis - Processing model with many variables
##------------------------------------------------------------------------------

process.model <- function(model, i){
  coeff <- data.table(summary(model)$coefficients[i, , drop=FALSE])
  colnames(coeff) <- c("OR", "stderr", "zvalue", "p")
  coeff$L95 <- coeff$OR - (1.96 * coeff$stderr)
  coeff$U95 <- coeff$OR + (1.96 * coeff$stderr)
  coeff$OR <-  exp(coeff$OR)
  coeff$L95 <-  exp(coeff$L95)
  coeff$U95 <-  exp(coeff$U95)
  cols <- c("OR","stderr","zvalue","L95", "U95")
  coeff[,(cols) := round(.SD,3), .SDcols=cols]
  coeff$CI <- paste(coeff$L95, sep=", ",coeff$U95)
  coeff$CI  <- paste0("(", coeff$CI , ")")
  coeff$ORC1 <- paste(coeff$OR,coeff$CI)
  coeff$p <- gt::vec_fmt_scientific(coeff$p, decimals = 2)
  coeff <- coeff[, c("ORC1","p")]
  return(coeff)
}  #process.model(mod.ced2, c(2,3))
