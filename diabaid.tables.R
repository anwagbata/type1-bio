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
#'  This script contains all the data table outputs for these analysis
#'

setProject("type1bio")
setTopic("type1bio/src/otherautoim/")
library(data.table)

##------------------------------------------------------------------------------
## Table 1 - Drugs for Psoriasis
##------------------------------------------------------------------------------

table1 <-  data.table(Drug = c(paste0("Acitretin"),
                               paste0("Apremilast"),
                               paste0("Biologics"),
                               paste0("Calcipotriol"),
                               paste0("Calcitriol"),
                               paste0("Clobestasol"),
                               paste0("Coal tar"),
                               paste0("Dithranol"),
                               paste0("Hydrocortisone"),
                               paste0("Methotrexate"),
                               paste0("Pimecrolimus"),
                               paste0("Tacalcitol"),
                               paste0("Tacrolimus"),
                               paste0("Tazarotene"),
                               paste0("Triamcinolone")),
                      
                      Usage = c(paste0("Psoriasis, Keratosis follicularis, Congenital ichthyosis"),
                                paste0("Psoriasis and Psoriathic arthritis"),
                                paste0("Several"),
                                paste0("Psoriasis"),
                                paste0("Psoriasis, Renal osteodystrophy, Postmenopausal osteoporosis"),
                                paste0("Psoriasis, Eczema and other skin disorders"),
                                paste0("Psoriasis and Eczema"),
                                paste0("Psoriasis"),
                                paste0("Several"),
                                paste0("Several"),
                                paste0("Psoriasis and Eczema "),
                                paste0("Psoriasis"),
                                paste0("Psoriasis and Eczema"),
                                paste0("Psoriasis and Acne"),
                                paste0("Several")),
                      
                      Included = c(paste0("No"),
                                   paste0("No"),
                                   paste0("No"),
                                   paste0("Yes"),
                                   paste0("Yes"),
                                   paste0("No"),
                                   paste0("No"),
                                   paste0("Yes"),
                                   paste0("No"),
                                   paste0("No"),
                                   paste0("No"),
                                   paste0("Yes"),
                                   paste0("No"),
                                  paste0("Yes"),
                                  paste0("No")),
                      
                      Reason = c(paste0("All skin disorders and with the same prescription"),
                                 paste0("Requires attending the same specialty, same prescription"),
                                 paste0("Several usage"),
                                 paste0("Psoriasis only"),
                                 paste0("Distinguished by application method which is as ointment for psoriasis and as capsules for other disorders"),
                                 paste0("The same mode of application and same specialty attendance"),
                                 paste0("Both skin disorders, same specialty attendance and both requires skin application"),
                                 paste0("Psoriasis only"),
                                 paste0("Several usage which requires skin application"),
                                 paste0("Used less often for psoriasis and used for several traits"),
                                 paste0("Both skin disorders, same specialty attendance and both requires skin application"), 
                                 paste0("Psoriasis only"), 
                                 paste0("Both skin disorders, same specialty attendance and both requires skin application"), 
                                 paste0("Psoriasis only recorded on NICE BNF"), 
                                 paste0("Several usage")))

save(table1, file="table1.RData")

##------------------------------------------------------------------------------
## Table 2 - Drugs for IBD
##------------------------------------------------------------------------------
table2 <- data.table(Drug = c(paste0("Azathioprine"),
                              paste0("Budesonide"),
                              paste0("Ciclosporin"),
                              paste0("Mesalazine"),
                              paste0("Mercaptopurine"),
                              paste0("Methotrexate"),
                              paste0("Olsalazine"),
                              paste0("Prednisolone"),
                              paste0("Sulfasalazine"),
                              paste0("Metronidazole"),
                              paste0("Ciprofloxacin")),
                     
                     Usage = c(paste0("RA, Lupus, Eczema, Myasthenia gravis, Supression of transplant rejection"),
                               paste0("Asthma, Microscopic colitis, Autoimmune hepatitis, Eosinophilic oesophagitis"),
                               paste0("RA, Eczema, Lupus, Psoriasis, Nephrotic syndrome, keratitis, organ and bone marrow transplant."),
                               paste0("IBD only"),
                               paste0("Lymphocytic leukemia, IBD"),
                               paste0("RA, JIA, PsA, Lupus"),
                               paste0("Ulcerative colitis"),
                               paste0("Several usage"),
                               paste0("RA, PsA, IBD"),
                               paste0("Several usage"),
                               paste0("Several usage")),
                     
                     Included = c(paste0("Yes"),
                                  paste0("Yes"),
                                  paste0("Yes"),
                                  paste0("Yes"),
                                  paste0("Yes"),
                                  paste0("Yes"),
                                  paste0("Yes"),
                                  paste0("Yes"),
                                  paste0("Yes"),
                                  paste0("No"),
                                  paste0("No")),
                     
                     Reason = c(paste0("Must have attended gastroenterology specialty more than once and have no Dermatology and Rheumatology record or be self-reported Psoriasis or RA"),
                                paste0("Must have attended gastroenterology specialty more than once with Budesonide record including Enema or Foam"),
                                paste0("Must have attended gastroenterology specialty more than once and have no Dermatology and Rheumatology record or be self-reported Psoriasis or RA"),
                                paste0("IBD only"),
                                paste0("Must have attended gastroenterology specialty more than once"),
                                paste0("Must have attended gastroenterology specialty more than once and have no Dermatology and Rheumatology record or be self-reported Psoriasis or RA"),
                                paste0("Must have attended gastroenterology specialty more than once"),
                                paste0("Must have attended gastroenterology specialty more than once with prednisolone record including Enema or Foam"),
                                paste0("Must have attended gastroenterology specialty more than once and have no Dermatology and Rheumatology record or be self-reported Psoriasis or RA"),
                                paste0("Several usage"),
                                paste0("Several usage") ))

save(table2, file="table2.RData")

##------------------------------------------------------------------------------
## Table 3 - Drugs for RA
##------------------------------------------------------------------------------
table3 <- data.table(Drug = c(paste0("Biologics"),
                              paste0("Hydroxychloroquine"),
                              paste0("Leflunomide"),
                              paste0("Methotrexate"),
                              paste0("Sulfasalazine")),
                     
                     Usage = c(paste0("Several"),
                               paste0("RA and Lupus"),
                               paste0("RA and Psoriatic arthritis"),
                               paste0("RA, IBD, JIA, PsAand Lupus"),
                               paste0("RA, IBD and PsA")),
                     
                     Included = c(paste0("No"),
                                  paste0("Yes"),
                                  paste0("Yes"),
                                  paste0("Yes"),
                                  paste0("Yes")),
                     
                     Reason = c(paste0("Several usage"),
                                paste0("Requires ame specialty, included to ascertain the percentage of self-reported and hospital discharge cases on it. Excluded Hydroxychloroquine only or hydroxychloroquine and methotrexate only records in additional checks"),
                                paste0("Must have attended Rheumatology more than once and have no Dermatology and Gastroenterology specialty or be self-reported Psoriasis or IBD"),
                                paste0("Requires ame specialty, included to ascertain the percentage of self-reported and hospital discharge cases on it. Excluded Methotrexate only or hydroxychloroquine and methotrexate only records in additional checks"),
                                paste0("Must have attended Rheumatology more than once and have no Dermatology and Gastroenterology specialty or be self-reported Psoriasis or IBD")) )
                     
save(table3, file="table3.RData")

##------------------------------------------------------------------------------
## Table 4 - Summary table
##------------------------------------------------------------------------------

source("baase.analysis.R")

table4 <-  data.table(Trait = c(paste0("Celiac"),
                                paste0("Psoriasis"),
                                paste0("IBD"),
                                paste0("Rheumatoid"),
                                paste0("Hypothyroidism"),
                                paste0("Pern anaemia")
                                ),
                      
                      SR = c(paste0(ced$Freq), 
                             paste0(ps$Freq),
                             paste0(ibd$Freq), 
                             paste0(ras$Freq), 
                             paste0(htds$Freq),
                             paste0(pa$Freq)
                             ),
                      
                      HDICD = c(paste0(nrow(ced.diag.unq.id)),
                             paste0(nrow(ps.diag.unq.id)),
                             paste0(nrow(ibd.diag.unq.id)),
                             paste0(nrow(ra.diag.unq.id)),
                             paste0(nrow(htd.diag.unq.id)),
                             paste0(nrow(pa.diag.unq.id))
                             ),
                      
                      meedxspe = c(paste0("-"),
                                paste0(nrow(spes.drugs.unq.id)),
                                paste0(nrow(gtt.drugs.unq.id)),
                                paste0(nrow(rhrr.drugs.unq.id)),
                                paste0(nrow(edd.drugs.unq.id)),
                                paste0(nrow(spegh.drugs.unq.id))
                                ),
                      
                      exc.crit.medsxspe = c(paste0("-"),
                                            paste0("-"),
                                            paste0(nrow(ibdd.drugs.unq.id.chkd)),
                                            paste0(nrow(rhrr.drugs.unq.idchkd)),
                                            paste0("-"),
                                            paste0("-")
                                            ),
                      
                      nocases = c(paste0(nrow(ced.cases)),
                                       paste0(nrow(ps.cases)),
                                       paste0(nrow(ibd.cases)),
                                       paste0(nrow(ra.cases)),
                                       paste0(nrow(htd.cases)),
                                       paste0(nrow(pa.cases))
                                  ),
                      
               nocasesgenexcl = c(paste0(nrow(samp.scores[samp.scores$celiac==1])),
                                  paste0(nrow(samp.scores[samp.scores$psoriasis==1])),
                                  paste0(nrow(samp.scores[samp.scores$ibd==1])),
                                  paste0(nrow(samp.scores[samp.scores$rheumatoid==1])),
                                  paste0(nrow(samp.scores[samp.scores$htd==1])),
                                  paste0(nrow(samp.scores[samp.scores$pernamia==1]))
                                  ) )

colnames(table4) <- c("Trait","Self reported","Hospital discharge","Medication and specialty>1",
                      "Medication based exclusion", "Distinctive cases", "Final no of cases")
save(table4, file="table4.RData")

##------------------------------------------------------------------------------
## Table 5 - Sample characteristics celiac
##------------------------------------------------------------------------------
source("base.analysis.R")

table5 <-  data.table(Characteristics = c(paste0("Sex,", "  ", "n(%)"),
                                          "Male", "Female",
                                paste0("Age (yrs),", "  ", "Mean(SD)"),
                                paste0("BMI (kg/m^2),", "  ", "Mean(SD)"),
                                paste0("C-peptide level,", "  ", "Mean(SD)"),
                                paste0("Duration of diabetes (yrs),", "  ", "Mean(SD)"),
                                paste0("Age of onset (yrs),", "  ", "Mean(SD)"),
                                paste0("Age of onset category,", "  ", "n(%)"),
                                      "<16", "16-25","25-40",">40",
                                paste0("Smoking,", "  ", "n(%)"),
                                       "Current Smoker","Ex-Smoker","Never Smoked","Unknown",
                                paste0("Alcohol intake,", "  ", "n(%)"),
                                       "Yes", "No", "unknown",
                                paste0("Ketoacidosis,", "  ", "n(%)"),
                                      "Yes", "No",
                                paste0("CVD,", "  ", "n(%)"),
                                        "Yes", "No"),
                      
                  Celiac = c("",
                                  paste0(gen.cs$gender.N[2], " (", round(genp.cs$gender.N[2],1), ")"), 
                                  paste0(gen.cs$gender.N[1]," (", round(genp.cs$gender.N[1],1), ")"),
                                paste0(round(cont.cs$mean[1],1), " (", round(cont.cs$sd[1],1), ")"),
                                paste0(bmiav.cs, " (", bmisd.cs, ")"),
                                paste0(cpepav.cs, " (", cpepsd.cs, ")"),
                                paste0(round(cont.cs$mean[3],1), " (", round(cont.cs$sd[3],1), ")"),
                                paste0(round(cont.cs$mean[2],1), " (", round(cont.cs$sd[2],1), ")"),
                                "",
                                   paste0(agct.cs$ageofonsetCat.N[1], " (", round(agctp.cs$ageofonsetCat.N[1],1), ")"),
                                   paste0(agct.cs$ageofonsetCat.N[2], " (", round(agctp.cs$ageofonsetCat.N[2],1), ")"),
                                   paste0(agct.cs$ageofonsetCat.N[3], " (", round(agctp.cs$ageofonsetCat.N[3],1), ")"),
                                   paste0(agct.cs$ageofonsetCat.N[4], " (", round(agctp.cs$ageofonsetCat.N[4],1), ")"),
                                "",
                                   paste0(smk.cs$smokingstatus.N[1], " (", round(smkp.cs$smokingstatus.N[1],1), ")"), 
                                   paste0(smk.cs$smokingstatus.N[2], " (", round(smkp.cs$smokingstatus.N[2],1), ")"), 
                                   paste0(smk.cs$smokingstatus.N[3], " (", round(smkp.cs$smokingstatus.N[3],1), ")"), 
                                   paste0(smk.cs$smokingstatus.N[4], " (", round(smkp.cs$smokingstatus.N[4],1), ")"), 
                                "",
                                  paste0(alc.cs$drinkalcohol.N[3], " (", round(alcp.cs$drinkalcohol.N[3],1), ")"), 
                                  paste0(alc.cs$drinkalcohol.N[1], " (", round(alcp.cs$drinkalcohol.N[1],1), ")"), 
                                  paste0(alc.cs$drinkalcohol.N[2], " (", round(alcp.cs$drinkalcohol.N[2],1), ")"),
                                "",
                                  paste0(gen.cs$keto.N[2], " (", round(genp.cs$keto.N[2],1), ")"), 
                                  paste0(gen.cs$keto.N[1], " (", round(genp.cs$keto.N[1],1), ")"), 
                                "",
                                  paste0(gen.cs$cad.N[2], " (", round(genp.cs$cad.N[2],1), ")"), 
                                  paste0(gen.cs$cad.N[1], " (", round(genp.cs$cad.N[1],1), ")")),
                  
                  Control = c("",
                             paste0(gen.ct$gender.N[2], " (", round(genp.ct$gender.N[2],1), ")"), 
                             paste0(gen.ct$gender.N[1]," (", round(genp.ct$gender.N[1],1), ")"),
                             paste0(round(cont.ct$mean[1],1), " (", round(cont.ct$sd[1],1), ")"),
                             paste0(bmiav.ct, " (", bmisd.ct, ")"),
                             paste0(cpepav.ct, " (", cpepsd.ct, ")"),
                             paste0(round(cont.ct$mean[3],1), " (", round(cont.ct$sd[3],1), ")"),
                             paste0(round(cont.ct$mean[2],1), " (", round(cont.ct$sd[2],1), ")"),
                             "",
                             paste0(agct.ct$ageofonsetCat.N[1], " (", round(agctp.ct$ageofonsetCat.N[1],1), ")"),
                             paste0(agct.ct$ageofonsetCat.N[2], " (", round(agctp.ct$ageofonsetCat.N[2],1), ")"),
                             paste0(agct.ct$ageofonsetCat.N[3], " (", round(agctp.ct$ageofonsetCat.N[3],1), ")"),
                             paste0(agct.ct$ageofonsetCat.N[4], " (", round(agctp.ct$ageofonsetCat.N[4],1), ")"),
                             "",
                             paste0(smk.ct$smokingstatus.N[1], " (", round(smkp.ct$smokingstatus.N[1],1), ")"), 
                             paste0(smk.ct$smokingstatus.N[2], " (", round(smkp.ct$smokingstatus.N[2],1), ")"), 
                             paste0(smk.ct$smokingstatus.N[3], " (", round(smkp.ct$smokingstatus.N[3],1), ")"), 
                             paste0(smk.ct$smokingstatus.N[4], " (", round(smkp.ct$smokingstatus.N[4],1), ")"), 
                             "",
                             paste0(alc.ct$drinkalcohol.N[3], " (", round(alcp.ct$drinkalcohol.N[3],1), ")"), 
                             paste0(alc.ct$drinkalcohol.N[1], " (", round(alcp.ct$drinkalcohol.N[1],1), ")"), 
                             paste0(alc.ct$drinkalcohol.N[2], " (", round(alcp.ct$drinkalcohol.N[2],1), ")"),
                             "",
                             paste0(gen.ct$keto.N[2], " (", round(genp.ct$keto.N[2],1), ")"), 
                             paste0(gen.ct$keto.N[1], " (", round(genp.ct$keto.N[1],1), ")"), 
                             "",
                             paste0(gen.ct$cad.N[2], " (", round(genp.ct$cad.N[2],1), ")"), 
                             paste0(gen.ct$cad.N[1], " (", round(genp.ct$cad.N[1],1), ")")),
                      
                  p = c("",
                              "", 
                              paste0(mod.sex$p),
                              paste0(mod.ag$p),
                              paste0(mod.bmi$p),
                              paste0(mod.cpep$p),
                              paste0(mod.dud$p),
                              paste0(mod.ageon$p),
                              "",
                                "",
                              paste0(mod.agct$p[1]),
                              paste0(mod.agct$p[2]),
                              paste0(mod.agct$p[3]),
                              
                              "",
                                "",
                              paste0(mod.smk$p[1]), 
                              paste0(mod.smk$p[2]), 
                              paste0(mod.smk$p[3]), 
                              "",
                              paste0(mod.alc$p[2]),
                              "",
                              paste0(mod.alc$p[1]),
                              "",
                                "", 
                              paste0(mod.ket$p), 
                              "",
                                 "", 
                              paste0(mod.cvd$p))
                              )

colnames(table5) <- c("Characteristics","Celiac(n=179)","Control(n=4779)","p")
                      
save(table5, file="table5.RData")

##------------------------------------------------------------------------------
## Table 6 - Scores analysis Celiac
##------------------------------------------------------------------------------

table6a
table6b
save(table6a, file="table6a.RData")
save(table6b, file="table6b.RData")

##------------------------------------------------------------------------------
## Table 7 - Sample characteristics Psoriasis
##------------------------------------------------------------------------------

source("base.analysis.R")

table7 <-  data.table(Characteristics = c(paste0("Sex,", "  ", "n(%)"),
                                          "Male", "Female",
                                          paste0("Age (yrs),", "  ", "Mean(SD)"),
                                          paste0("BMI (kg/m^2),", "  ", "Mean(SD)"),
                                          paste0("C-peptide level,", "  ", "Mean(SD)"),
                                          paste0("Duration of diabetes (yrs),", "  ", "Mean(SD)"),
                                          paste0("Age of onset (yrs),", "  ", "Mean(SD)"),
                                          paste0("Age of onset category,", "  ", "n(%)"),
                                          "<16", "16-25","25-40",">40",
                                          paste0("Smoking,", "  ", "n(%)"),
                                          "Current Smoker","Ex-Smoker","Never Smoked","Unknown",
                                          paste0("Alcohol intake,", "  ", "n(%)"),
                                          "Yes", "No", "unknown",
                                          paste0("Ketoacidosis,", "  ", "n(%)"),
                                          "Yes", "No",
                                          paste0("CVD,", "  ", "n(%)"),
                                          "Yes", "No"),
                      
                      Psoriasis = c("",
                                 paste0(gen.cs$gender.N[2], " (", round(genp.cs$gender.N[2],1), ")"), 
                                 paste0(gen.cs$gender.N[1]," (", round(genp.cs$gender.N[1],1), ")"),
                                 paste0(round(cont.cs$mean[1],1), " (", round(cont.cs$sd[1],1), ")"),
                                 paste0(bmiav.cs, " (", bmisd.cs, ")"),
                                 paste0(cpepav.cs, " (", cpepsd.cs, ")"),
                                 paste0(round(cont.cs$mean[3],1), " (", round(cont.cs$sd[3],1), ")"),
                                 paste0(round(cont.cs$mean[2],1), " (", round(cont.cs$sd[2],1), ")"),
                                 "",
                                 paste0(agct.cs$ageofonsetCat.N[1], " (", round(agctp.cs$ageofonsetCat.N[1],1), ")"),
                                 paste0(agct.cs$ageofonsetCat.N[2], " (", round(agctp.cs$ageofonsetCat.N[2],1), ")"),
                                 paste0(agct.cs$ageofonsetCat.N[3], " (", round(agctp.cs$ageofonsetCat.N[3],1), ")"),
                                 paste0(agct.cs$ageofonsetCat.N[4], " (", round(agctp.cs$ageofonsetCat.N[4],1), ")"),
                                 "",
                                 paste0(smk.cs$smokingstatus.N[1], " (", round(smkp.cs$smokingstatus.N[1],1), ")"), 
                                 paste0(smk.cs$smokingstatus.N[2], " (", round(smkp.cs$smokingstatus.N[2],1), ")"), 
                                 paste0(smk.cs$smokingstatus.N[3], " (", round(smkp.cs$smokingstatus.N[3],1), ")"), 
                                 paste0(smk.cs$smokingstatus.N[4], " (", round(smkp.cs$smokingstatus.N[4],1), ")"), 
                                 "",
                                 paste0(alc.cs$drinkalcohol.N[3], " (", round(alcp.cs$drinkalcohol.N[3],1), ")"), 
                                 paste0(alc.cs$drinkalcohol.N[1], " (", round(alcp.cs$drinkalcohol.N[1],1), ")"), 
                                 paste0(alc.cs$drinkalcohol.N[2], " (", round(alcp.cs$drinkalcohol.N[2],1), ")"),
                                 "",
                                 paste0(gen.cs$keto.N[2], " (", round(genp.cs$keto.N[2],1), ")"), 
                                 paste0(gen.cs$keto.N[1], " (", round(genp.cs$keto.N[1],1), ")"), 
                                 "",
                                 paste0(gen.cs$cad.N[2], " (", round(genp.cs$cad.N[2],1), ")"), 
                                 paste0(gen.cs$cad.N[1], " (", round(genp.cs$cad.N[1],1), ")")),
                      
                      Control = c("",
                                  paste0(gen.ct$gender.N[2], " (", round(genp.ct$gender.N[2],1), ")"), 
                                  paste0(gen.ct$gender.N[1]," (", round(genp.ct$gender.N[1],1), ")"),
                                  paste0(round(cont.ct$mean[1],1), " (", round(cont.ct$sd[1],1), ")"),
                                  paste0(bmiav.ct, " (", bmisd.ct, ")"),
                                  paste0(cpepav.ct, " (", cpepsd.ct, ")"),
                                  paste0(round(cont.ct$mean[3],1), " (", round(cont.ct$sd[3],1), ")"),
                                  paste0(round(cont.ct$mean[2],1), " (", round(cont.ct$sd[2],1), ")"),
                                  "",
                                  paste0(agct.ct$ageofonsetCat.N[1], " (", round(agctp.ct$ageofonsetCat.N[1],1), ")"),
                                  paste0(agct.ct$ageofonsetCat.N[2], " (", round(agctp.ct$ageofonsetCat.N[2],1), ")"),
                                  paste0(agct.ct$ageofonsetCat.N[3], " (", round(agctp.ct$ageofonsetCat.N[3],1), ")"),
                                  paste0(agct.ct$ageofonsetCat.N[4], " (", round(agctp.ct$ageofonsetCat.N[4],1), ")"),
                                  
                                  "",
                                  paste0(smk.ct$smokingstatus.N[1], " (", round(smkp.ct$smokingstatus.N[1],1), ")"), 
                                  paste0(smk.ct$smokingstatus.N[2], " (", round(smkp.ct$smokingstatus.N[2],1), ")"), 
                                  paste0(smk.ct$smokingstatus.N[3], " (", round(smkp.ct$smokingstatus.N[3],1), ")"), 
                                  paste0(smk.ct$smokingstatus.N[4], " (", round(smkp.ct$smokingstatus.N[4],1), ")"), 
                                  "",
                                  paste0(alc.ct$drinkalcohol.N[3], " (", round(alcp.ct$drinkalcohol.N[3],1), ")"), 
                                  paste0(alc.ct$drinkalcohol.N[1], " (", round(alcp.ct$drinkalcohol.N[1],1), ")"), 
                                  paste0(alc.ct$drinkalcohol.N[2], " (", round(alcp.ct$drinkalcohol.N[2],1), ")"),
                                  "",
                                  paste0(gen.ct$keto.N[2], " (", round(genp.ct$keto.N[2],1), ")"), 
                                  paste0(gen.ct$keto.N[1], " (", round(genp.ct$keto.N[1],1), ")"), 
                                  "",
                                  paste0(gen.ct$cad.N[2], " (", round(genp.ct$cad.N[2],1), ")"), 
                                  paste0(gen.ct$cad.N[1], " (", round(genp.ct$cad.N[1],1), ")")),
                      
                      p = c("",
                            "", 
                            paste0(mod.sex$p),
                            paste0(mod.ag$p),
                            paste0(mod.bmi$p),
                            paste0(mod.cpep$p),
                            paste0(mod.dud$p),
                            paste0(mod.ageon$p),
                            "",
                            "",
                            paste0(mod.agct$p[1]),
                            paste0(mod.agct$p[2]),
                            paste0(mod.agct$p[3]),
                            "",
                            "",
                            paste0(mod.smk$p[1]), 
                            paste0(mod.smk$p[2]), 
                            paste0(mod.smk$p[3]), 
                            "",
                            paste0(mod.alc$p[2]),
                            "",
                            paste0(mod.alc$p[1]),
                            "",
                            "", 
                            paste0(mod.ket$p), 
                            "",
                            "", 
                            paste0(mod.cvd$p))
)

colnames(table7) <- c("Characteristics","Psoriasis(n=193)","Control(n=4765)","p")

save(table7, file="table7.RData")

##------------------------------------------------------------------------------
## Table 8 - Scores analysis Psoriasis
##------------------------------------------------------------------------------

table8a
table8b
save(table8a, file="table8a.RData")
save(table8b, file="table8b.RData")

##------------------------------------------------------------------------------
## Table 9 - Sample characteristics IBD
##------------------------------------------------------------------------------

source("base.analysis.R")

table9 <-  data.table(Characteristics = c(paste0("Sex,", "  ", "n(%)"),
                                          "Male", "Female",
                                          paste0("Age (yrs),", "  ", "Mean(SD)"),
                                          paste0("BMI (kg/m^2),", "  ", "Mean(SD)"),
                                          paste0("C-peptide level,", "  ", "Mean(SD)"),
                                          paste0("Duration of diabetes (yrs),", "  ", "Mean(SD)"),
                                          paste0("Age of onset (yrs),", "  ", "Mean(SD)"),
                                          paste0("Age of onset category,", "  ", "n(%)"),
                                          "<16", "16-25","25-40",">40",
                                          paste0("Smoking,", "  ", "n(%)"),
                                          "Current Smoker","Ex-Smoker","Never Smoked","Unknown",
                                          paste0("Alcohol intake,", "  ", "n(%)"),
                                          "Yes", "No", "unknown",
                                          paste0("Ketoacidosis,", "  ", "n(%)"),
                                          "Yes", "No",
                                          paste0("CVD,", "  ", "n(%)"),
                                          "Yes", "No"),
                      
                      IBD = c("",
                                    paste0(gen.cs$gender.N[2], " (", round(genp.cs$gender.N[2],1), ")"), 
                                    paste0(gen.cs$gender.N[1]," (", round(genp.cs$gender.N[1],1), ")"),
                                    paste0(round(cont.cs$mean[1],1), " (", round(cont.cs$sd[1],1), ")"),
                                    paste0(bmiav.cs, " (", bmisd.cs, ")"),
                                    paste0(cpepav.cs, " (", cpepsd.cs, ")"),
                                    paste0(round(cont.cs$mean[3],1), " (", round(cont.cs$sd[3],1), ")"),
                                    paste0(round(cont.cs$mean[2],1), " (", round(cont.cs$sd[2],1), ")"),
                                    "",
                                    paste0(agct.cs$ageofonsetCat.N[1], " (", round(agctp.cs$ageofonsetCat.N[1],1), ")"),
                                    paste0(agct.cs$ageofonsetCat.N[2], " (", round(agctp.cs$ageofonsetCat.N[2],1), ")"),
                                    paste0(agct.cs$ageofonsetCat.N[3], " (", round(agctp.cs$ageofonsetCat.N[3],1), ")"),
                                    paste0(agct.cs$ageofonsetCat.N[4], " (", round(agctp.cs$ageofonsetCat.N[4],1), ")"),
                                    "",
                                    paste0(smk.cs$smokingstatus.N[1], " (", round(smkp.cs$smokingstatus.N[1],1), ")"), 
                                    paste0(smk.cs$smokingstatus.N[2], " (", round(smkp.cs$smokingstatus.N[2],1), ")"), 
                                    paste0(smk.cs$smokingstatus.N[3], " (", round(smkp.cs$smokingstatus.N[3],1), ")"), 
                                    paste0(smk.cs$smokingstatus.N[4], " (", round(smkp.cs$smokingstatus.N[4],1), ")"), 
                                    "",
                                    paste0(alc.cs$drinkalcohol.N[3], " (", round(alcp.cs$drinkalcohol.N[3],1), ")"), 
                                    paste0(alc.cs$drinkalcohol.N[1], " (", round(alcp.cs$drinkalcohol.N[1],1), ")"), 
                                    paste0(alc.cs$drinkalcohol.N[2], " (", round(alcp.cs$drinkalcohol.N[2],1), ")"),
                                    "",
                                    paste0(gen.cs$keto.N[2], " (", round(genp.cs$keto.N[2],1), ")"), 
                                    paste0(gen.cs$keto.N[1], " (", round(genp.cs$keto.N[1],1), ")"), 
                                    "",
                                    paste0(gen.cs$cad.N[2], " (", round(genp.cs$cad.N[2],1), ")"), 
                                    paste0(gen.cs$cad.N[1], " (", round(genp.cs$cad.N[1],1), ")")),
                      
                      Control = c("",
                                  paste0(gen.ct$gender.N[2], " (", round(genp.ct$gender.N[2],1), ")"), 
                                  paste0(gen.ct$gender.N[1]," (", round(genp.ct$gender.N[1],1), ")"),
                                  paste0(round(cont.ct$mean[1],1), " (", round(cont.ct$sd[1],1), ")"),
                                  paste0(bmiav.ct, " (", bmisd.ct, ")"),
                                  paste0(cpepav.ct, " (", cpepsd.ct, ")"),
                                  paste0(round(cont.ct$mean[3],1), " (", round(cont.ct$sd[3],1), ")"),
                                  paste0(round(cont.ct$mean[2],1), " (", round(cont.ct$sd[2],1), ")"),
                                  "",
                                  paste0(agct.ct$ageofonsetCat.N[1], " (", round(agctp.ct$ageofonsetCat.N[1],1), ")"),
                                  paste0(agct.ct$ageofonsetCat.N[2], " (", round(agctp.ct$ageofonsetCat.N[2],1), ")"),
                                  paste0(agct.ct$ageofonsetCat.N[3], " (", round(agctp.ct$ageofonsetCat.N[3],1), ")"),
                                  paste0(agct.ct$ageofonsetCat.N[4], " (", round(agctp.ct$ageofonsetCat.N[4],1), ")"),
                                  "",
                                  paste0(smk.ct$smokingstatus.N[1], " (", round(smkp.ct$smokingstatus.N[1],1), ")"), 
                                  paste0(smk.ct$smokingstatus.N[2], " (", round(smkp.ct$smokingstatus.N[2],1), ")"), 
                                  paste0(smk.ct$smokingstatus.N[3], " (", round(smkp.ct$smokingstatus.N[3],1), ")"), 
                                  paste0(smk.ct$smokingstatus.N[4], " (", round(smkp.ct$smokingstatus.N[4],1), ")"), 
                                  "",
                                  paste0(alc.ct$drinkalcohol.N[3], " (", round(alcp.ct$drinkalcohol.N[3],1), ")"), 
                                  paste0(alc.ct$drinkalcohol.N[1], " (", round(alcp.ct$drinkalcohol.N[1],1), ")"), 
                                  paste0(alc.ct$drinkalcohol.N[2], " (", round(alcp.ct$drinkalcohol.N[2],1), ")"),
                                  "",
                                  paste0(gen.ct$keto.N[2], " (", round(genp.ct$keto.N[2],1), ")"), 
                                  paste0(gen.ct$keto.N[1], " (", round(genp.ct$keto.N[1],1), ")"), 
                                  "",
                                  paste0(gen.ct$cad.N[2], " (", round(genp.ct$cad.N[2],1), ")"), 
                                  paste0(gen.ct$cad.N[1], " (", round(genp.ct$cad.N[1],1), ")")),
                      
                      p = c("",
                            "", 
                            paste0(mod.sex$p),
                            paste0(mod.ag$p),
                            paste0(mod.bmi$p),
                            paste0(mod.cpep$p),
                            paste0(mod.dud$p),
                            paste0(mod.ageon$p),
                            "",
                            "",
                            paste0(mod.agct$p[1]),
                            paste0(mod.agct$p[2]),
                            paste0(mod.agct$p[3]),
                            "",
                            "",
                            paste0(mod.smk$p[1]), 
                            paste0(mod.smk$p[2]), 
                            paste0(mod.smk$p[3]), 
                            "",
                            paste0(mod.alc$p[2]),
                            "",
                            paste0(mod.alc$p[1]),
                            "",
                            "", 
                            paste0(mod.ket$p), 
                            "",
                            "", 
                            paste0(mod.cvd$p))
)

colnames(table9) <- c("Characteristics","IBD(n=187)","Control(n=4771)","p")

save(table9, file="table9.RData")

##------------------------------------------------------------------------------
## Table 10 - Scores analysis IBD
##------------------------------------------------------------------------------

table10a
table10b
save(table10a, file="table10a.RData")
save(table10b, file="table10b.RData")

##------------------------------------------------------------------------------
## Table 11 - Sample characteristics RA
##------------------------------------------------------------------------------

source("base.analysis.R")

table11 <-  data.table(Characteristics = c(paste0("Sex,", "  ", "n(%)"),
                                           "Male", "Female",
                                           paste0("Age (yrs),", "  ", "Mean(SD)"),
                                           paste0("BMI (kg/m^2),", "  ", "Mean(SD)"),
                                           paste0("C-peptide level,", "  ", "Mean(SD)"),
                                           paste0("Duration of diabetes (yrs),", "  ", "Mean(SD)"),
                                           paste0("Age of onset (yrs),", "  ", "Mean(SD)"),
                                           paste0("Age of onset category,", "  ", "n(%)"),
                                           "<16", "16-25","25-40",">40",
                                           paste0("Smoking,", "  ", "n(%)"),
                                           "Current Smoker","Ex-Smoker","Never Smoked","Unknown",
                                           paste0("Alcohol intake,", "  ", "n(%)"),
                                           "Yes", "No", "unknown",
                                           paste0("Ketoacidosis,", "  ", "n(%)"),
                                           "Yes", "No",
                                           paste0("CVD,", "  ", "n(%)"),
                                           "Yes", "No"),
                      
                      Rheumatoid = c("",
                              paste0(gen.cs$gender.N[2], " (", round(genp.cs$gender.N[2],1), ")"), 
                              paste0(gen.cs$gender.N[1]," (", round(genp.cs$gender.N[1],1), ")"),
                              paste0(round(cont.cs$mean[1],1), " (", round(cont.cs$sd[1],1), ")"),
                              paste0(bmiav.cs, " (", bmisd.cs, ")"),
                              paste0(cpepav.cs, " (", cpepsd.cs, ")"),
                              paste0(round(cont.cs$mean[3],1), " (", round(cont.cs$sd[3],1), ")"),
                              paste0(round(cont.cs$mean[2],1), " (", round(cont.cs$sd[2],1), ")"),
                              "",
                              paste0(agct.cs$ageofonsetCat.N[1], " (", round(agctp.cs$ageofonsetCat.N[1],1), ")"),
                              paste0(agct.cs$ageofonsetCat.N[2], " (", round(agctp.cs$ageofonsetCat.N[2],1), ")"),
                              paste0(agct.cs$ageofonsetCat.N[3], " (", round(agctp.cs$ageofonsetCat.N[3],1), ")"),
                              paste0(agct.cs$ageofonsetCat.N[4], " (", round(agctp.cs$ageofonsetCat.N[4],1), ")"),
                              "",
                              paste0(smk.cs$smokingstatus.N[1], " (", round(smkp.cs$smokingstatus.N[1],1), ")"), 
                              paste0(smk.cs$smokingstatus.N[2], " (", round(smkp.cs$smokingstatus.N[2],1), ")"), 
                              paste0(smk.cs$smokingstatus.N[3], " (", round(smkp.cs$smokingstatus.N[3],1), ")"), 
                              paste0(smk.cs$smokingstatus.N[4], " (", round(smkp.cs$smokingstatus.N[4],1), ")"), 
                              "",
                              paste0(alc.cs$drinkalcohol.N[3], " (", round(alcp.cs$drinkalcohol.N[3],1), ")"), 
                              paste0(alc.cs$drinkalcohol.N[1], " (", round(alcp.cs$drinkalcohol.N[1],1), ")"), 
                              paste0(alc.cs$drinkalcohol.N[2], " (", round(alcp.cs$drinkalcohol.N[2],1), ")"),
                              "",
                              paste0(gen.cs$keto.N[2], " (", round(genp.cs$keto.N[2],1), ")"), 
                              paste0(gen.cs$keto.N[1], " (", round(genp.cs$keto.N[1],1), ")"), 
                              "",
                              paste0(gen.cs$cad.N[2], " (", round(genp.cs$cad.N[2],1), ")"), 
                              paste0(gen.cs$cad.N[1], " (", round(genp.cs$cad.N[1],1), ")")),
                      
                      Control = c("",
                                  paste0(gen.ct$gender.N[2], " (", round(genp.ct$gender.N[2],1), ")"), 
                                  paste0(gen.ct$gender.N[1]," (", round(genp.ct$gender.N[1],1), ")"),
                                  paste0(round(cont.ct$mean[1],1), " (", round(cont.ct$sd[1],1), ")"),
                                  paste0(bmiav.ct, " (", bmisd.ct, ")"),
                                  paste0(cpepav.ct, " (", cpepsd.ct, ")"),
                                  paste0(round(cont.ct$mean[3],1), " (", round(cont.ct$sd[3],1), ")"),
                                  paste0(round(cont.ct$mean[2],1), " (", round(cont.ct$sd[2],1), ")"),
                                  "",
                                  paste0(agct.ct$ageofonsetCat.N[1], " (", round(agctp.ct$ageofonsetCat.N[1],1), ")"),
                                  paste0(agct.ct$ageofonsetCat.N[2], " (", round(agctp.ct$ageofonsetCat.N[2],1), ")"),
                                  paste0(agct.ct$ageofonsetCat.N[3], " (", round(agctp.ct$ageofonsetCat.N[3],1), ")"),
                                  paste0(agct.ct$ageofonsetCat.N[4], " (", round(agctp.ct$ageofonsetCat.N[4],1), ")"),
                                  "",
                                  paste0(smk.ct$smokingstatus.N[1], " (", round(smkp.ct$smokingstatus.N[1],1), ")"), 
                                  paste0(smk.ct$smokingstatus.N[2], " (", round(smkp.ct$smokingstatus.N[2],1), ")"), 
                                  paste0(smk.ct$smokingstatus.N[3], " (", round(smkp.ct$smokingstatus.N[3],1), ")"), 
                                  paste0(smk.ct$smokingstatus.N[4], " (", round(smkp.ct$smokingstatus.N[4],1), ")"), 
                                  "",
                                  paste0(alc.ct$drinkalcohol.N[3], " (", round(alcp.ct$drinkalcohol.N[3],1), ")"), 
                                  paste0(alc.ct$drinkalcohol.N[1], " (", round(alcp.ct$drinkalcohol.N[1],1), ")"), 
                                  paste0(alc.ct$drinkalcohol.N[2], " (", round(alcp.ct$drinkalcohol.N[2],1), ")"),
                                  "",
                                  paste0(gen.ct$keto.N[2], " (", round(genp.ct$keto.N[2],1), ")"), 
                                  paste0(gen.ct$keto.N[1], " (", round(genp.ct$keto.N[1],1), ")"), 
                                  "",
                                  paste0(gen.ct$cad.N[2], " (", round(genp.ct$cad.N[2],1), ")"), 
                                  paste0(gen.ct$cad.N[1], " (", round(genp.ct$cad.N[1],1), ")")),
                      
                      p = c("",
                            "", 
                            paste0(mod.sex$p),
                            paste0(mod.ag$p),
                            paste0(mod.bmi$p),
                            paste0(mod.cpep$p),
                            paste0(mod.dud$p),
                            paste0(mod.ageon$p),
                            "",
                            "",
                            paste0(mod.agct$p[1]),
                            paste0(mod.agct$p[2]),
                            paste0(mod.agct$p[3]),

                            "",
                            "",
                            paste0(mod.smk$p[1]), 
                            paste0(mod.smk$p[2]), 
                            paste0(mod.smk$p[3]), 
                            "",
                            paste0(mod.alc$p[2]),
                            "",
                            paste0(mod.alc$p[1]),
                            "",
                            "", 
                            paste0(mod.ket$p), 
                            "",
                            "", 
                            paste0(mod.cvd$p))
)

colnames(table11) <- c("Characteristics","Rheumatoid(n=184)","Control(n=4774)","p")

save(table11, file="table11.RData")

##------------------------------------------------------------------------------
## Table 12 - Scores analysis RA
##------------------------------------------------------------------------------

table12a
table12b
save(table12a, file="table12a.RData")
save(table12b, file="table12b.RData") 

##------------------------------------------------------------------------------
## Table 13 - Sample characteristics HTD
##------------------------------------------------------------------------------

source("base.analysis.R")

table13 <-  data.table(Characteristics = c(paste0("Sex,", "  ", "n(%)"),
                                           "Male", "Female",
                                           paste0("Age (yrs),", "  ", "Mean(SD)"),
                                           paste0("BMI (kg/m^2),", "  ", "Mean(SD)"),
                                           paste0("C-peptide level,", "  ", "Mean(SD)"),
                                           paste0("Duration of diabetes (yrs),", "  ", "Mean(SD)"),
                                           paste0("Age of onset (yrs),", "  ", "Mean(SD)"),
                                           paste0("Age of onset category,", "  ", "n(%)"),
                                           "<16", "16-25","25-40",">40",
                                           paste0("Smoking,", "  ", "n(%)"),
                                           "Current Smoker","Ex-Smoker","Never Smoked","Unknown",
                                           paste0("Alcohol intake,", "  ", "n(%)"),
                                           "Yes", "No", "unknown",
                                           paste0("Ketoacidosis,", "  ", "n(%)"),
                                           "Yes", "No",
                                           paste0("CVD,", "  ", "n(%)"),
                                           "Yes", "No"),
                      
                      Hypothyroidism = c("",
                              paste0(gen.cs$gender.N[2], " (", round(genp.cs$gender.N[2],1), ")"), 
                              paste0(gen.cs$gender.N[1]," (", round(genp.cs$gender.N[1],1), ")"),
                              paste0(round(cont.cs$mean[1],1), " (", round(cont.cs$sd[1],1), ")"),
                              paste0(bmiav.cs, " (", bmisd.cs, ")"),
                              paste0(cpepav.cs, " (", cpepsd.cs, ")"),
                              paste0(round(cont.cs$mean[3],1), " (", round(cont.cs$sd[3],1), ")"),
                              paste0(round(cont.cs$mean[2],1), " (", round(cont.cs$sd[2],1), ")"),
                              "",
                              paste0(agct.cs$ageofonsetCat.N[1], " (", round(agctp.cs$ageofonsetCat.N[1],1), ")"),
                              paste0(agct.cs$ageofonsetCat.N[2], " (", round(agctp.cs$ageofonsetCat.N[2],1), ")"),
                              paste0(agct.cs$ageofonsetCat.N[3], " (", round(agctp.cs$ageofonsetCat.N[3],1), ")"),
                              paste0(agct.cs$ageofonsetCat.N[4], " (", round(agctp.cs$ageofonsetCat.N[4],1), ")"),
                              "",
                              paste0(smk.cs$smokingstatus.N[1], " (", round(smkp.cs$smokingstatus.N[1],1), ")"), 
                              paste0(smk.cs$smokingstatus.N[2], " (", round(smkp.cs$smokingstatus.N[2],1), ")"), 
                              paste0(smk.cs$smokingstatus.N[3], " (", round(smkp.cs$smokingstatus.N[3],1), ")"), 
                              paste0(smk.cs$smokingstatus.N[4], " (", round(smkp.cs$smokingstatus.N[4],1), ")"), 
                              "",
                              paste0(alc.cs$drinkalcohol.N[3], " (", round(alcp.cs$drinkalcohol.N[3],1), ")"), 
                              paste0(alc.cs$drinkalcohol.N[1], " (", round(alcp.cs$drinkalcohol.N[1],1), ")"), 
                              paste0(alc.cs$drinkalcohol.N[2], " (", round(alcp.cs$drinkalcohol.N[2],1), ")"),
                              "",
                              paste0(gen.cs$keto.N[2], " (", round(genp.cs$keto.N[2],1), ")"), 
                              paste0(gen.cs$keto.N[1], " (", round(genp.cs$keto.N[1],1), ")"), 
                              "",
                              paste0(gen.cs$cad.N[2], " (", round(genp.cs$cad.N[2],1), ")"), 
                              paste0(gen.cs$cad.N[1], " (", round(genp.cs$cad.N[1],1), ")")),
                      
                      Control = c("",
                                  paste0(gen.ct$gender.N[2], " (", round(genp.ct$gender.N[2],1), ")"), 
                                  paste0(gen.ct$gender.N[1]," (", round(genp.ct$gender.N[1],1), ")"),
                                  paste0(round(cont.ct$mean[1],1), " (", round(cont.ct$sd[1],1), ")"),
                                  paste0(bmiav.ct, " (", bmisd.ct, ")"),
                                  paste0(cpepav.ct, " (", cpepsd.ct, ")"),
                                  paste0(round(cont.ct$mean[3],1), " (", round(cont.ct$sd[3],1), ")"),
                                  paste0(round(cont.ct$mean[2],1), " (", round(cont.ct$sd[2],1), ")"),
                                  "",
                                  paste0(agct.ct$ageofonsetCat.N[1], " (", round(agctp.ct$ageofonsetCat.N[1],1), ")"),
                                  paste0(agct.ct$ageofonsetCat.N[2], " (", round(agctp.ct$ageofonsetCat.N[2],1), ")"),
                                  paste0(agct.ct$ageofonsetCat.N[3], " (", round(agctp.ct$ageofonsetCat.N[3],1), ")"),
                                  paste0(agct.ct$ageofonsetCat.N[4], " (", round(agctp.ct$ageofonsetCat.N[4],1), ")"),
                                  "",
                                  paste0(smk.ct$smokingstatus.N[1], " (", round(smkp.ct$smokingstatus.N[1],1), ")"), 
                                  paste0(smk.ct$smokingstatus.N[2], " (", round(smkp.ct$smokingstatus.N[2],1), ")"), 
                                  paste0(smk.ct$smokingstatus.N[3], " (", round(smkp.ct$smokingstatus.N[3],1), ")"), 
                                  paste0(smk.ct$smokingstatus.N[4], " (", round(smkp.ct$smokingstatus.N[4],1), ")"), 
                                  "",
                                  paste0(alc.ct$drinkalcohol.N[3], " (", round(alcp.ct$drinkalcohol.N[3],1), ")"), 
                                  paste0(alc.ct$drinkalcohol.N[1], " (", round(alcp.ct$drinkalcohol.N[1],1), ")"), 
                                  paste0(alc.ct$drinkalcohol.N[2], " (", round(alcp.ct$drinkalcohol.N[2],1), ")"),
                                  "",
                                  paste0(gen.ct$keto.N[2], " (", round(genp.ct$keto.N[2],1), ")"), 
                                  paste0(gen.ct$keto.N[1], " (", round(genp.ct$keto.N[1],1), ")"), 
                                  "",
                                  paste0(gen.ct$cad.N[2], " (", round(genp.ct$cad.N[2],1), ")"), 
                                  paste0(gen.ct$cad.N[1], " (", round(genp.ct$cad.N[1],1), ")")),
                      
                      p = c("",
                            "", 
                            paste0(mod.sex$p),
                            paste0(mod.ag$p),
                            paste0(mod.bmi$p),
                            paste0(mod.cpep$p),
                            paste0(mod.dud$p),
                            paste0(mod.ageon$p),
                            "",
                            "",
                            paste0(mod.agct$p[1]),
                            paste0(mod.agct$p[2]),
                            paste0(mod.agct$p[3]),
      
                            "",
                            "",
                            paste0(mod.smk$p[1]), 
                            paste0(mod.smk$p[2]), 
                            paste0(mod.smk$p[3]), 
                            "",
                            paste0(mod.alc$p[2]),
                            "",
                            paste0(mod.alc$p[1]),
                            "",
                            "", 
                            paste0(mod.ket$p), 
                            "",
                            "", 
                            paste0(mod.cvd$p))
)

colnames(table13) <- c("Characteristics","Hypothyroidsm(n=355)","Control(n=4603)","p")

save(table13, file="table13.RData")
                                        
##------------------------------------------------------------------------------
## Table 14 - Scores analysis Htd
##------------------------------------------------------------------------------

table14a
table14b
save(table14a, file="table14a.RData")
save(table14b, file="table14b.RData")

##------------------------------------------------------------------------------
## Table 15 - Sample characteristics Pernamia
##------------------------------------------------------------------------------

source("pernamia.analysis.R")

table15 <-  data.table(Characteristics = c(paste0("Sex,", "  ", "n(%)"),
                                           "Male", "Female",
                                           paste0("Age (yrs),", "  ", "Mean(SD)"),
                                           paste0("BMI (kg/m^2),", "  ", "Mean(SD)"),
                                           paste0("C-peptide level,", "  ", "Mean(SD)"),
                                           paste0("Duration of diabetes (yrs),", "  ", "Mean(SD)"),
                                           paste0("Age of onset (yrs),", "  ", "Mean(SD)"),
                                           paste0("Age of onset category,", "  ", "n(%)"),
                                           "<16", "16-25","25-40",">40",
                                           paste0("Smoking,", "  ", "n(%)"),
                                           "Current Smoker","Ex-Smoker","Never Smoked","Unknown",
                                           paste0("Alcohol intake,", "  ", "n(%)"),
                                           "Yes", "No", "unknown",
                                           paste0("Ketoacidosis,", "  ", "n(%)"),
                                           "Yes", "No",
                                           paste0("CVD,", "  ", "n(%)"),
                                           "Yes", "No"),
                       
                       Pernamia = c("",
                                          paste0(gen.cs$gender.N[2], " (", round(genp.cs$gender.N[2],1), ")"), 
                                          paste0(gen.cs$gender.N[1]," (", round(genp.cs$gender.N[1],1), ")"),
                                          paste0(round(cont.cs$mean[1],1), " (", round(cont.cs$sd[1],1), ")"),
                                          paste0(bmiav.cs, " (", bmisd.cs, ")"),
                                          paste0(cpepav.cs, " (", cpepsd.cs, ")"),
                                          paste0(round(cont.cs$mean[3],1), " (", round(cont.cs$sd[3],1), ")"),
                                          paste0(round(cont.cs$mean[2],1), " (", round(cont.cs$sd[2],1), ")"),
                                          "",
                                          paste0(agct.cs$ageofonsetCat.N[1], " (", round(agctp.cs$ageofonsetCat.N[1],1), ")"),
                                          paste0(agct.cs$ageofonsetCat.N[2], " (", round(agctp.cs$ageofonsetCat.N[2],1), ")"),
                                          paste0(agct.cs$ageofonsetCat.N[3], " (", round(agctp.cs$ageofonsetCat.N[3],1), ")"),
                                          paste0(agct.cs$ageofonsetCat.N[4], " (", round(agctp.cs$ageofonsetCat.N[4],1), ")"),
                                          "",
                                          paste0(smk.cs$smokingstatus.N[1], " (", round(smkp.cs$smokingstatus.N[1],1), ")"), 
                                          paste0(smk.cs$smokingstatus.N[2], " (", round(smkp.cs$smokingstatus.N[2],1), ")"), 
                                          paste0(smk.cs$smokingstatus.N[3], " (", round(smkp.cs$smokingstatus.N[3],1), ")"), 
                                          paste0(smk.cs$smokingstatus.N[4], " (", round(smkp.cs$smokingstatus.N[4],1), ")"), 
                                          "",
                                          paste0(alc.cs$drinkalcohol.N[3], " (", round(alcp.cs$drinkalcohol.N[3],1), ")"), 
                                          paste0(alc.cs$drinkalcohol.N[1], " (", round(alcp.cs$drinkalcohol.N[1],1), ")"), 
                                          paste0(alc.cs$drinkalcohol.N[2], " (", round(alcp.cs$drinkalcohol.N[2],1), ")"),
                                          "",
                                          paste0(gen.cs$keto.N[2], " (", round(genp.cs$keto.N[2],1), ")"), 
                                          paste0(gen.cs$keto.N[1], " (", round(genp.cs$keto.N[1],1), ")"), 
                                          "",
                                          paste0(gen.cs$cad.N[2], " (", round(genp.cs$cad.N[2],1), ")"), 
                                          paste0(gen.cs$cad.N[1], " (", round(genp.cs$cad.N[1],1), ")")),
                       
                       Control = c("",
                                   paste0(gen.ct$gender.N[2], " (", round(genp.ct$gender.N[2],1), ")"), 
                                   paste0(gen.ct$gender.N[1]," (", round(genp.ct$gender.N[1],1), ")"),
                                   paste0(round(cont.ct$mean[1],1), " (", round(cont.ct$sd[1],1), ")"),
                                   paste0(bmiav.ct, " (", bmisd.ct, ")"),
                                   paste0(cpepav.ct, " (", cpepsd.ct, ")"),
                                   paste0(round(cont.ct$mean[3],1), " (", round(cont.ct$sd[3],1), ")"),
                                   paste0(round(cont.ct$mean[2],1), " (", round(cont.ct$sd[2],1), ")"),
                                   "",
                                   paste0(agct.ct$ageofonsetCat.N[1], " (", round(agctp.ct$ageofonsetCat.N[1],1), ")"),
                                   paste0(agct.ct$ageofonsetCat.N[2], " (", round(agctp.ct$ageofonsetCat.N[2],1), ")"),
                                   paste0(agct.ct$ageofonsetCat.N[3], " (", round(agctp.ct$ageofonsetCat.N[3],1), ")"),
                                   paste0(agct.ct$ageofonsetCat.N[4], " (", round(agctp.ct$ageofonsetCat.N[4],1), ")"),
                                   "",
                                   paste0(smk.ct$smokingstatus.N[1], " (", round(smkp.ct$smokingstatus.N[1],1), ")"), 
                                   paste0(smk.ct$smokingstatus.N[2], " (", round(smkp.ct$smokingstatus.N[2],1), ")"), 
                                   paste0(smk.ct$smokingstatus.N[3], " (", round(smkp.ct$smokingstatus.N[3],1), ")"), 
                                   paste0(smk.ct$smokingstatus.N[4], " (", round(smkp.ct$smokingstatus.N[4],1), ")"), 
                                   "",
                                   paste0(alc.ct$drinkalcohol.N[3], " (", round(alcp.ct$drinkalcohol.N[3],1), ")"), 
                                   paste0(alc.ct$drinkalcohol.N[1], " (", round(alcp.ct$drinkalcohol.N[1],1), ")"), 
                                   paste0(alc.ct$drinkalcohol.N[2], " (", round(alcp.ct$drinkalcohol.N[2],1), ")"),
                                   "",
                                   paste0(gen.ct$keto.N[2], " (", round(genp.ct$keto.N[2],1), ")"), 
                                   paste0(gen.ct$keto.N[1], " (", round(genp.ct$keto.N[1],1), ")"), 
                                   "",
                                   paste0(gen.ct$cad.N[2], " (", round(genp.ct$cad.N[2],1), ")"), 
                                   paste0(gen.ct$cad.N[1], " (", round(genp.ct$cad.N[1],1), ")")),
                       
                       p = c("",
                             "", 
                             paste0(mod.sex$p),
                             paste0(mod.ag$p),
                             paste0(mod.bmi$p),
                             paste0(mod.cpep$p),
                             paste0(mod.dud$p),
                             paste0(mod.ageon$p),
                             "",
                             "",
                             paste0(mod.agct$p[1]),
                             paste0(mod.agct$p[2]),
                             paste0(mod.agct$p[3]),
                             
                             "",
                             "",
                             paste0(mod.smk$p[1]), 
                             paste0(mod.smk$p[2]), 
                             paste0(mod.smk$p[3]), 
                             "",
                             paste0(mod.alc$p[2]),
                             "",
                             paste0(mod.alc$p[1]),
                             "",
                             "", 
                             paste0(mod.ket$p), 
                             "",
                             "", 
                             paste0(mod.cvd$p))
)

colnames(table15) <- c("Characteristics","Pern anaemia(n=122)","Control(n=4836)","p")
save(table15, file="table15.RData")

##------------------------------------------------------------------------------
## Table 16 - Scores analysis Pernamia
##------------------------------------------------------------------------------

table16a
table16b
save(table16a, file="table16a.RData")
save(table16b, file="table16b.RData")

##------------------------------------------------------------------------------
## Table 17 - Adjusted and unadjusted age of onset
##------------------------------------------------------------------------------

source("base.analysis.R")
source("assoc.functions.R")
source("ageofonset.R")

table17 <-  data.table(Trait = c(paste0("Celiac (study date)"),
                                 paste0("Celiac"),
                                 paste0("Psoriasis"),
                                 paste0("IBD"),
                                 paste0("Rheumatoid"),
                                 paste0("Hypothyroidism"),
                                 paste0("Pernicious Anaemia")),
                       
                       ORCI = c(paste0(ced.rc1$ORC1),
                                paste0(ced1$ORC1),
                                paste0(ps1$ORC1),
                                paste0(ibd1$ORC1),
                                paste0(ra1$ORC1),
                                paste0(htd1$ORC1),
                                paste0(pa1$ORC1)),
                       
                       p =  c(paste0(ced.rc1$p),
                              paste0(ced1$p),
                              paste0(ps1$p),
                              paste0(ibd1$p),
                              paste0(ra1$p),
                              paste0(htd1$p),
                              paste0(pa1$p)),
                       
                       ORCI = c(paste0(ced.rc2$ORC1),
                                paste0(ced2$ORC1),
                                paste0(ps2$ORC1),
                                paste0(ibd2$ORC1),
                                paste0(ra2$ORC1),
                                paste0(htd2$ORC1),
                                paste0(pa2$ORC1)),
                       
                       p =  c(paste0(ced.rc2$p),
                              paste0(ced2$p),
                              paste0(ps2$p),
                              paste0(ibd2$p),
                              paste0(ra2$p),
                              paste0(htd2$p),
                              paste0(pa2$p)))

colnames(table17) <- c("Trait","Unadjusted OR", "Unadjusted p", "Adjusted OR","Adjusted p")
save(table17, file="table17.RData")

##------------------------------------------------------------------------------
## Table 18 - Predicting age of onset and c-peptide level with predicted gene expression scores of core genes
##------------------------------------------------------------------------------

source("base.analysis.R")
source("ageofonset.R")

table18 <- data.table(Trait = c(paste0("eQTL - LGALS3BP, FOXP3, STAT1"),
                                paste0("T1D age of onset"),
                                paste0("C-peptide level"),
                                paste0("pQTL - CCL19, CRTAM, CD5L, LAG3, GCG"),
                                paste0("T1D age of onset"),
                                paste0("C-peptide level")),
                      
                      Estimate = c("",
                                   paste0(round(summary(mod3)$coefficients[2,1],2), "(", round(mod3.ci[2,1],2), ",", " ", round(mod3.ci[2,2],2), ")"), 
                                   paste0(round(summary(mod4)$coefficients[2,1],2), "(", round(mod4.ci[2,1],2), ",", " ", round(mod4.ci[2,2],2), ")"), 
                                   "",
                                   paste0(round(summary(mod5)$coefficients[2,1],2), "(", round(mod5.ci[2,1],2), ",", " ", round(mod5.ci[2,2],2), ")"), 
                                   paste0(round(summary(mod6)$coefficients[2,1],2), "(", round(mod6.ci[2,1],2), ",", " ", round(mod6.ci[2,2],2), ")")),
                      
                      p = c("",
                            paste0(round(summary(mod3)$coefficients[2,4],2)),
                            paste0(round(summary(mod4)$coefficients[2,4],2)),
                            "",
                            paste0(round(summary(mod5)$coefficients[2,4],2)),
                            paste0(round(summary(mod6)$coefficients[2,4],2))))

save(table18, file="table18.RData")
