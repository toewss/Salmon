library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(scales)
library(cowplot)
library(ggpubr)
library(here)
Area=243623
ChinookW=8.5 #kg
############################## ESCAPEMENT
setwd("C:/Users/fannyc64/Sync/Chapters/Chapter 1 - Ewe/Model/Data/Chinook salmon/Chinook FG from CTC/Data CTC 2022/")
PSC_Cohort_Original<-read.csv("Data/2022_5_Couture_cohort_escapement.csv")
PSC_Cohort_Original<-subset(PSC_Cohort_Original, select = -c(AEQCohort, StockNum, Age, Cohort))
PSC_Cohort_Original<-aggregate(.~Year+Stock, data=PSC_Cohort_Original, FUN=sum)
PSC_Cohort_Original$Terminal.Run<-NULL
PSC_Cohort_Original$FG<-ifelse(PSC_Cohort_Original$Stock %in% c(" FS2"," FS3"," NKS"), "FRGSPS SP",
                               ifelse(PSC_Cohort_Original$Stock %in% c(" FSS"," FSO"," SNO", " SKG"), "FRGSPS SU",
                                      ifelse(PSC_Cohort_Original$Stock %in% c(" FCF"," FHF"," MGS"," LGS"," NKF"," PSY"," PSN"," PSF"," STL"), "FRGSPS FA",
                                             ifelse(PSC_Cohort_Original$Stock %in% c(" WVH"," WVN"), "WCVI FA",
                                                    ifelse(PSC_Cohort_Original$Stock %in% c(" CWS"," WSH"), "CRWOR SP",
                                                           ifelse(PSC_Cohort_Original$Stock %in% c(" SUM"), "CRWOR SU",
                                                                  ifelse(PSC_Cohort_Original$Stock %in% c(" WCN"," WCH"," NOC"," CWF"," MCB"," LYF"," URB"," BON", " SPR"), "CRWOR FA", 0)))))))
PSC_Cohort_Original$Stock<-NULL
PSC_Cohort_Original<-aggregate(.~Year+FG, FUN=sum, data=PSC_Cohort_Original)
# CATCH
PSC_Catch_Original1<-read.csv("2022_05_Couture_catch_mortality.csv")
PSC_Catch_Original<-PSC_Catch_Original1
PSC_Catch_Original<-subset(PSC_Catch_Original, select=-c(FishNum, StockNum,
                                                         Scaled.to.Observed.Catch,
                                                         Model.Catch.to.Observed.Catch.Ratio,
                                                         AEQ.Catch, AEQ.Shakers, AEQ.CNRSubLeg, AEQ.CNRLeg))
PSC_Catch_Original$X<-NULL
PSC_Catch_Original$Fishery<-NULL
PSC_Catch_Original$Age<-NULL
PSC_Catch_Original<-aggregate(.~Year+ Stock, data=PSC_Catch_Original, FUN=sum)
PSC_Catch_Original$Overall_Catch<-rowSums(PSC_Catch_Original[,(3:6)])
PSC_Catch_Original<-subset(PSC_Catch_Original, select=-c(Catch, Shakers, CNR.Legals, CNR.Sublegals))
PSC_Catch_Original$FG<-ifelse(PSC_Catch_Original$Stock %in% c(" FS2"," FS3"," NKS"), "FRGSPS SP",
                              ifelse(PSC_Catch_Original$Stock %in% c(" FSS"," FSO"," SNO"," SKG"), "FRGSPS SU",
                                     ifelse(PSC_Catch_Original$Stock %in% c(" FCF"," FHF"," MGS"," LGS"," NKF"," PSY"," PSN"," PSF"," STL"), "FRGSPS FA",
                                            ifelse(PSC_Catch_Original$Stock %in% c(" WVH"," WVN"), "WCVI FA",
                                                   ifelse(PSC_Catch_Original$Stock %in% c(" CWS"," WSH"), "CRWOR SP",
                                                          ifelse(PSC_Catch_Original$Stock %in% c(" SUM"), "CRWOR SU",
                                                                 ifelse(PSC_Catch_Original$Stock %in% c(" WCN"," WCH"," NOC"," CWF"," MCB"," LYF"," URB"," BON", " SPR"), "CRWOR FA", 0)))))))
View(PSC_Catch_Original)
View(PSC_Catch_Original)