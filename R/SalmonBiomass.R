#################################################################################
#################################################################################
##                     DFO MMCP SRKW ECOSYSTEM MODEL                           ##
##          Exploring and combining CHinook Salmon abundance and catch         ##
#################################################################################
#################################################################################

#Author: S. Toews (17-June-2022)
#Purpose: Exploring and summarizing salmon data from PSC 

#----Clear R environment----

rm(list = ls(all=TRUE)) ; ls()

#----Install required packages----

suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
suppressMessages(library(here))
suppressMessages(library(tibble))
suppressMessages(library(readxl))
suppressMessages(library(ggplot2))
suppressMessages(library(ggpubr))
library(readr)
library(ggridges)
library(tidyverse)
library(ggrepel)
library(ggtext)
library(showtext)
library(hrbrthemes)
library(viridis)
font_add_google("Lato")
showtext_auto()

Area=176000
ChinookW=8.5/1000

#----Read in terminal estimate data ---- 
ter<-read_csv("Data/2022_5_Couture_cohort_escapement.csv")
ter<-subset(ter, select = -c(AEQCohort, StockNum,  Cohort))
ter$Stock<-as.factor(ter$Stock)
ter$Age<-as.factor((ter$Age))
str(ter)
ter<-aggregate(.~Age+Year+Stock, data=ter, FUN=sum)
#ter<-aggregate(.~Year+Stock+Age, data=ter, FUN=sum)
ter$Terminal.Run<-NULL

#----Combine abnundance data by group and year----
str(ter)
ter2<-ter %>%
  mutate(FG = case_when(Stock == 'FS2' | Stock == 'FS3' ~ 'FRG SP',
                        Stock == 'FSS' | Stock == 'FSO' ~ 'FRG SU',
                        Stock == 'FCF' | Stock == 'FHF' ~ 'FRG FA',
                        Stock == 'MGS' | Stock =="LGS" ~ 'GST FA',
                        #Stock == 'LGS' ~ 'GST LO',
                        Stock == 'NKF' | Stock == 'PSY'|Stock == 'PSN'|Stock == 'PSF'|Stock == 'SKG'|Stock == 'SNO'|Stock == 'STL'|Stock == 'NKS'  ~ 'PSD FA',
                        Stock == 'WVN' | Stock == 'WVH' ~ 'WCV FA',
                        Stock == 'NOC'  ~ 'CAO FA',
                        Stock == 'WCN' | Stock == 'WCH' ~ 'WAC FA',
                        Stock == 'CWF' | Stock == 'MCB'|Stock == 'LYF'|Stock == 'URB'|Stock == 'BON'|Stock == 'SPR' ~ 'COR FA',
                        Stock == 'SUM'  ~ 'COR SU',
                        Stock == 'CWS' | Stock == 'WSH' ~ 'COR SP'))

ter2$FG<-as.factor((ter2$FG))
ter2$FG <-factor(ter2$FG, levels=c("FRG SP", "COR SP","FRG SU","COR SU", "FRG FA","COR FA", "GST FA","PSD FA","WCV FA","WAC FA","CAO FA" ))
str(ter2)
summary(ter2$FG)
ter2$Stock<-NULL
ter3<-aggregate(.~Year+FG+Age, FUN=sum, data=ter2)


#ter3$Age<-NULL
ter3$Year<-as.integer(ter3$Year)
str(ter3)
#view(ter3)

# Stacked

Term<-ggplot(ter3, aes(fill=Age, y=Escapement, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Chinook Terminal Runs") +
  facet_wrap(~FG)+#,scales="free_y") #+
theme_ipsum() +
  theme(legend.position="none") +
  xlab("")
Term
ggsave(Term,file="OUTPUTS/ChinookTermRun.png",width = 28, height = 12, units = "cm")



################CATCH DATA##############################################
#---Read in catch statistics----
catch<-read_csv("Data/2022_05_Couture_catch_mortality.csv")
spec(catch)
str(catch)

catch<-subset(catch, select=-c(...1,FishNum, StockNum,Scaled.to.Observed.Catch, Model.Catch.to.Observed.Catch.Ratio,
                               AEQ.Catch, AEQ.Shakers, AEQ.CNRSubLeg, AEQ.CNRLeg))
unique(catch$Fishery)

#----Remove Fisheries Outside of SRKW Summer Zone----

#catch<-catch[(catch$Fishery!="ALASKA T"),]
#catch<-catch[(catch$Fishery!="ALASKA N"),]
#catch<-catch[(catch$Fishery!="TBC TBR FN"),]
#catch<-catch[(catch$Fishery!="TAK YAK N"),]
#catch<-catch[(catch$Fishery!="ALASKA S"),]
#catch<-catch[(catch$Fishery!="TAK TBR N"),]
#catch<-catch[(catch$Fishery!="TYK YAK FN"),]
#catch<-catch[(catch$Fishery!="TAK TBR S"),]
catch$Fishery<-NULL
#catch$Age<-NULL
catch<-aggregate(.~Year+ Stock+Age, data=catch, FUN=sum)
catch$X<-NULL
str(catch)
catch$Overall_Catch<-rowSums(catch[,(4:7)])
catch2<-subset(catch, select=-c(Catch, Shakers, CNR.Legals, CNR.Sublegals))


#----Combine catch data by group and year----
str(catch2)
catch2<-catch2 %>%
  mutate(FG = case_when(Stock == 'FS2' | Stock == 'FS3' ~ 'FRG SP',
                        Stock == 'FSS' | Stock == 'FSO' ~ 'FRG SU',
                        Stock == 'FCF' | Stock == 'FHF' ~ 'FRG FA',
                        Stock == 'MGS' | Stock =="LGS" ~ 'GST FA',
                        #Stock == 'LGS' ~ 'GST LO',
                        Stock == 'NKF' | Stock == 'PSY'|Stock == 'PSN'|Stock == 'PSF'|Stock == 'SKG'|Stock == 'SNO'|Stock == 'STL'|Stock == 'NKS'  ~ 'PSD FA',
                        Stock == 'WVN' | Stock == 'WVH' ~ 'WCV FA',
                        Stock == 'NOC'  ~ 'CAO FA',
                        Stock == 'WCN' | Stock == 'WCH' ~ 'WAC FA',
                        Stock == 'CWF' | Stock == 'MCB'|Stock == 'LYF'|Stock == 'URB'|Stock == 'BON'|Stock == 'SPR' ~ 'COR FA',
                        Stock == 'SUM'  ~ 'COR SU',
                        Stock == 'CWS' | Stock == 'WSH' ~ 'COR SP'))
str(catch2)
catch2$FG<-as.factor((catch2$FG))
catch2$FG <-factor(catch2$FG, levels=c("FRG SP", "COR SP","FRG SU","COR SU", "FRG FA","COR FA", "GST FA","PSD FA","WCV FA","WAC FA","CAO FA" ))
str(catch2)

str(catch2)
summary(catch2$FG)


catch2$Stock<-NULL
catch2$X<-NULL
catch3<-aggregate(.~Year+FG+Age, FUN=sum, data=catch2)
catch3$Age<-as.factor(catch3$Age)
catch3$Year<-as.integer(catch3$Year)
str(catch3)
#view(catch3)


Cat<-  ggplot(catch3, aes(fill=Age, y=Overall_Catch, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Chinook Total Catch") +
  facet_wrap(~FG)+# ,scales="free_y") #+
theme_ipsum() +
  theme(legend.position="none") +
  xlab("")  

Cat  

ggsave(Cat,file="OUTPUTS/ChinookTotalCatch.png",width = 28, height = 12, units = "cm")




################Fisheries Mortality Estimates DATA##############################################
#---Read in FM statistics----
FM<-read_csv("Data/2022_05_Couture_catch_mortality.csv")
spec(FM)
str(FM)

FM<-subset(FM, select=-c(...1,FishNum, StockNum,Scaled.to.Observed.Catch, Model.Catch.to.Observed.Catch.Ratio,
                               AEQ.Catch, AEQ.Shakers, AEQ.CNRSubLeg, AEQ.CNRLeg))
unique(FM$Fishery)

#----Remove Fisheries Outside of SRKW Summer Zone----

FM<-FM[(FM$Fishery!="CENTRL N"),]
FM<-FM[(FM$Fishery!="WCVI N"),]
FM<-FM[(FM$Fishery!="TBC TBR FN"),]
#FM<-FM[(FM$Fishery!="J DE F N"),]
#FM<-FM[(FM$Fishery!="PGSDN N"),]
#FM<-FM[(FM$Fishery!="PGSDO N"),]
FM<-FM[(FM$Fishery!="WASH CST N"),]
FM<-FM[(FM$Fishery!="TCOL R N"),]
FM<-FM[(FM$Fishery!="TPS FN"),]
FM<-FM[(FM$Fishery!="TWAC FN"),]
#FM<-FM[(FM$Fishery!="TBC TBR FN"),]
FM<-FM[(FM$Fishery!="TCENTRAL FN"),]
FM<-FM[(FM$Fishery!="TGEO ST FN"),]
#FM<-FM[(FM$Fishery!="JNST N"),]
FM<-FM[(FM$Fishery!="TFRAS FN"),]
FM<-FM[(FM$Fishery!="TCENTRAL FS"),]
FM<-FM[(FM$Fishery!="FRASER N"),]
FM<-FM[(FM$Fishery!="TFRASER FS"),]
FM<-FM[(FM$Fishery!="TWCVI FS"),]
FM<-FM[(FM$Fishery!="TGS FS"),]
FM<-FM[(FM$Fishery!="TPS FS"),]
FM<-FM[(FM$Fishery!="TSF FS"),]
FM<-FM[(FM$Fishery!="TCOL R S"),]


FM$Fishery<-NULL
#FM$Age<-NULL
FM<-aggregate(.~Year+ Stock+Age, data=FM, FUN=sum)
FM$X<-NULL
str(FM)
FM$Overall_FM<-rowSums(FM[,(4:7)])
FM2<-subset(FM, select=-c(Catch, Shakers, CNR.Legals, CNR.Sublegals))


#----Combine FM data by group and year----
str(FM2)
FM2<-FM2 %>%
  mutate(FG = case_when(Stock == 'FS2' | Stock == 'FS3' ~ 'FRG SP',
                        Stock == 'FSS' | Stock == 'FSO' ~ 'FRG SU',
                        Stock == 'FCF' | Stock == 'FHF' ~ 'FRG FA',
                        Stock == 'MGS' | Stock =="LGS" ~ 'GST FA',
                        #Stock == 'LGS' ~ 'GST LO',
                        Stock == 'NKF' | Stock == 'PSY'|Stock == 'PSN'|Stock == 'PSF'|Stock == 'SKG'|Stock == 'SNO'|Stock == 'STL'|Stock == 'NKS'  ~ 'PSD FA',
                        Stock == 'WVN' | Stock == 'WVH' ~ 'WCV FA',
                        Stock == 'NOC'  ~ 'CAO FA',
                        Stock == 'WCN' | Stock == 'WCH' ~ 'WAC FA',
                        Stock == 'CWF' | Stock == 'MCB'|Stock == 'LYF'|Stock == 'URB'|Stock == 'BON'|Stock == 'SPR' ~ 'COR FA',
                        Stock == 'SUM'  ~ 'COR SU',
                        Stock == 'CWS' | Stock == 'WSH' ~ 'COR SP'))
str(FM2)
FM2$FG<-as.factor((FM2$FG))
FM2$FG <-factor(FM2$FG, levels=c("FRG SP", "COR SP","FRG SU","COR SU", "FRG FA","COR FA", "GST FA","PSD FA","WCV FA","WAC FA","CAO FA" ))
str(FM2)

str(FM2)
summary(FM2$FG)


FM2$Stock<-NULL
FM2$X<-NULL
FM3<-aggregate(.~Year+FG+Age, FUN=sum, data=FM2)
FM3$Age<-as.factor(FM3$Age)
FM3$Year<-as.integer(FM3$Year)
str(FM3)
#view(FM3)


Cat<-  ggplot(FM3, aes(fill=Age, y=Overall_FM, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Chinook Total Catch") +
  facet_wrap(~FG )+#,scales="free_y")+
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")  

Cat  


ggsave(Cat,file="OUTPUTS/ChinookTotalFM.png",width = 28, height = 12, units = "cm")


 
#----Create ID index for both dataframes---- 
Salmon_Biomass<-ter3%>%left_join(catch3)
Salmon_Biomass<-Salmon_Biomass%>%left_join(FM3)
Salmon_Biomass$Abundance<-Salmon_Biomass$Escapement+Salmon_Biomass$Overall_Catch
unique(Salmon_Biomass$Age)
str(Salmon_Biomass)
Salmon_Biomass<-Salmon_Biomass %>%
  mutate(Weight=case_when(Age==2 ~ 4.086,
                          Age==3 ~ 6.777,
                          Age==4 ~ 8.854,
                          Age==5 ~ 10.589))

str(Salmon_Biomass)
Salmon_Biomass$Biomass_t<-(Salmon_Biomass$Abundance*Salmon_Biomass$Weight)/1000

Salmon_Biomass$Biomass_t_km2<-Salmon_Biomass$Biomass/Area
Salmon_Biomass$Catch_t_km2<-(Salmon_Biomass$Overall_Catch*Salmon_Biomass$Weight)/1000/Area
Salmon_Biomass$Fishing_Mortality<-Salmon_Biomass$Catch_t_km2/Salmon_Biomass$Biomass_t_km2
Salmon_Biomass$Catch_t_km2_NFW<-(Salmon_Biomass$Overall_FM*Salmon_Biomass$Weight)/1000/Area
Salmon_Biomass$Fishing_Mortality_NFW<-Salmon_Biomass$Catch_t_km2_NFW/Salmon_Biomass$Biomass_t_km2
Salmon_Biomass$Fishing_Mortality_Rate<-Salmon_Biomass$Fishing_Mortality_NFW*Salmon_Biomass$Biomass_t_km2
#Salmon_Biomass<-subset(Salmon_Biomass, select=c(Year, FG, Abundance, Biomass_t_km2))


abund<- ggplot(Salmon_Biomass, aes(fill=Age, y=Biomass_t_km2, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Chinook Abunance") +
  facet_wrap(~FG)+#, scales="free_y") #+
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")

abund

write_csv(Salmon_Biomass,"OUTPUTS/Chinook_Parameters.csv")

Initial_Biomass<-subset(Salmon_Biomass, Year=="1979"& Age=="4")

Salmon_Biomass_4<-subset(Salmon_Biomass, Age=="4")


str(Initial_Biomass)
str(Salmon_Biomass_4)
Salmon_Biomass_4$RelativeBiomass <- Salmon_Biomass_4$Biomass_t_km2/ Initial_Biomass$Biomass_t_km2[ match( Salmon_Biomass_4$FG , Initial_Biomass$FG ) ]

write_csv(Salmon_Biomass_4,"OUTPUTS/Salmon_Biomass_4.csv")


Salmon_Biomass$Group <- paste(Salmon_Biomass$FG, Salmon_Biomass$Age, sep="_")
str(Salmon_Biomass)

Salmon_BiomassAg<-aggregate(.~brood_year, FUN = sum, data=Chum_Dataset)

  
####FRG_SP
FRG_SP<-SalmonBiomass[Salmon_Biomass$FG=="FRG" & Chinook_Dataset$run==1,]
FRG_SP_Ecopath<-FRG_SP$Biomass_t_km2[1]
FRG_SP$Relative_Biomass_Ecopath<-FRG_SP$Biomass_t_km2/FRG_SP_Ecopath
write.csv(FRG_SP, "OUTPUT/Hatchery/FRG SP hatchery releases per year.csv")




ggarrange(Term,Cat,abund,                                       # First row with scatter plot
          ncol = 1, labels = c("A","B", "C"), # Second row with box and dot plots
          nrow = 3) 


AGE<-df%>%filter(Year.x,Age.x,FG.x)

table(df$Year.x,df$Age.x,df$FG.x)
write_rds(df,"Data/ChinookAbundance.rds")
write_csv(df,"Data/ChinookAbundance.csv") 
