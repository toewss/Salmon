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

#----Install required packages----c

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

Area=242623
ChinookW=8.5/1000

#----Read in terminal estimate data ---- 
ter<-read_csv("Data/2022_5_Couture_cohort_escapement.csv")
ter<-subset(ter, select = -c(AEQCohort, StockNum,  Cohort))
ter$Stock<-as.factor(ter$Stock)
#ter$StockNum<-as.factor(ter$StockNum)
ter$Year<-ymd(ter$Year, truncated =2L)
ter$Age<-as.factor(ter$Age)
ter<-aggregate(.~Year+Stock, data=ter, FUN=sum)
#ter<-aggregate(.~Year+Stock+Age, data=ter, FUN=sum)
ter$Terminal.Run<-NULL

#----Combine abnundance data by group and year----
str(ter)
ter2<-ter %>%
  mutate(FG = case_when(Stock == 'FS2' | Stock == 'FS3'|Stock == 'NKS' ~ 'FRGSPS SP',
                        Stock == 'FSS' | Stock == 'FSO'|Stock == 'SNO'|Stock == 'SKG' ~ 'FRGSPS SU',
                        Stock == 'FCF' | Stock == 'FHF'| Stock == 'NKF' | Stock == 'PSY'|Stock == 'PSN'|Stock == 'PSF'|Stock == 'STL'|Stock == 'MGS'|Stock == 'LGS' ~ 'FRGSPS FA',
                        Stock == 'WVN' | Stock == 'WVH' ~ 'WCVI FA',
                        Stock == 'NOC' |Stock == 'WCN' | Stock == 'WCH'|Stock == 'CWF' | Stock == 'MCB'|Stock == 'LYF'|Stock == 'URB'|Stock == 'BON'|Stock == 'SPR' ~ 'CRWOC FA',
                        Stock == 'SUM'  ~ 'CRWOC  SU',
                        Stock == 'CWS' | Stock == 'WSH' ~ 'CRWOC  SP'))
ter2$FG<-as.factor((ter2$FG))
ter2$FG <-factor(ter2$FG, levels=c('FRGSPS SP', 'FRGSPS SU', 'FRGSPS FA','WCVI FA','CRWOC  SP','CRWOC  SU','CRWOC FA' ))
str(ter2)
summary(ter2$FG)
ter2$Stock<-NULL
ter3<-aggregate(.~Year+FG, FUN=sum, data=ter2)
#ter3<-aggregate(.~Year+FG+Age, FUN=sum, data=ter2)
view(ter3)

# Stacked

Term<-ggplot(ter3, aes(fill=Age, y=Escapement, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Chinook Terminal Runs") +
  facet_wrap(~FG,scales="free_y") #+
#theme_ipsum() +
#  theme(legend.position="none") +
#  xlab("")
Term
ggsave(Term,file="OUTPUTS/ChinookTermRun.png",width = 28, height = 12, units = "cm")

################CATCH DATA##############################################
#---Read in catch statistics----
catch<-read_csv("Data/2022_05_Couture_catch_mortality.csv")

spec(catch)
str(catch)

catch<-subset(catch, select=-c(FishNum, StockNum,Scaled.to.Observed.Catch, Model.Catch.to.Observed.Catch.Ratio,
                                                         AEQ.Catch, AEQ.Shakers, AEQ.CNRSubLeg, AEQ.CNRLeg))
unique(catch$Fishery)
#----Remove Fisheries Outside of SRKW Summer Zone----

catch<-catch[(catch$Fishery!="ALASKA T"),]
catch<-catch[(catch$Fishery!="ALASKA N"),]
catch<-catch[(catch$Fishery!="TBC TBR FN"),]
catch<-catch[(catch$Fishery!="TAK YAK N"),]
catch<-catch[(catch$Fishery!="ALASKA S"),]
catch<-catch[(catch$Fishery!="TAK TBR N"),]
catch<-catch[(catch$Fishery!="TYK YAK FN"),]
catch<-catch[(catch$Fishery!="TAK TBR S"),]
catch$Fishery<-NULL
#catch$Age<-NULL
catch<-aggregate(.~Year+ Stock, data=catch, FUN=sum)
catch$X<-NULL
str(catch)
catch$Overall_Catch<-rowSums(catch[,(5:8)])
#catch$Total<-catch$Catch+catch$Shakers+catch$CNR.Legals+catch$CNR.Sublegals
catch2<-subset(catch, select=-c(Catch, Shakers, CNR.Legals, CNR.Sublegals))

#"TGS FS","TPS FS","TGEO ST FN","TSF FS","TFRAS FN","TPS FN","TWAC FN","TWCVI FS","TFRASER FS","TCOL R N") 
#----Combine catch data by group and year----
str(catch2)
catch2<-catch2 %>%
  mutate(FG = case_when(Stock == 'FS2' | Stock == 'FS3'|Stock == 'NKS' ~ 'FRGSPS SP',
                        Stock == 'FSS' | Stock == 'FSO'|Stock == 'SNO'|Stock == 'SKG' ~ 'FRGSPS SU',
                        Stock == 'FCF' | Stock == 'FHF'| Stock == 'NKF' | Stock == 'PSY'|Stock == 'PSN'|Stock == 'PSF'|Stock == 'STL'|Stock == 'MGS'|Stock == 'LGS' ~ 'FRGSPS FA',
                        Stock == 'WVN' | Stock == 'WVH' ~ 'WCVI FA',
                        Stock == 'NOC' |Stock == 'WCN' | Stock == 'WCH'|Stock == 'CWF' | Stock == 'MCB'|Stock == 'LYF'|Stock == 'URB'|Stock == 'BON'|Stock == 'SPR' ~ 'CRWOC FA',
                        Stock == 'SUM'  ~ 'CRWOC  SU',
                        Stock == 'CWS' | Stock == 'WSH' ~ 'CRWOC  SP'))


str(catch2)
catch2$FG<-as.factor((catch2$FG))
catch2$FG <-factor(catch2$FG, levels=c('FRGSPS SP', 'FRGSPS SU', 'FRGSPS FA','WCVI FA','CRWOC  SP','CRWOC  SU','CRWOC FA' ))

catch2$Stock<-NULL
catch2$X<-NULL
catch3<-aggregate(.~Year+FG, FUN=sum, data=catch2)
view(catch3)



catch$Stock<-as.factor(catch$Stock)
catch$Fishery<-as.factor(catch$Fishery)
catch$Year<-ymd(catch$Year, truncated =2L)
catch$Age<-as.factor(catch$Age)




str(catch2)
summary(catch2$FG)




catch3<-catch2 %>%
  group_by(Year,FG )%>%#,Age) %>%
  summarise(Total = sum(Catch))

view(catch3)
Cat<-  ggplot(catch2, aes(fill=Fishery, y=Total, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Chinook Total Catch") +
  facet_wrap(~FG ,scales="free_y") #+
#theme_ipsum() +
#  theme(legend.position="none") +
#  xlab("")  

Cat  
catch3
print(catch3,n=48)
c<-as.data.frame(catch3)
write_csv(c,"OUTPUTS/Fisheries.csv") 
ggsave(Cat,file="OUTPUTS/ChinookTotalCatch.png",width = 28, height = 12, units = "cm")




#----Create ID index for both dataframes----  
catch2 <- cbind(ID = 1:nrow(catch2), catch2)    # Applying cbind function
#catch2$Type<-c("Fishery")                                      # Printing updated data
ter2 <- cbind(ID = 1:nrow(ter2), ter2)    # Applying cbind function
#ter2$Type<-c("Terminal")  

df<- full_join(ter2, catch2, by = 'ID')
df
df$Abund<-df$Terminal+df$Total
df$Biomass<-df$Abund*8.5
df$catratio<-df$Total/df$Abund
df

abund<- ggplot(df, aes(y=Abund, x=Year.x)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Chinook Abunance") +
  facet_wrap(~FG.x, scales="free_y") #+
#theme_ipsum() +
 # theme(legend.position="none") +
#  xlab("")
abund


df2<-as.data.frame(df)
print(df2)
write_csv(df2,"OUTPUTS/Totals.csv") 
ggarrange(Term,Cat,abund,                                       # First row with scatter plot
          ncol = 1, labels = c("A","B", "C"), # Second row with box and dot plots
          nrow = 3) 


AGE<-df%>%filter(Year.x,Age.x,FG.x)