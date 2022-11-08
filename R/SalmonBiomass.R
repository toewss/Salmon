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
#suppressMessages(library(maptools))
#suppressMessages(library(rgeos))
#suppressMessages(library(rgdal))
#suppressMessages(library(spatial))
suppressMessages(library(here))
#suppressMessages(library(sf))
suppressMessages(library(tibble))
suppressMessages(library(readxl))
suppressMessages(library(ggplot2))
#suppressMessages(library(raster))
#suppressMessages(library(sp))
#suppressMessages(library(inlabru))
suppressMessages(library(ggpubr))
#suppressMessages(library(renv))
library(readr)
library(ggridges)
#library(plot3D)
#library(ggrgl)
library(tidyverse)
library(ggrepel)
library(ggtext)
library(showtext)

font_add_google("Lato")
showtext_auto()

# Libraries

library(hrbrthemes)
library(viridis)
#sal<-read_xlsx("Data/ChinookEscape_1960_2020.xlsx")
#----Read in terminal estimate data ---- 
ter<-read_csv("Data/2021_07_cohort_escapement_couture_v3.csv")
spec(ter)
str(ter)
ter$Stock<-as.factor(ter$Stock)
ter$StockNum<-as.factor(ter$StockNum)
ter$Year<-ymd(ter$Year, truncated =2L)
ter$Age<-as.factor(ter$Age)

#----Combine abnundance data by group and year----
str(ter)
ter2<-ter %>%
  mutate(FG = case_when(Stock == 'FS2' | Stock == 'FS3' ~ 'FRG SP',
                        Stock == 'FSS' | Stock == 'FSO' ~ 'FRG SU',
                        Stock == 'FCF' | Stock == 'FHF' ~ 'FRG FA',
                        Stock == 'MGS'  ~ 'GST NO',
                        Stock == 'LGS' ~ 'GST LO',
                        Stock == 'NKF' | Stock == 'PSY'|Stock == 'PSN'|Stock == 'PSF'|Stock == 'SKG'|Stock == 'SNO'|Stock == 'STL' ~ 'PSD FA',
                        Stock == 'NKS'  ~ 'PSD SP',
                        Stock == 'WVN' | Stock == 'WVH' ~ 'WCV FA',
                        Stock == 'NOC'  ~ 'CAO FA',
                        Stock == 'WCN' | Stock == 'WCH' ~ 'WAC FA',
                        Stock == 'CWF' | Stock == 'MCB'|Stock == 'LYF'|Stock == 'URB'|Stock == 'BON'|Stock == 'SPR' ~ 'COR FA',
                        Stock == 'SUM'  ~ 'COR SU',
                        Stock == 'CWS' | Stock == 'WSH' ~ 'COR SP'))
str(ter2)
ter2$FG<-as.factor((ter2$FG))
ter2$FG <-factor(ter2$FG, levels=c("FRG SP", "PSD SP", "COR SP","FRG SU","COR SU", "FRG FA","COR FA", "GST NO","GST LO","PSD FA","WCV FA","WAC FA","CAO FA" ))
str(ter2)


str(ter2)
summary(ter2$FG)
plot(ter$Terminal.Run ~ ter$Stock+ter$Year)

ter2<-ter %>%
  group_by(Stock,Year,Age) %>%
  summarise(Terminal = sum(Terminal.Run))

ter2<-ter2 %>%
  group_by(FG,Year,Age) %>%
  summarise(Terminal = sum(Terminal.Run))


str(ter2)
# Stacked

Term<-ggplot(ter2, aes(fill=Age, y=Terminal, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Chinook Terminal Runs") +
  facet_wrap(~FG,scales="free_y") #+
theme_ipsum() +
  theme(legend.position="none") +
  xlab("")
Term
ggsave(Term,file="OUTPUTS/ChinookTermRun.png",width = 28, height = 12, units = "cm")


#---Read in catch statistics----
catch<-read_csv("Data/2021_07_catch_mortality_couture_v3.csv")
spec(catch)
str(catch)
catch$Stock<-as.factor(catch$Stock)
catch$Fishery<-as.factor(catch$Fishery)
catch$Year<-ymd(catch$Year, truncated =2L)
catch$Age<-as.factor(catch$Age)
catch$Total<-catch$Catch+catch$Shakers+catch$CNR.Legals+catch$CNR.Sublegals
catch <- droplevels(catch[!catch$Fishery == 'TSF FS'&!catch$Fishery == 'TCENTRAL FS' & 
                            !catch$Fishery == 'TNORTH'&!catch$Fishery == 'TWCVI FS'&
                            !catch$Fishery == 'TFRASER FS'&!catch$Fishery == 'TGS FS'&
                            !catch$Fishery == 'TPS FS',])

str(datf)
c<-table(catch$Fishery)#,catch$Age,df$FG.x)
c<-as.data.frame(c)
write_csv(c,"Data/Fisheries.csv") 

#----Remove Fisheries Outside of SRKW Summer Zone----

catch<-subset(catch, Name!="George" & Name!="Andrea")
selected<-c("WCVI T","WCVI N","WCVI ISBM S","WCVI AABM S",
            "GEO ST S", "GEO ST T","J DE F N","PGSDN N",
            "PGSDN S","PGSDO N", "PGSDO S") 
catch<-catch[catch$Fishery %in% selected,]

#----Combine catch data by group and year----
str(catch)
catch2<-catch %>%
  mutate(FG = case_when(Stock == 'FS2' | Stock == 'FS3' ~ 'FRG SP',
                        Stock == 'FSS' | Stock == 'FSO' ~ 'FRG SU',
                        Stock == 'FCF' | Stock == 'FHF' ~ 'FRG FA',
                        Stock == 'MGS'  ~ 'GST NO',
                        Stock == 'LGS' ~ 'GST LO',
                        Stock == 'NKF' | Stock == 'PSY'|Stock == 'PSN'|Stock == 'PSF'|Stock == 'SKG'|Stock == 'SNO'|Stock == 'STL' ~ 'PSD FA',
                        Stock == 'NKS'  ~ 'PSD SP',
                        Stock == 'WVN' | Stock == 'WVH' ~ 'WCV FA',
                        Stock == 'NOC'  ~ 'CAO FA',
                        Stock == 'WCN' | Stock == 'WCH' ~ 'WAC FA',
                        Stock == 'CWF' | Stock == 'MCB'|Stock == 'LYF'|Stock == 'URB'|Stock == 'BON'|Stock == 'SPR' ~ 'COR FA',
                        Stock == 'SUM'  ~ 'COR SU',
                        Stock == 'CWS' | Stock == 'WSH' ~ 'COR SP'))
str(catch2)
catch2$FG<-as.factor((catch2$FG))
catch2$FG <-factor(catch2$FG, levels=c("FRG SP", "PSD SP", "COR SP","FRG SU","COR SU", "FRG FA","COR FA", "GST NO","GST LO","PSD FA","WCV FA","WAC FA","CAO FA" ))
str(catch2)

str(catch2)
summary(catch2$FG)


catch2<-catch2 %>%
  group_by(Stock,Year,Age) %>%
  summarise(Total = sum(Total))

catch2<-catch2 %>%
  group_by(FG, Year,Age) %>%
  summarise(Total = sum(Total))

Cat<-  ggplot(catch2, aes(fill=Age, y=Total, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Chinook Total Catch") +
  facet_wrap(~Fishery ,scales="free_y") #+
theme_ipsum() +
  theme(legend.position="none") +
  xlab("")  

Cat  


ggsave(Cat,file="OUTPUTS/ChinookTotalCatch.png",width = 28, height = 12, units = "cm")




#----Create ID index for both dataframes----  
catch2 <- cbind(ID = 1:nrow(catch2), catch2)    # Applying cbind function
catch2$Type<-c("Fishery")                                      # Printing updated data
ter2 <- cbind(ID = 1:nrow(ter2), ter2)    # Applying cbind function
ter2$Type<-c("Terminal")  

df<- full_join(ter2, catch2, by = 'ID')
df
df$Abund<-df$Terminal+df$Total
df$catratio<-df$Total/df$Abund

abund<- ggplot(df, aes(fill=Age.x, y=Abund, x=Year.x)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Chinook Abunance") +
  facet_wrap(~FG.x, scales="free_y") #+
theme_ipsum() +
  theme(legend.position="none") +
  xlab("")

abund

ggarrange(Term,Cat,abund,                                       # First row with scatter plot
          ncol = 1, labels = c("A","B", "C"), # Second row with box and dot plots
          nrow = 3) 


AGE<-df%>%filter(Year.x,Age.x,FG.x)

table(df$Year.x,df$Age.x,df$FG.x)
write_rds(df,"Data/ChinookAbundance.rds")
write_csv(df,"Data/ChinookAbundance.csv") 
