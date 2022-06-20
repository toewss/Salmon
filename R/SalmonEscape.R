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


#---Read in catch statistics----
catch<-read_csv("Data/2021_07_catch_mortality_couture_v3.csv")
spec(catch)
str(catch)
catch$Stock<-as.factor(catch$Stock)
catch$Fishery<-as.factor(catch$Fishery)

#----Combine abnundance data by group and year----

ter<-ter %>%
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
ter$FG<-as.factor((ter$FG))
ter$FG2 < - factor(ter$FG, levels=c("FRG SP", "PSD SP", "COR SP","FRG SU","COR SU", "FRG FA","COR FA", "GST NO","GST LO","PSD FA","WCV FA","WAC FA","CAO FA" ))



str(ter)
summary(ter$FG)
plot(ter$Terminal.Run ~ ter$Stock+ter$Year)

ter2<-ter %>%
  group_by(Stock,Year,Age) %>%
  summarise(Terminal = sum(Terminal.Run))

ter2<-ter %>%
  group_by(FG,Year,Age) %>%
  summarise(Terminal = sum(Terminal.Run))


str(ter2)
# Stacked

ggplot(ter2, aes(fill=Age, y=Terminal, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Chinook Terminal Runs") +
  facet_wrap(~Stock) #+
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")



