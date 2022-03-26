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


#---Read in catch statistics----
catch<-read_csv("Data/2021_07_catch_mortality_couture_v3.csv")
spec(catch)
str(catch)
catch$Stock<-as.factor(catch$Stock)
catch$Fishery<-as.factor(catch$Fishery)

#----Combine abnundance data by group and year----

ter<-ter %>%
  mutate(FG = case_when(Stock == 'FS2' | Stock == 'FS3' ~ 'FRG_SP',
                        Stock == 'FSS' | Stock == 'FSO' ~ 'FRG_SU',
                        Stock == 'FCF' | Stock == 'FHF' ~ 'FRG_FA',
                        Stock == 'MGS'  ~ 'GST_NO',
                        Stock == 'LGS' ~ 'GST_LO',
                        Stock == 'NKF' | Stock == 'PSY'|Stock == 'PSN'|Stock == 'PSF'|Stock == 'SKG'|Stock == 'SNO'|Stock == 'STL' ~ 'PSD_FA',
                        Stock == 'NKS'  ~ 'PSD_SP',
                        Stock == 'WVN' | Stock == 'WVH' ~ 'WCV_FA',
                        Stock == 'NOC'  ~ 'CAO_FA',
                        Stock == 'FCF' | Stock == 'FHF' ~ 'FRG_FA',
                        Stock == 'CWF' | Stock == 'MCB'|Stock == 'LYF'|Stock == 'URB'|Stock == 'BON'|Stock == 'SPR' ~ 'COR_FA',
                        Stock == 'CWS' | Stock == 'WSH' ~ 'COR_SP'))
ter$FG<-as.factor((ter$FG))
str(ter)
plot(ter$Terminal.Run ~ ter$Stock+ter$Year)

ter2<-ter %>%
  group_by(FG) %>%
  summarise_all(funs(paste(na.omit(.), collapse = ",")))

ter2<-ter %>%
  group_by(FG,Year,Age) %>%
  summarise(Terminal = sum(Terminal.Run))



ggplot(data =ter2)+
   aes(x = Year, y = Terminal, group = FG)+
  geom_line(scale = 10, size = 0.25, rel_min_height = 0.03) +
  theme_ridges() +
  scale_x_continuous(limits = c(1, 200), expand = c(0, 0))# +
  scale_y_reverse(
    breaks = c(2000, 1980, 1960, 1940, 1920, 1900),
    expand = c(0, 0)
  )
df3<-df2 %>%
  group_by(CALENDAR_YEAR, LICENCE_AREA2) %>%
  summarise(Freq = sum(TotCHINOOK))
