library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(scales)
library(cowplot)
library(ggpubr)
library(lubridate)

setwd("C:/Users/fannyc64/Sync/Chapters/Chapter 1 - Ewe/Model/Data/Salmon smolts/RMIS - Hatchery release data/RMIS Query 5 - Regional refined + Brood year/")
All_Releases<-read.csv("All releases W + H per year - brood year.csv")
Months<-rep(c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"), times=40)
All_Releases <- All_Releases[rep(seq_len(nrow(All_Releases)), each = 12),]
All_Releases$Months<-Months
write.csv(All_Releases, "All_Releases_with years and months - Ecosim.csv")

coho<-read.csv("Coho hatchery releases per year.csv")
coho=subset(coho, select=c(2,3,5))
Months<-rep(c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"), times=42)
coho_months_year <- coho[rep(seq_len(nrow(coho)), each = 12),]
coho_months_year$Months<-Months
write.csv(coho_months_year, "Coho_with years and months - Ecosim.csv")

chum<-read.csv("Chum hatchery releases per year.csv")
chum=subset(chum, select=c(2,3,5))
Months<-rep(c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"), times=42)
chum_months_year <- chum[rep(seq_len(nrow(chum)), each = 12),]
chum_months_year$Months<-Months
write.csv(chum_months_year, "Chum_with years and months - Ecosim.csv")
