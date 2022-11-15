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

Area=242623


setwd("C:/Users/fannyc64/Sync/Chapters/Chapter 1 - Ewe/Model/Data/Salmon smolts/RMIS - Hatchery release data/RMIS Query 5 - Regional refined + Brood year/")
Overall_Dataset<-read.csv("Overall Query all species all seasons.csv")
Overall_Dataset<-Overall_Dataset[Overall_Dataset$release_location_rmis_basin %in% c("JSM","JSVI","SWVI","GSMN","GSMS","GSVI","UPFR",
"LOFR","LOTR","UPTR","NOOK","BESA","SJUA",
"LOSK","UPSK","WICI","STIL","SNOH","EKPN",
"LAKW","DUWA","PUYA","KENN","DES","NISQ",
"CHAM","EKPS","SKDO","WKIT","LUDA","LYHO",
"ELDU","QUHO","QEQU","GHLC","UPCH","NASE",
"NOSM","WILR","NEHA","TILN","SIYA","ALSE",
"SIUS","COOS","COQU","SIXE","CHET","ROGU",
"UMPQ","HEAD","MEOK","WECH","PRGC","YAKI",
"MEOK","ROCK","UMAT","KLIC","WIND","HOO",
"DESC","WILL","SAND","SAWA","LEWI","GREL",
"YOCL","COWL","UPSN","SALM","CLEA","GRIA",
"LOSN","MAEL","SMIT","KLAM","TRIN","SACR"),]



All_Releases<-c("tagged_adclipped","tagged_unclipped","untagged_adclipped","untagged_unclipped","untagged_unknown")
Overall_Dataset$All_Releases<-rowSums(Overall_Dataset[, All_Releases])
Overall_Dataset=subset(Overall_Dataset, select=-c(4, 8:12))

Chinook_Dataset<-Overall_Dataset[Overall_Dataset$species==1,]
Coho_Dataset<-Overall_Dataset[Overall_Dataset$species==2,]
Chum_Dataset<-Overall_Dataset[Overall_Dataset$species==5,]

################################################## COHO SALMON

Coho_Dataset=subset(Coho_Dataset,select=-c(1,2,4:6))
Coho_Dataset<-aggregate(.~brood_year, FUN = sum, data=Coho_Dataset)
Coho_Dataset<-Coho_Dataset[Coho_Dataset$brood_year!=1977,]
Coho_Dataset<-Coho_Dataset[Coho_Dataset$brood_year!=1978,]
# Coho size when entering ocean (from Beamish et al. 2004, averaged for 2000-2001 season (table 1)=435.5mm)
# coho weight ('synchronous failure' study by Beamis et al. 2012) =74.6g (averages across 11 seasons)
CohoWSmolt=0.0746
Coho_Dataset$Biomass_t_km2<-((Coho_Dataset$All_Releases*CohoWSmolt)/1000)/Area
Coho_Ecopath<-Coho_Dataset$Biomass_t_km2[1]
Coho_Dataset$Relative_Biomass_Ecopath<-Coho_Dataset$Biomass_t_km2/Coho_Ecopath
#write.csv(Coho_Dataset, "Coho hatchery releases per year.csv")
# Loop for entering in recruitment function in Ecopath

Months<-rep(c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"), times=42)
Coho_Dataset <- Coho_Dataset[rep(seq_len(nrow(Coho_Dataset)), each = 12),]
Coho_Dataset$Months<-Months
#write.csv(Coho_Dataset,"Coho hatchery M5.csv")

################################################## CHUM SALMON

Chum_Dataset=subset(Chum_Dataset,select=-c(1,2,4:6))
Chum_Dataset<-aggregate(.~brood_year, FUN = sum, data=Chum_Dataset)
Chum_Dataset<-Chum_Dataset[Chum_Dataset$brood_year!=1978,]

ChumSmoltW<-0.0254 #(from Beamish et al. 2012 again)
Chum_Dataset$Biomass_t_km2<-((Chum_Dataset$All_Releases*ChumSmoltW)/1000)/Area
Chum_Ecopath<-Chum_Dataset$Biomass_t_km2[1]
Chum_Dataset$Relative_Biomass_Ecopath<-Chum_Dataset$Biomass_t_km2/Chum_Ecopath
write.csv(Chum_Dataset, "Chum hatchery releases per year.csv")
# Loop for entering in recruitment function in Ecopath

Chum_Dataset <- Chum_Dataset[rep(seq_len(nrow(Chum_Dataset)), each = 12),]
Chum_Dataset$Months<-Months
#write.csv(Chum_Dataset,"Chum hatchery M5.csv")


################################################## CHINOOK SALMON (7 GROUPS)



ChinookS_W<-0.038
Chinook_Dataset$Functional_Group_Region<-ifelse(Chinook_Dataset$release_location_rmis_region%in%c("WCVI"),"WCVI",
                                                ifelse(Chinook_Dataset$release_location_rmis_region%in%c("FRTH","JNST","GST","NOWA","SKAG","NPS","MPS","SPS","HOOD","JUAN"),"FRGSPS",
                                                       ifelse(Chinook_Dataset$release_location_rmis_region%in%c("NWC","GRAY","WILP","NOOR","SOOR","UPCR","SNAK","CECR","LOCR","SAFA","NOCA","KLTR"), "CRWORC", "OTHER")))






Chinook_Dataset<-Chinook_Dataset%>%filter(!(Chinook_Dataset$release_location_rmis_region=="CECA" & Chinook_Dataset$run==1))
Chinook_Dataset<-Chinook_Dataset%>%filter(!(Chinook_Dataset$release_location_rmis_region=="CECA" & Chinook_Dataset$run==2))

Chinook_Dataset<-Chinook_Dataset%>%filter(!(Chinook_Dataset$release_location_rmis_region=="NOCA" & Chinook_Dataset$run==1))
Chinook_Dataset<-Chinook_Dataset%>%filter(!(Chinook_Dataset$release_location_rmis_region=="NOCA" & Chinook_Dataset$run==2))

Chinook_Dataset<-Chinook_Dataset%>%filter(!(Chinook_Dataset$release_location_rmis_region=="KLTR" & Chinook_Dataset$run==1))
Chinook_Dataset<-Chinook_Dataset%>%filter(!(Chinook_Dataset$release_location_rmis_region=="KLTR" & Chinook_Dataset$run==2))

Chinook_Dataset<-Chinook_Dataset%>%filter(!(Chinook_Dataset$release_location_rmis_region=="SAFA" & Chinook_Dataset$run==1))
Chinook_Dataset<-Chinook_Dataset%>%filter(!(Chinook_Dataset$release_location_rmis_region=="SAFA" & Chinook_Dataset$run==2))




Chinook_Dataset=subset(Chinook_Dataset,select=-c(1,4:6))
Chinook_Dataset<-aggregate(.~brood_year+run+Functional_Group_Region, FUN=sum, data=Chinook_Dataset)
Chinook_Dataset$Biomass_t_km2<-((Chinook_Dataset$All_Releases*ChinookS_W)/1000)/Area


Chinook_Dataset<-Chinook_Dataset[Chinook_Dataset$brood_year!=1977,]
Chinook_Dataset<-Chinook_Dataset[Chinook_Dataset$brood_year!=1978,]

####FRGSPS_SP
FRGSPS_SP<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="FRGSPS" & Chinook_Dataset$run==1,]
FRGSPS_SP_Ecopath<-FRGSPS_SP$Biomass_t_km2[1]
FRGSPS_SP$Relative_Biomass_Ecopath<-FRGSPS_SP$Biomass_t_km2/FRGSPS_SP_Ecopath
#write.csv(FRGSPS_SP, "FRGSPS SP hatchery releases per year.csv")

FRGSPS_SP <- FRGSPS_SP[rep(seq_len(nrow(FRGSPS_SP)), each = 12),]
MonthsFRSP<-rep(c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"), times=41)

FRGSPS_SP$Months<-MonthsFRSP
#write.csv(FRGSPS_SP,"FRGSPS_SP M5.csv")

####FRGSPS_SU
FRGSPS_SU<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="FRGSPS" & Chinook_Dataset$run==2,]
FRGSPS_SU_Ecopath<-FRGSPS_SU$Biomass_t_km2[1]
FRGSPS_SU$Relative_Biomass_Ecopath<-FRGSPS_SU$Biomass_t_km2/FRGSPS_SU_Ecopath
write.csv(FRGSPS_SU, "FRGSPS SU hatchery releases per year.csv")
FRGSPS_SU <- FRGSPS_SU[rep(seq_len(nrow(FRGSPS_SU)), each = 12),]
FRGSPS_SU$Months<-MonthsFRSP
#write.csv(FRGSPS_SU,"FRGSPS_SU M5.csv")

####FRGSPS_FA
FRGSPS_FA<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="FRGSPS" & Chinook_Dataset$run==3,]
FRGSPS_FA_Ecopath<-FRGSPS_FA$Biomass_t_km2[1]
FRGSPS_FA$Relative_Biomass_Ecopath<-FRGSPS_FA$Biomass_t_km2/FRGSPS_FA_Ecopath
write.csv(FRGSPS_FA, "FRGSPS FA hatchery releases per year.csv")
FRGSPS_FA <- FRGSPS_FA[rep(seq_len(nrow(FRGSPS_FA)), each = 12),]
FRGSPS_FA$Months<-MonthsFRSP
#write.csv(FRGSPS_FA,"FRGSPS_FA M5.csv")

# CRWORC_SP
CRWORC_SP<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="CRWORC" & Chinook_Dataset$run==1,]
CRWORC_SP_Ecopath<-CRWORC_SP$Biomass_t_km2[1]
CRWORC_SP$Relative_Biomass_Ecopath<-CRWORC_SP$Biomass_t_km2/CRWORC_SP_Ecopath
write.csv(CRWORC_SP, "CRWORC SP hatchery releases per year.csv")
CRWORC_SP <- CRWORC_SP[rep(seq_len(nrow(CRWORC_SP)), each = 12),]
CRWORC_SP$Months<-MonthsFRSP
write.csv(CRWORC_SP,"CRWORC_SP M5.csv")

#CRWORC_SU
CRWORC_SU<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="CRWORC" & Chinook_Dataset$run==2,]
CRWORC_SU_Ecopath<-CRWORC_SU$Biomass_t_km2[1]
CRWORC_SU$Relative_Biomass_Ecopath<-CRWORC_SU$Biomass_t_km2/CRWORC_SU_Ecopath
write.csv(CRWORC_SU, "CRWORC SU hatchery releases per year.csv")
CRWORC_SU <- CRWORC_SU[rep(seq_len(nrow(CRWORC_SU)), each = 12),]
CRWORC_SU$Months<-MonthsFRSP
write.csv(CRWORC_SU,"CRWORC_SU M5.csv")

#CRWORC_FA
CRWORC_FA<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="CRWORC" & Chinook_Dataset$run==3,]
CRWORC_FA_Ecopath<-CRWORC_FA$Biomass_t_km2[1]
CRWORC_FA$Relative_Biomass_Ecopath<-CRWORC_FA$Biomass_t_km2/CRWORC_FA_Ecopath
write.csv(CRWORC_FA, "CRWORC FA hatchery releases per year.csv")
CRWORC_FA <- CRWORC_FA[rep(seq_len(nrow(CRWORC_FA)), each = 12),]
CRWORC_FA$Months<-MonthsFRSP
write.csv(CRWORC_FA,"CRWORC_FA M5.csv")

#WCVI
WCVI<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="WCVI" ,]
WCVI_Ecopath<-WCVI$Biomass_t_km2[1]
WCVI$Relative_Biomass_Ecopath<-WCVI$Biomass_t_km2/WCVI_Ecopath
write.csv(WCVI, "WCVI hatchery releases per year.csv")
WCVI <- WCVI[rep(seq_len(nrow(WCVI)), each = 12),]
WCVI$Months<-MonthsFRSP
write.csv(WCVI,"WCVI M5.csv")
