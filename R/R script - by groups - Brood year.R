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
library(here)
Area=242623
Area=176000

#setwd("C:/Users/fannyc64/Sync/Chapters/Chapter 1 - Ewe/Model/Data/Salmon smolts/RMIS - Hatchery release data/RMIS Query 5 - Regional refined + Brood year/")
Overall_Dataset<-read.csv("Data/Hatchery/Overall Query all species all seasons.csv")
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
write.csv(Coho_Dataset, "Data/Hatchery/Coho hatchery releases per year.csv")
# Loop for entering in recruitment function in Ecopath

Months<-rep(c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"), times=42)
Coho_Dataset <- Coho_Dataset[rep(seq_len(nrow(Coho_Dataset)), each = 12),]
Coho_Dataset$Months<-Months
write.csv(Coho_Dataset,"Data/Hatchery/Coho hatchery M5.csv")

################################################## CHUM SALMON

Chum_Dataset=subset(Chum_Dataset,select=-c(1,2,4:6))
Chum_Dataset<-aggregate(.~brood_year, FUN = sum, data=Chum_Dataset)
Chum_Dataset<-Chum_Dataset[Chum_Dataset$brood_year!=1978,]

ChumSmoltW<-0.0254 #(from Beamish et al. 2012 again)
Chum_Dataset$Biomass_t_km2<-((Chum_Dataset$All_Releases*ChumSmoltW)/1000)/Area
Chum_Ecopath<-Chum_Dataset$Biomass_t_km2[1]
Chum_Dataset$Relative_Biomass_Ecopath<-Chum_Dataset$Biomass_t_km2/Chum_Ecopath
write.csv(Chum_Dataset, "Data/Hatchery/Chum hatchery releases per year.csv")
# Loop for entering in recruitment function in Ecopath

Chum_Dataset <- Chum_Dataset[rep(seq_len(nrow(Chum_Dataset)), each = 12),]
Chum_Dataset$Months<-Months
write.csv(Chum_Dataset,"Data/Hatchery/Chum hatchery M5.csv")


################################################## CHINOOK SALMON (7 GROUPS)



ChinookS_W<-0.038
Chinook_Dataset$Functional_Group_Region<-ifelse(Chinook_Dataset$release_location_rmis_region%in%c("WCVI"),"WVI",
                                                ifelse(Chinook_Dataset$release_location_rmis_region%in%c("FRTH"),"FRG",
                                                       ifelse(Chinook_Dataset$release_location_rmis_region%in%c("GST","JNST","JUAN"),"GST",
                                                              ifelse(Chinook_Dataset$release_location_rmis_region%in%c("NOWA", "SKAG","NPS","MPS","SPS","HOOD"),"PSD",
                                                                     ifelse(Chinook_Dataset$release_location_rmis_region%in%c("CECR", "LOCR","SNAK","UPCR"),"COL",
                                                                            ifelse(Chinook_Dataset$release_location_rmis_region%in%c("NWC","GRAY","WILP"),"WAC",
                                                                                ifelse(Chinook_Dataset$release_location_rmis_region%in%c("NOOR","SOOR","SAFA","NOCA","KLTR"), "CAO", "OTHER")))))))
                                                              
                                                              


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

####FRG_SP
FRG_SP<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="FRG" & Chinook_Dataset$run==1,]
FRG_SP_Ecopath<-FRG_SP$Biomass_t_km2[1]
FRG_SP$Relative_Biomass_Ecopath<-FRG_SP$Biomass_t_km2/FRG_SP_Ecopath
write.csv(FRG_SP, "OUTPUT/Hatchery/FRG SP hatchery releases per year.csv")

FRG_SP <- FRG_SP[rep(seq_len(nrow(FRG_SP)), each = 12),]
MonthsFRSP<-rep(c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"), times=41)

FRG_SP$Months<-MonthsFRSP
write.csv(FRG_SP,"OUTPUT/Hatchery/FRG_SP M5.csv")

####FRG_SU
FRG_SU<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="FRG" & Chinook_Dataset$run==2,]
FRG_SU_Ecopath<-FRG_SU$Biomass_t_km2[1]
FRG_SU$Relative_Biomass_Ecopath<-FRG_SU$Biomass_t_km2/FRG_SU_Ecopath
write.csv(FRG_SU, "OUTPUT/Hatchery/FRG SU hatchery releases per year.csv")
FRG_SU <- FRG_SU[rep(seq_len(nrow(FRG_SU)), each = 12),]
FRG_SU$Months<-MonthsFRSP
write.csv(FRG_SU,"OUTPUT/Hatchery/FRG_SU M5.csv")

####FRG_FA
FRG_FA<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="FRG" & Chinook_Dataset$run==3,]
FRG_FA_Ecopath<-FRG_FA$Biomass_t_km2[1]
FRG_FA$Relative_Biomass_Ecopath<-FRG_FA$Biomass_t_km2/FRG_FA_Ecopath
write.csv(FRG_FA, "OUTPUT/Hatchery/FRG FA hatchery releases per year.csv")
FRG_FA <- FRG_FA[rep(seq_len(nrow(FRG_FA)), each = 12),]
FRG_FA$Months<-MonthsFRSP
write.csv(FRG_FA,"OUTPUT/Hatchery/FRG_FA M5.csv")

####GST_FA
GST_FA<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="GST" & Chinook_Dataset$run==3,]
GST_FA_Ecopath<-GST_FA$Biomass_t_km2[1]
GST_FA$Relative_Biomass_Ecopath<-GST_FA$Biomass_t_km2/GST_FA_Ecopath
write.csv(GST_FA, "OUTPUT/Hatchery/GST FA hatchery releases per year.csv")
GST_FA <- GST_FA[rep(seq_len(nrow(GST_FA)), each = 12),]
GST_FA$Months<-MonthsFRSP
write.csv(GST_FA,"OUTPUT/Hatchery/GST_FA M5.csv")

# COL_SP
COL_SP<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="COL" & Chinook_Dataset$run==1,]
COL_SP_Ecopath<-COL_SP$Biomass_t_km2[1]
COL_SP$Relative_Biomass_Ecopath<-COL_SP$Biomass_t_km2/COL_SP_Ecopath
write.csv(COL_SP, "OUTPUT/Hatchery/COL SP hatchery releases per year.csv")
COL_SP <- COL_SP[rep(seq_len(nrow(COL_SP)), each = 12),]
COL_SP$Months<-MonthsFRSP
write.csv(COL_SP,"OUTPUT/Hatchery/COL_SP M5.csv")

#COL_SU
COL_SU<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="COL" & Chinook_Dataset$run==2,]
COL_SU_Ecopath<-COL_SU$Biomass_t_km2[1]
COL_SU$Relative_Biomass_Ecopath<-COL_SU$Biomass_t_km2/COL_SU_Ecopath
write.csv(COL_SU, "OUTPUT/Hatchery/COL SU hatchery releases per year.csv")
COL_SU <- COL_SU[rep(seq_len(nrow(COL_SU)), each = 12),]
COL_SU$Months<-MonthsFRSP
write.csv(COL_SU,"OUTPUT/Hatchery/COL_SU M5.csv")

#COL_FA
COL_FA<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="COL" & Chinook_Dataset$run==3,]
COL_FA_Ecopath<-COL_FA$Biomass_t_km2[1]
COL_FA$Relative_Biomass_Ecopath<-COL_FA$Biomass_t_km2/COL_FA_Ecopath
write.csv(COL_FA, "OUTPUT/Hatchery/COL FA hatchery releases per year.csv")
COL_FA <- COL_FA[rep(seq_len(nrow(COL_FA)), each = 12),]
COL_FA$Months<-MonthsFRSP
write.csv(COL_FA,"OUTPUT/Hatchery/COL_FA M5.csv")

#PSD_FA
PSD_FA<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="PSD" & Chinook_Dataset$run==3,]
PSD_FA_Ecopath<-PSD_FA$Biomass_t_km2[1]
PSD_FA$Relative_Biomass_Ecopath<-PSD_FA$Biomass_t_km2/PSD_FA_Ecopath
write.csv(PSD_FA, "OUTPUT/Hatchery/PSD FA hatchery releases per year.csv")
PSD_FA <- PSD_FA[rep(seq_len(nrow(PSD_FA)), each = 12),]
PSD_FA$Months<-MonthsFRSP
write.csv(PSD_FA,"OUTPUT/Hatchery/PSD_FA M5.csv")

#WAC_FA
WAC_FA<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="WAC" & Chinook_Dataset$run==3,]
WAC_FA_Ecopath<-WAC_FA$Biomass_t_km2[1]
WAC_FA$Relative_Biomass_Ecopath<-WAC_FA$Biomass_t_km2/WAC_FA_Ecopath
write.csv(WAC_FA, "OUTPUT/Hatchery/WAC FA hatchery releases per year.csv")
WAC_FA <- WAC_FA[rep(seq_len(nrow(WAC_FA)), each = 12),]
WAC_FA$Months<-MonthsFRSP
write.csv(WAC_FA,"OUTPUT/Hatchery/WAC_FA M5.csv")

#CAO_FA
CAO_FA<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="CAO" & Chinook_Dataset$run==3,]
CAO_FA_Ecopath<-CAO_FA$Biomass_t_km2[1]
CAO_FA$Relative_Biomass_Ecopath<-CAO_FA$Biomass_t_km2/CAO_FA_Ecopath
write.csv(CAO_FA, "OUTPUT/Hatchery/CAO FA hatchery releases per year.csv")
CAO_FA <- CAO_FA[rep(seq_len(nrow(CAO_FA)), each = 12),]
CAO_FA$Months<-MonthsFRSP
write.csv(CAO_FA,"OUTPUT/Hatchery/CAO_FA M5.csv")

#WCVI
WCVI<-Chinook_Dataset[Chinook_Dataset$Functional_Group_Region=="WCVI" ,]
WCVI_Ecopath<-WCVI$Biomass_t_km2[1]
WCVI$Relative_Biomass_Ecopath<-WCVI$Biomass_t_km2/WCVI_Ecopath
write.csv(WCVI, "OUTPUT/Hatchery/WCVI hatchery releases per year.csv")
WCVI <- WCVI[rep(seq_len(nrow(WCVI)), each = 12),]
WCVI$Months<-MonthsFRSP
write.csv(WCVI,"OUTPUT/Hatchery/WCVI M5.csv")
