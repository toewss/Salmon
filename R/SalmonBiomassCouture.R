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

#sal<-read_xlsx("Data/ChinookEscape_1960_2020.xlsx")
#----Read in terminal estimate data ---- 
ter<-read_csv("Data/2022_5_Couture_cohort_escapement.csv")
spec(ter)
str(ter)
ter$Stock<-as.factor(ter$Stock)
ter$StockNum<-as.factor(ter$StockNum)
ter$Year<-ymd(ter$Year, truncated =2L)
ter$Age<-as.factor(ter$Age)

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
str(ter2)
ter2$FG<-as.factor((ter2$FG))
ter2$FG <-factor(ter2$FG, levels=c('FRGSPS SP', 'FRGSPS SU', 'FRGSPS FA','WCVI FA','CRWOC  SP','CRWOC  SU','CRWOC FA' ))
str(ter2)
summary(ter2$FG)
#plot(ter$Terminal.Run ~ ter$Stock+ter$Year)

#ter2<-ter %>%
#  group_by(Stock,Year,Age) %>%
#  summarise(Terminal = sum(Terminal.Run))

ter2<-ter2 %>%
  group_by(FG,Year)%>%#,Age) %>%
  summarise(Terminal = sum(Terminal.Run))

print(ter2,n=100)

str(ter2)
# Stacked

Term<-ggplot(ter2, aes(fill=Age, y=Terminal, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Chinook Terminal Runs") +
  facet_wrap(~FG,scales="free_y") #+
#theme_ipsum() +
#  theme(legend.position="none") +
#  xlab("")
Term
ggsave(Term,file="OUTPUTS/ChinookTermRun.png",width = 28, height = 12, units = "cm")


#---Read in catch statistics----
catch<-read_csv("Data/2022_05_Couture_catch_mortality.csv")
spec(catch)
str(catch)
catch$Stock<-as.factor(catch$Stock)
catch$Fishery<-as.factor(catch$Fishery)
catch$Year<-ymd(catch$Year, truncated =2L)
catch$Age<-as.factor(catch$Age)
catch$Total<-catch$Catch+catch$Shakers+catch$CNR.Legals+catch$CNR.Sublegals
catch$Total<-catch$AEQ.Catch+catch$AEQ.Shakers+catch$AEQ.CNRLeg+catch$AEQ.CNRSubLeg

#catch <- droplevels(catch[!catch$Fishery == 'TSF FS'&!catch$Fishery == 'TCENTRAL FS' & 
#                            !catch$Fishery == 'TNORTH'&!catch$Fishery == 'TWCVI FS'&
#                            !catch$Fishery == 'TFRASER FS'&!catch$Fishery == 'TGS FS'&
#                            !catch$Fishery == 'TPS FS',])

str(catch)
#c<-table(catch$Fishery)#,catch$Age,df$FG.x)
#c<-as.data.frame(c)
#write_csv(c,"Data/Fisheries.csv") 

#----Remove Fisheries Outside of SRKW Summer Zone----
#"TCENTRAL FN","TYK YAK FN", "TCOL R S","TNORTH FS","TCENTRAL FS",
#catch<-subset(catch, Name!="George" & Name!="Andrea")
selected<-c("ALASKA T", "ALASKA N", "ALASKA S", "TAK YAK N","TAK TBR N","TBC TBR FN","TYK YAK FN","TAK TBR S",
  
            "TCENTRAL FN","TCOL R S","TNORTH FS","TCENTRAL FS","TCOL R N","TGEO ST FN","TFRAS FN","TPS FN","TWAC FN","NORTH T",
            
            "NORTH N","TWCVI FS","TFRASER FS","TGS FS","TPS FS","TSF FS","CENTRAL T","CENTRAL N","WCVI T","N FALCON T","S FALCON T",
            
            "WCVI N","WCVI ISBM S","WASH CST N","JNST N","FRASER N","CBC S","NBC AABM S","NBC ISBM S","WCVI AABM S","N FALCON S",
            
            "S FALCON S","GEO ST S", "GEO ST T", "J DE F N","PGSDN N","PGSDN S","PGSDO N", "PGSDO S", "BC JS S")


selected<-c( "TCOL R S","TNORTH FS","TCENTRAL FS","NORTH T",
            
            "NORTH N","TGS FS","TPS FS","CENTRAL T","CENTRAL N","WCVI T","N FALCON T","S FALCON T",
            
            "WCVI N","WCVI ISBM S","WASH CST N","JNST N","FRASER N","CBC S","NBC AABM S","NBC ISBM S","WCVI AABM S","N FALCON S",
            
            "S FALCON S","GEO ST S", "GEO ST T", "J DE F N","PGSDN N","PGSDN S","PGSDO N", "PGSDO S", "BC JS S")


selected<-c("NORTH T","NORTH N","CENTRAL T","CENTRAL N","WCVI T","N FALCON T","S FALCON T",
            "WCVI N","WCVI ISBM S","WCVI AABM S","WASH CST N","JNST N","FRASER N","CBC S","NBC AABM S","NBC ISBM S","WCVI AABM S","N FALCON S","S FALCON S",
            "GEO ST S", "GEO ST T","J DE F N","PGSDN N","PGSDN S","PGSDO N", "PGSDO S", "BC JS S")
catch<-catch[catch$Fishery %in% selected,]
#"TGS FS","TPS FS","TGEO ST FN","TSF FS","TFRAS FN","TPS FN","TWAC FN","TWCVI FS","TFRASER FS","TCOL R N") 
#----Combine catch data by group and year----
str(catch)
catch2<-catch %>%
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
str(catch2)

str(catch2)
summary(catch2$FG)


#catch2<-catch2 %>%
#  group_by(Stock,Year,Age) %>%
#  summarise(Total = sum(Total))

catch3<-catch2 %>%
  group_by(FG, Year)%>%#,Age) %>%
  summarise(Total = sum(Catch))

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