#----Load required packages----
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
suppressMessages(library(maptools))
suppressMessages(library(rgeos))
suppressMessages(library(rgdal))
suppressMessages(library(spatial))
suppressMessages(library(here))
suppressMessages(library(sf))
suppressMessages(library(tibble))
suppressMessages(library(readxl))
suppressMessages(library(ggplot2))
suppressMessages(library(raster))
suppressMessages(library(sp))
suppressMessages(library(inlabru))
suppressMessages(library(ggpubr))
suppressMessages(library(renv))
library(readr)


memory.size() ### Checking your memory size
memory.limit() ## Checking the set limit
memory.limit(size=560000)


# get all files with the .shp extension from working directory
#setwd(here("Data","Results","SRKW_Occurrence","AcrossMonth","Regional SRKW Polygons_AM"))
# get all files with the .shp extension from working directory


df <- list.files(path = "Data",     # Identify all csv files in folder
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                                            # Store all files in list
  bind_rows                                                       # Combine data sets into one data set 
str(df)      
df$CALENDAR_YEAR<-factor(df$CALENDAR_YEAR)# Print data to RStudio console
plot(df$CALENDAR_YEAR,df$CHINOOK_KEPT)
plot(df$CALENDAR_YEAR,df$CHINOOK_RELD)


df<-df %>%
 dplyr::select( c('FISHERY', 'LICENCE_AREA','CALENDAR_YEAR','VESSEL_COUNT',
             'BOAT_DAYS', 'CHINOOK_KEPT','CHINOOK_RELD'))
df
summary(df)
df <- mutate_at(df, c("CHINOOK_RELD"), ~replace(., is.na(.), 0))


df$TotCHINOOK<-df$CHINOOK_KEPT+df$CHINOOK_RELD
df<-df[complete.cases(df[ , 8]),]
plot(df$CALENDAR_YEAR,df$TotCHINOOK)

df$LICENCE_AREA<-factor(df$LICENCE_AREA)

df2<-df %>%
  separate(LICENCE_AREA , c("LICENCE_AREA2", "FISHERY"), "-")

df3<-df2 %>%
  group_by(CALENDAR_YEAR, LICENCE_AREA2) %>%
  summarise(Freq = sum(TotCHINOOK))
df3

ggplot(df3, aes(fill=LICENCE_AREA2, y=Freq, x=CALENDAR_YEAR)) + 
  geom_bar(position="stack", stat="identity")
