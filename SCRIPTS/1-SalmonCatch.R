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
library(ggridges)
library(plot3D)
# Libraries

library(hrbrthemes)
library(viridis)

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
#df$CALENDAR_YEAR<-factor(df$CALENDAR_YEAR)# Print data to RStudio console
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
str(df3)
df3$LICENCE_AREA2<-factor(df3$LICENCE_AREA2)
#df3$Year<-factor(df3$CALENDAR_YEAR, order=T)

str(df3)
ggplot(df3, aes(fill=LICENCE_AREA2, y=Freq, x=CALENDAR_YEAR)) + 
  geom_bar(position="stack", stat="identity")


ggplot(df3, aes(CALENDAR_YEAR, Freq, height = Freq, group = LICENCE_AREA2), fill = (LICENCE_AREA2)) +
  geom_ridgeline_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
  coord_cartesian(clip = "off") +
  labs(title = 'Temperatures in Lincoln NE in 2016') +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(axis.title.y = element_blank())




fig.us <- plot_ly(df3, x = ~Year , y = ~Freq, z = ~Area, 
                  mode = 'lines', color=~Area)
fig.us

fig <- plot_ly(df3, x = ~LICENCE_AREA2, y = ~y, z = ~cut, type = 'scatter3d', mode = 'lines', color = ~cut)

ggplot(df3, aes(x = CALENDAR_YEAR, y = LICENCE_AREA2, group = LICENCE_AREA2)) +
  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03) +
  theme_ridges() +
  scale_x_continuous(limits = c(1, 200), expand = c(0, 0)) +
  scale_y_reverse(
    breaks = c(2000, 1980, 1960, 1940, 1920, 1900),
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off")





ggplot(df3, aes(CALENDAR_YEAR, LICENCE_AREA2, height = Freq, group = LICENCE_AREA2, fill = (LICENCE_AREA2))) +
  geom_ridgeline_gradient() +
  scale_fill_viridis_d(direction = -1, guide = "none")

p2 <- ggplot(data=df3, aes(x=Year, group=Area, fill=Area)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()
p2

