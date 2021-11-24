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
library(ggrgl)
library(tidyverse)
library(ggrepel)
library(ggtext)
library(showtext)
font_add_google("Lato")
showtext_auto()

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

ggplot(df3) + 
  geom_line_3d(aes(x=CALENDAR_YEAR, fill =LICENCE_AREA2), position="stack", z=200,extrude=TRUE,colour='black')

pp <- streamgraph(data, key="name", value="value", date="year", height="300px", width="1000px") %>%
  sg_legend(show=TRUE, label="names: ")

plt <- ggplot(
  # The ggplot object has associated the data for the highlighted countries
  df3, 
  aes(CALENDAR_YEAR, Freq, group = LICENCE_AREA2)
) + 
  # Geometric annotations that play the role of grid lines
  geom_vline(
    xintercept = seq(1996, 2020, by = 5),
    color = "grey91", 
    size = .6
  ) +
  geom_segment(
    data = tibble(y = seq(-4, 3, by = 1), x1 = 1996, x2 = 2020),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey91",
    size = .6
  ) +
  geom_segment(
    data = tibble(y = 0, x1 = 1996, x2 = 2020),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey60",
    size = .8
  ) +
  
  
  ## Lines for the highlighted countries.
  # It's important to put them after the grey lines
  # so the colored ones are on top
  geom_line(
    aes(color = LICENCE_AREA2),
    size = .9
  )
plt

plt <- plt + 
   geom_text_repel(
    aes(color = LICENCE_AREA2, label = LICENCE_AREA2),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2020.8, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
  ## coordinate system + scales
  coord_cartesian(
    clip = "off",
    ylim = c(-4, 3)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(2000, 2023.5), 
    breaks = seq(2000, 2020, by = 5)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(-4, 3, by = 1),
    labels = glue::glue("{format(seq(-4, 3, by = 1), nsmall = 2)}$")
  )
plt 

df3%>%
lines3D(x=df3$CALENDAR_YEAR, y=df3$LICENCE_AREA2 , z=df3$Freq)

scatter3D(df3$CALENDAR_YEAR, df3$Freq, df3$LICENCE_AREA2, bty = "g", type = "l", 
          ticktype = "detailed", lwd = 4)

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

fig <- plot_ly(df3, x = ~LICENCE_AREA2, y = ~Freq, z = ~cut, type = 'scatter3d', mode = 'lines', color = ~cut)

fig <- plot_ly(data, x = ~x, y = ~y, z = ~cut, type = 'scatter3d', mode = 'lines', color = ~cut)

fig



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

p2 <- ggplot(data=df3, aes(x=CALENDAR_YEAR, y=Freq,group=LICENCE_AREA2, fill=LICENCE_AREA2)) +
  geom_line(stat="identity"), geom_density(adjust=1.5, alpha=.4) 

+
  theme_ipsum()
p2


stat = "identity",
