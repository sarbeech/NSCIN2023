library(glfishr)
library(dplyr)
library(ggplot2)
library(gfsR)
library(readxl)
library(tidyverse)
library(lakeontario)
library(rgdal)
library(sp)
library(maptools)
library(raster)

mytheme <- theme(
  axis.ticks = element_line(size = 1, colour = "black"),
  axis.text.x = element_text(size = 16, colour = "black"),
  axis.text.y = element_text(size = 16, colour = "black"),
  axis.title = element_text(size = 22, colour = "black", vjust = -0.5),
  panel.background = element_blank(),
  axis.line = element_line(colour = 'black', size = 1))
historicalCUE <- read_excel("./Data/CUESummary2001to2023_ARupdate.xlsx") %>%
  mutate(Species = ifelse(nchar(Species) == 2, as.character(paste("0", Species, sep = "")), as.character(Species)))

fish <- "331"
historicalCUE %>%
  filter(Species == fish, 
         EMB %in% c("Lake St. Francis", "Thousand Islands", "North Channel Kingston")) %>%
  group_by(Year) %>%
  summarise(meanCUE = mean(AMEAN)) %>%
  ggplot(aes(x = Year, y = meanCUE)) +
  geom_point(size = 3) +
  geom_line(data = . %>% filter(Year >= 2007 & Year <= 2009),
            color = "black", size = 1) +
  scale_y_continuous(name = "CPUE (Catch per Trapnet)", breaks = seq(0, 10, 5), limits = c(0, 10)) +
  scale_x_continuous(name = "Year", breaks = seq(2005, 2025, 5), limits = c(2005, 2025)) +
  mytheme +
  theme(legend.position = "none")
