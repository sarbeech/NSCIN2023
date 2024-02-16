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
library(cowplot)


mytheme <- theme(
  axis.ticks = element_line(size = 1, colour = "black"),
  axis.text.x = element_text(size = 16, colour = "black"),
  axis.text.y = element_text(size = 16, colour = "black"),
  axis.title = element_text(size = 22, colour = "black", vjust = -0.5),
  panel.background = element_blank(),
  axis.line = element_line(colour = 'black', size = 1))
historicalCUE <- read_excel("./Data/CUESummary2001to2023_ARupdate.xlsx") %>%
  mutate(Species = ifelse(nchar(Species) == 2, as.character(paste("0", Species, sep = "")), as.character(Species)))

fish <- c("319")

p1<-historicalCUE %>%
  filter(Species %in% c("313", "314"), 
         EMB %in% c("Lake St. Francis")) %>%
  group_by(Year) %>%
  summarise(meanCUE = mean(AMEAN),
            sum = sum(AMEAN)) %>%
  ggplot(aes(x = Year, y = sum)) +
  geom_point(size = 5) +
  geom_line(data = . %>% filter(Year >= 2007 & Year <= 2009),
            color = "black", size = 1) +
  scale_y_continuous(name = "", breaks = seq(0, 40, 10), limits = c(0, 40)) +
  scale_x_continuous(name = "", breaks = seq(2005, 2025, 5), limits = c(2005, 2025)) +
  mytheme +
  geom_text(aes(label = "Lake St. Francis", x = 2023, y = 10), color = "black", size = 7, vjust = -0.5) +
  theme(legend.position = "none");p1

p2<-historicalCUE %>%
  filter(Species%in%c("313", "314") & 
         EMB %in% c("Thousand Islands")) %>%
  group_by(Year) %>%
  summarise(
            meanCUE = mean(AMEAN),
            sum = sum(AMEAN)) %>%
  ggplot(aes(x = Year, y = sum)) +
  geom_point(size = 5) +
  scale_y_continuous(name = "CPUE (Catch per Trapnet)", breaks = seq(0, 60, 20), limits = c(0, 60)) +
  scale_x_continuous(name = "", breaks = seq(2005, 2025, 5), limits = c(2005, 2025)) +
  geom_text(aes(label = "Thousand Islands", x = 2023, y = 15), color = "black", size = 7, vjust = -0.5) +
  mytheme +
  theme(legend.position = "none");p2

p3<-historicalCUE %>%
  filter(Species%in%c("313", "314") & 
         EMB %in% c("North Channel Kingston")) %>%
  group_by(Year) %>%
  summarise(meanCUE = mean(AMEAN),
            sum = sum(AMEAN)) %>%
  ggplot(aes(x = Year, y = sum)) +
  geom_point(size = 5) +
  geom_line(data = . %>% filter(Year >= 2007 & Year <= 2009),
            color = "black", size = 1) +
  scale_y_continuous(name = "", breaks = seq(0, 10, 2), limits = c(0, 10)) +
  scale_x_continuous(name = "Year", breaks = seq(2005, 2025, 5), limits = c(2005, 2025)) +
  geom_text(aes(label = "North Channel Kingston", x = 2023, y = 2), color = "black", size = 7, vjust = -0.5) +
  mytheme +
  theme(legend.position = "none");p3

tiff(filename = paste("./Data/Exports/SpeciesCUEStockStatus/Fish_", fish, ".tiff"), units = "in", width = 12, height = 8.8, res = 250, compression = "lzw")
plot_grid(p1,p2,p3,  nrow = 3, ncol = 1, align = "hv")
dev.off()
