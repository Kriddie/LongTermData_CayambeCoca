##Supplementary Figure 3
library(here)
library(dplyr)
library(scales)
library(ggplot2)
library(tidyr)

#read in data
all_stn <- read.csv(here::here("Whitmore_etal_Biogeochemistry/allstation_data_3.csv"))
all_storm <- read.csv(here::here("Whitmore_etal_Biogeochemistry/stn01_storm_data.csv"))

#order by month
all_stn$month <- reorder(all_stn$month, all_stn$month_number)
all_storm$month <- reorder(all_storm$month, all_storm$month_number)

#plot
hex <- hue_pal()(12)
p<-ggplot() + geom_point(data=all_stn%>%drop_na(Q_m3s_02,CO2_ppm_01),aes(x=Q_m3s_02*1000,y=CO2_ppm_01*Q_m3s_02*1000),color="grey") +
  geom_point(data=all_storm,aes(x=Q_m3s*1000,y=pCO2_ppm*Q_m3s*1000,color=month)) +
  scale_color_manual(values=c(hex[4],hex[5],hex[6],hex[7],hex[8],hex[9],hex[10],hex[11],hex[12])) +
  scale_x_continuous(transform = "log", breaks = c(3,10,30,100,300))+ ylab(expression(italic(p)~CO[2] ~'load (ppm*L/s)')) +     xlab(expression(paste('Discharge (L ' , s^-1,")"))) +
  theme_bw(base_size = 14) 
