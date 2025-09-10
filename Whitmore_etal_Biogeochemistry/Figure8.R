#figure 8
library(here)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)

all_stn <- read.csv(here::here("Whitmore_etal_Biogeochemistry/allstation_data_3.csv"))
all_stn$DateTime <- as.POSIXct(all_stn$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")

p4_stn4 <- ggplot() + 
  geom_point(data=all_stn%>%drop_na(CO2_ppm_01,CO2_ppm_04)%>%filter(DateTime>"2021-06-06 00:00:00"), aes(x=CO2_ppm_01,y=CO2_ppm_04,color=month)) +
  scale_y_continuous(transform = "log", breaks = c(300,1000,3000,10000,25000))+
  scale_x_continuous(transform = "log", breaks = c(300,1000,3000,10000,25000),limits = c(1000,26000))+
  xlab(expression('Downstream'~italic(p)*CO[2] ~'(ppm)')) +
  ylab(expression(~italic(p)*CO[2] ~'(ppm)')) + 
  annotate("text", x = 1800, y = 11000, label = "Station 4") +
  theme_bw(base_size = 14) +  geom_abline(intercept = 0, slope = 1,linetype="dashed", linewidth=1)

#all_stn_remove_3 <- all_stn%>%rename(Month=month)
p3_stn3 <- ggplot() + 
  geom_point(data=all_stn%>%drop_na(CO2_ppm_01,CO2_ppm_03)
             ,aes(x=CO2_ppm_01,y=CO2_ppm_03,color=month)) +
  scale_y_continuous(transform = "log", breaks = c(300,1000,3000,5000,10000,25000))+
  scale_x_continuous(transform = "log", breaks = c(300,1000,3000,10000,25000),limits = c(1000,26000))+
  xlab(expression('Downstream'~italic(p)~CO[2] ~'(ppm)')) +
  ylab(expression(~italic(p)*CO[2] ~'(ppm)')) + 
  annotate("text", x = 1800, y = 13000, label = "Station 3") +
  theme_bw(base_size = 14) +  geom_abline(intercept = 0, slope = 1,linetype="dashed",linewidth=1) 

p2_stn2 <- ggplot() + 
  geom_point(data=all_stn%>%drop_na(CO2_ppm_01,CO2_ppm_02)
             ,aes(x=CO2_ppm_01,y=CO2_ppm_02,color=month)) +
  scale_y_continuous(transform = "log", breaks = c(300,1000,3000,10000,15000,25000))+
  scale_x_continuous(transform = "log", breaks = c(300,1000,3000,10000,25000),limits = c(1000,26000))+
  xlab(expression('Downstream'~italic(p)*CO[2] ~'(ppm)')) +
  ylab(expression(~italic(p)*CO[2] ~'(ppm)')) + 
  annotate("text", x = 1800, y = 10000, label = "Station 2") +
  theme_bw(base_size = 14) +  geom_abline(intercept = 0, slope = 1,linetype="dashed",linewidth=1)

#plot all
plot_all <- ggarrange(p2_stn2+ rremove("xlab"),p3_stn3+ rremove("xlab"),p4_stn4, ncol=1, nrow=3, common.legend = TRUE, legend="right",labels="auto",heights = c(1,1,1.1))

