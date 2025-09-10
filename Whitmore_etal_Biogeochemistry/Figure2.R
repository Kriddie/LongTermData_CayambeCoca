#figure 2
library(here)
library(dplyr)
library(ggplot2)
library(viridis)

#read in data
all_storm <- read.csv(here::here("Whitmore_etal_Biogeochemistry/stn01_storm_data.csv"))

#filter for storms 2021-2022 and format name
storm_subset <- all_storm%>%filter(hyst_analysis=="yes")
storm_subset$storm_name_1= factor(storm_subset$storm_name, levels=c("July 2019","Aug. 2019","June 2021","July 2021","Aug. 2021","Sept. 2021","Oct. 2021","Nov. 2021","Dec. 2021","April 2022","May 2022","Oct. 2022","Nov. 2022")) 

#plot
p <- ggplot(storm_subset, aes(x=Q_m3s*1000,y=pCO2_ppm,color=time_elapsed_day))+ 
  facet_wrap( ~ storm_name_1#, scales = "free"
  )  + geom_point() + 
  scale_y_continuous(transform = "log", breaks = c(2000,6000,16000))+ scale_x_continuous(transform = "log", breaks = c(3,10,30,100,300))+
  scale_color_viridis(option = "H",name= "days since\nstart of storm",guide=guide_colourbar(reverse = TRUE)) +
  ylab(expression(italic(p)*CO[2] ~'(ppm)')) +     xlab(expression(paste('Discharge (L ' , s^-1,")"))) +
  theme_bw(base_size = 14)

