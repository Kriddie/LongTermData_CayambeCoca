#Supplementary Figure 2
library(here)
library(dplyr)
library(ggplot2)

#read in data
stn01_DO_df <- read.csv(here::here("Whitmore_etal_Biogeochemistry/DO_data_stn01.csv"))

#plot
p <- ggplot(stn01_DO_df%>%drop_na(WL_m,DO_mgL),aes(x=WL_m*100,y=DO_mgL)) + geom_point() +
  ylab(expression(paste("DO (mg"~L^-1~")"))) +
  xlab("Water level (cm)") +
  geom_hline(yintercept = 0,color="red",linetype="dashed") +
  geom_hline(yintercept = 3,color="red",linetype="dashed") +
  theme_bw(base_size = 14)
