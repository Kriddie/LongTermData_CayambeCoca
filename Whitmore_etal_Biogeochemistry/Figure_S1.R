#Supplementary Figure 1
library(here)
library(dplyr)
library(ggplot2)

CDOM_DOC_df <- read.csv(here::here("Whitmore_etal_Biogeochemistry/CDOM_data.csv"))%>%
  drop_na(calibrate_DOC)

#plot
p <- ggplot(CDOM_DOC_df)+
  geom_point(aes(x=CDOM_ppb, y=DOC
  ),fill="black",shape=21,size=3) +
  ylab(expression(paste('DOC (mg ' , L^-1,")"))) +
  xlab("CDOM (ppb)") +
  theme_bw(base_size = 14)
