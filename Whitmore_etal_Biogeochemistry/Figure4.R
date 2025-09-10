#Figure 4
library(here)
library(dplyr)
library(ggplot2)
library(cowplot)
library(lubridate)
library(ggpubr)

C6_Q_df_storm <- read.csv(here::here("Whitmore_etal_Biogeochemistry/CDOM_data.csv"))%>%
  drop_na(storm)

#plot CDOM hysteresis
C6_p3 <- ggplot(C6_Q_df_storm) + 
  geom_point(aes(x=Q_m3s*1000,y=CDOM_ppb,color=time_elapsed/60/24),size=4)+
  scale_color_viridis_c(name= "days since\nstart of storm",guide=guide_colourbar(reverse = TRUE)) +
  scale_y_continuous(breaks=c(30,35,40,45,50),labels=c("30.0","35.0","40.0","45.0","50.0")) +
  ylab('CDOM (ppb)') +  xlab(expression(paste('Discharge (L ' , s^-1,")"))) +
  theme_bw(base_size = 14)+
  annotate('curve', x = 170, y = 45, xend = 100, yend = 50,
           linewidth = 1, curvature = 0.3, arrow = arrow(length = unit(0.5, 'cm'))) +
  annotate('curve', x = 125, y = 29.5, xend = 175, yend = 34, linewidth = 1, curvature = 0.2, arrow = arrow(length = unit(0.5, 'cm')))

#plot pCO2 hysteresis
CO2_c6_hyst <- ggplot(C6_Q_df_storm) + 
  geom_point(aes(x=Q_m3s*1000,y=pCO2_ppm,color=time_elapsed/60/24),size=4)+
  scale_color_viridis_c(name= "days since\nstart of storm", guide=guide_colourbar(reverse = TRUE)) +
  ylab(expression(~italic(p)*CO[2] ~'(ppm)')) + 
  xlab(expression(paste('Discharge (L ' , s^-1,")"))) +
  theme_bw(base_size = 14)+
  annotate('curve', x = 75, y = 4000, xend = 120, yend = 3000,
           linewidth = 1, curvature = -0.1, arrow = arrow(length = unit(0.5, 'cm'))) +
  annotate('curve', x = 75, y = 1100, xend = 50, yend = 1600, linewidth = 1, curvature = -0.1, arrow = arrow(length = unit(0.5, 'cm')))

#extract legend
legend <- get_legend(
  # create some space to the left of the legend
  C6_p3 + theme(legend.box.margin = margin(0, 0, 0, 12))
)

#plot together
plot_hyst_1 <- plot_grid(
  C6_p3+rremove("xlab")+rremove("legend"),        
  CO2_c6_hyst+rremove("legend"), 
  labels = "auto",ncol=1,align = "v")

#plot together with legend
p_final <- plot_grid(plot_hyst_1,legend,ncol = 2,rel_widths = c(1,.4))

