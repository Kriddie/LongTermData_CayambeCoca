#Figure 5
library(here)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)

CH4_plot_df <- read.csv(here::here("Whitmore_etal_Biogeochemistry/methane_df.csv"))

DOC_df <- read.csv(here::here("Whitmore_etal_Biogeochemistry/DOC_df.csv"))
DOC_df$DateTime <- as.POSIXct(DOC_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")
DOC_df$DateTime <- round_date(DOC_df$DateTime,unit="15 minutes")

DOC_PeatlandOutlet <- DOC_df%>%filter(DateTime > "2021-01-01")%>%filter(Station=="Stn02")


all_stn <- read.csv(here::here("Whitmore_etal_Biogeochemistry/allstation_data_3.csv"))%>%select(DateTime,Q_m3s_02)%>%rename(Q_m3s=Q_m3s_02)
all_stn$DateTime <- as.POSIXct(all_stn$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")
all_stn <- all_stn%>%filter(DateTime>as.POSIXct("2020-01-01 00:00"))

DOC_PeatlandOutlet <- left_join(DOC_PeatlandOutlet,all_stn#%>%select(!Station)
                         , by=(c("DateTime")))

#Figure 5 a
DOC_PeatlandOutlet$Date_format <- as.Date(DOC_PeatlandOutlet$Date,format = "%m/%d/%y")
DOC_PeatlandOutlet$Date_plot <- as.character(format(DOC_PeatlandOutlet$Date_format,format="%Y %B %d"))
DOC_PeatlandOutlet$Date_plot <- reorder(DOC_PeatlandOutlet$Date_plot, DOC_PeatlandOutlet$Date_format)

DOC_temporal <- ggplot(DOC_PeatlandOutlet %>%filter(DateTime  > as.POSIXct("2020-01-01 00:00")))+
  geom_point(aes(x=Q_m3s*1000, y=DOC#,fill=Date_plot
  ),fill="black",shape=21,size=3) +   geom_smooth(aes(x=Q_m3s*1000, y=DOC),method='lm',color="grey50",linewidth=.5 ,se = FALSE) +
  ylab(expression(paste('DOC (mg ' , L^-1,")"))) +
  xlab(expression(paste('Discharge (L ' , s^-1,")"))) +
  scale_x_continuous(transform = "log",breaks=c(10,30,100,300,1000)) + scale_y_continuous(transform = "log",breaks=c(1,2,3,4,5,6,7)) +
  labs(fill="Date") +
  theme_bw(base_size = 14)+
  annotate("text", label = paste0("atop('adj. '* R^2==0.87,p-value<.0001)"),
           x = 100, y = 2.6, size = 3, parse = TRUE)


###Figure 5 b
#methane
CH4_plot_df$Date_format <- as.Date(CH4_plot_df$DateTime,format = "%m/%d/%y")
CH4_plot_df$Date_plot <- as.character(format(CH4_plot_df$Date_format,format="%Y %B %d"))
CH4_plot_df$Date_plot <- reorder(CH4_plot_df$Date_plot, CH4_plot_df$Date_format)

p1_ch4 <- ggplot(CH4_plot_df%>%drop_na(DO_mgL)%>%filter(Site2!="50m")%>%filter(Site2!="100m")) +
  geom_smooth(method='lm',aes(x=DO_mgL,y=pCH4_ppm,linetype=Site),linewidth=.5 ,se = FALSE)  +
  geom_point(aes(x=DO_mgL,y=pCH4_ppm#,fill=Date_plot
  ),fill="black",size=3,shape=21) +
  ylab(expression(italic(p)*CH[4] ~'(ppm)')) + xlab("DO at outlet (mg/l)")  + labs(linetype="") +
  scale_y_log10() + scale_x_continuous(transform = "log10",limits = c(4.5,7.5)) +
  theme_bw(base_size = 14) +
  guides(linetype=guide_legend(nrow=2,byrow=TRUE)) +
  theme(legend.position = "top")+
  
  annotate("text", label = paste0("atop('adj. '* R^2==1.0,p-value==0.02)"),
           x = 6.5, y = 1500/(60/101), size = 3, parse = TRUE) +
  annotate("text", label = paste0("atop('adj. '* R^2==0.54,p-value==0.32)"),
           x = 6.5, y = 280/(60/101), size = 3, parse = TRUE)


##figure 5 all
temporal_full_plot <- ggarrange(DOC_temporal,p1_ch4#,common.legend = TRUE
)

