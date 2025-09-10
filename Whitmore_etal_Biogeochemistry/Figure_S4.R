#Supplimentary Figure 4
library(here)
library(dplyr)
library(ggplot2)
library(lubridate)

DOC_df <- read.csv(here::here("Whitmore_etal_Biogeochemistry/DOC_df.csv"))
DOC_df$DateTime <- as.POSIXct(DOC_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")
DOC_df$DateTime <- round_date(DOC_df$DateTime,unit="15 minutes")

DOC_PeatlandOutlet <- DOC_df%>%filter(DateTime > "2021-01-01")%>%filter(Station=="Stn02")

all_stn <- read.csv(here::here("Whitmore_etal_Biogeochemistry/allstation_data_3.csv"))%>%select(DateTime,Q_m3s_02)%>%rename(Q_m3s=Q_m3s_02)
all_stn$DateTime <- as.POSIXct(all_stn$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")
all_stn <- all_stn%>%filter(DateTime>as.POSIXct("2020-01-01 00:00"))

mergeDOC_df <- left_join(DOC_PeatlandOutlet,all_stn#%>%select(!Station)
                         , by=(c("DateTime")))

#plot
TDN_temporal <- ggplot(mergeDOC_df)+
  geom_point(aes(x=Q_m3s*1000, y=TDN#,fill=Date_plot
  ),fill="black",shape=21,size=3) +   geom_smooth(aes(x=Q_m3s*1000, y=TDN),method='lm',color="grey50",linewidth=.5 ,se = FALSE) +
  ylab(expression(paste('TDN (mg ' , L^-1,")"))) +
  xlab(expression(paste('Discharge (L ' , s^-1,")"))) +
  scale_x_continuous(transform = "log",breaks=c(10,30,100,300,1000)) + scale_y_continuous(transform = "log"#,breaks=c(1,2,3,4,5,6,7)
  ) +
  labs(fill="Date") +
  theme_bw(base_size = 14)+
  annotate("text", label = paste0("atop('adj. '* R^2==0.53,p-value==0.03)"),
           x = 100, y = .08, size = 3, parse = TRUE)
