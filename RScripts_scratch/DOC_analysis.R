#this script is for the exploration for DOC data collected in 2019 and 2021 at Cayambe Coca

library(here)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gridExtra)

DOC_2021 <- read.csv(here::here("DOC/DOC_9-1-2021_UNC_EDIT3.csv"))%>%select(Date,Time,Station,SampleType,DOC,TDN)
DOC_synop <-  read.csv(here::here("DOC/WaterChem_synoptic_2022-01-28.csv")) 
DOC_2019 <- read.csv(here::here("DOC/WaterChem_2019_EDIT.csv"))%>%select(Date,Time,Station,DOC..mg.L.,TDN..mg.L.)%>%rename(
  DOC=DOC..mg.L.,TDN=TDN..mg.L.)%>%drop_na(DOC)
DOC_2019$SampleType <- "Station"
DOC_df <- rbind(DOC_2021,DOC_2019)

DOC_df$DateTime <- as.POSIXct(paste(DOC_df$Date, DOC_df$Time), format="%m/%d/%y %H:%M", tz = "UTC")
DOC_df$DateTime <- round_date(DOC_df$DateTime,unit="15 minutes")

#c6 data
C6_df <-read.csv(here::here("DOC/C6_df.csv"))
C6_df$DateTime <- as.POSIXct(C6_df$DateTime, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
C6_df$DateTime <- round_date(C6_df$DateTime, unit="15 minute")
C6_df <- C6_df%>%drop_na(DateTime)
C6_df <- unique(C6_df)

p <- ggplot(DOC_df%>%drop_na(Station)%>%drop_na(DOC)%>%filter(Station!="ssoutlet") , aes(x=Station, y=DOC)) + 
  geom_boxplot()
p

p2 <- ggplot(DOC_2021%>%drop_na(Station) , aes(x=Station, y=TDN)) + 
  geom_boxplot()
p2

p3 <- ggplot(DOC_2021%>%drop_na(Station) , aes(x=DOC, y=TDN)) + 
  geom_point(aes(color=Station),size=3)
p3

#################
##Long term data#
#################
#### station 2
WL_df <- read.csv(here::here("data_cleaned/WL_02_cleaned.csv"))
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")

##merge with doc
mergeDOC_df <- left_join(DOC_df,WL_df%>%select(!Station), by=(c("DateTime")))

#merge C with station01 Q
C6_Q_df <- left_join(C6_df,WL_df%>%select(!Station),by="DateTime")
C6_Q_df <- C6_Q_df%>%filter(DateTime != as.POSIXct("2019-07-15 14:45",tz="UTC"))%>%
  filter(DateTime != as.POSIXct("2019-07-15 15:15",tz="UTC"))%>%
  filter(DateTime != as.POSIXct("2019-07-15 15:45",tz="UTC"))%>%
  filter(DateTime != as.POSIXct("2019-07-15 21:00",tz="UTC"))%>%
  filter(DateTime != as.POSIXct("2019-07-15 22:00",tz="UTC"))%>%
  filter(DateTime != as.POSIXct("2019-07-15 22:15",tz="UTC"))%>%
  filter(DateTime != as.POSIXct("2019-07-15 23:00",tz="UTC"))
C6_Q_df <- C6_Q_df[order(as.Date(C6_Q_df$DateTime, format="%Y-%m-%d %H:%M:%S")),]
#merge c6 (I think I will do this manually because otherwise it is too complicated)
C6_Q_df$DOC <- NA
C6_Q_df$TDN <- NA

#this sample was collected 2019-07-11 14:00
C6_Q_df[C6_Q_df$DateTime==as.POSIXct("2019-07-11 15:30:00",tz="UTC"), "DOC"] <- 3.7630
C6_Q_df[C6_Q_df$DateTime==as.POSIXct("2019-07-11 15:30:00",tz="UTC"), "TDN"] <- 0.11140

#This sample was collected at 2019-07-16 14:30:00
C6_Q_df[C6_Q_df$DateTime==as.POSIXct("2019-07-16 08:45:00",tz="UTC"), "DOC"] <- 4.6100
C6_Q_df[C6_Q_df$DateTime==as.POSIXct("2019-07-16 08:45:00",tz="UTC"), "TDN"] <- 0.16920

#This sample was collected at 2019-07-19 13:15:00
C6_Q_df[C6_Q_df$DateTime==as.POSIXct("2019-07-19 13:00:00",tz="UTC"), "DOC"] <- 3.7400
C6_Q_df[C6_Q_df$DateTime==as.POSIXct("2019-07-19 13:00:00",tz="UTC"), "TDN"] <- 0.10280

ggplot(C6_Q_df%>%drop_na(DOC))+
  geom_point(aes(x=CDOM_ppb,y=DOC),size=3) +
  ylab("DOC [mg/l]") + xlab("CDOM [ppb]") +
  scale_x_continuous(transform = "log") + 
  scale_y_continuous(transform = "log") +
  theme_bw()+theme(text=element_text(size=21))

#######
#now plot we are interested in the relationship between DOC and Q
############
p3 <- ggplot(mergeDOC_df%>%
               filter(Station=="Stn01"|Station=="Stn02"|Station=="Stn03"|Station=="Stn04"|Station=="Stn05"|Station=="Stn06")%>%
               filter(DateTime > "2020-01-01 00:00:00"))+
  geom_point(aes(x=Q_m3s, y=DOC, fill=Station), shape=21,size=3) + 
  ylab("DOC [mg/]") + xlab("Q [m^3/s]") 
#+
#  facet_wrap(~WS, scales = "free_x")
p3+ theme_bw()+theme(text=element_text(size=18))

p3.01 <- ggplot(mergeDOC_df%>%
                 filter(Station=="Stn01")%>%
                 filter(DateTime < "2020-01-01 00:00:00"))+
  geom_point(aes(x=Q_m3s, y=DOC, fill=Station), shape=21,size=3) + 
  ylab("DOC [mg/]") + xlab("Q [m^3/s]")
p3.02 <- ggplot(mergeDOC_df%>%
                 filter(Station=="Stn02")%>%
                 filter(DateTime > "2020-01-01 00:00:00"))+
  geom_point(aes(x=Q_m3s, y=DOC, fill=Station), shape=21,size=3) + 
  ylab("DOC [mg/]") + xlab("Q [m^3/s]")

p3.01 <- ggplot(mergeDOC_df%>%
                  filter(Station=="Stn02"#|
                   # Station=="Stn01"
                   )
                   )+
  geom_point(aes(x=Q_m3s, y=DOC, fill=DateTime), shape=21,size=3) + 
  ylab("DOC [mg/]") + xlab("Q [m^3/s]")
p3.01+ theme_bw()+theme(text=element_text(size=18))

p3.1 <- ggplot(mergeDOC_df %>%
                 filter(Station=="Stn01"|Station=="Stn02"|Station=="Stn03"|Station=="Stn04"|Station=="Stn05"|Station=="Stn06")%>%
                 filter(DateTime > "2020-01-01 00:00:00"))+
  geom_point(aes(x=WLTemp_c, y=DOC, fill=Station), shape=21,size=3) + 
  ylab("DOC [mg/]") + xlab("Water Temp [c]") +
  facet_wrap(~WS, scales = "free_x")
p3.1+ theme_bw()+theme(text=element_text(size=18))

p3.2 <- ggplot(mergeDOC_df %>%
                 filter(Station=="Stn01"|Station=="Stn02"|Station=="Stn03"|Station=="Stn04"|Station=="Stn05"|Station=="Stn06")%>%
                 filter(DateTime > "2020-01-01 00:00:00"))+
  geom_point(aes(x=WLTemp_c, y=Q_m3s, fill=Station), shape=21,size=3) + 
  facet_wrap(~WS, scales = "free_x")
p3.2

#plot c6 data
fig1 <- plot_ly(data = C6_Q_df , x = ~DateTime, y = ~Q_m3s)
fig1
fig2 <- plot_ly(data = C6_Q_df, x = ~DateTime, y = ~CDOM_ppb)
fig2

fig3 <- ggplot(C6_Q_df) + geom_point(aes(x=DateTime,y=Q_m3s)) + theme(text=element_text(size=18))
###########################
#hysteresis of storm event
############################
#start 
start <- as.POSIXct("2019-07-14 00:15",tz="UTC")
end <- as.POSIXct("2019-07-16 09:00",tz="UTC")


C6_p1 <- ggplot(C6_Q_df%>%filter(DateTime>start&DateTime<end) ) + geom_point(aes(x=DateTime,y=CDOM_ppb,color=DateTime))+
  scale_color_gradient(low="blue", high="red")+ theme_bw() + theme(legend.position = "none") + theme(text=element_text(size=18))
C6_p2 <- ggplot(C6_Q_df%>%filter(DateTime>start&DateTime<end)) + geom_point(aes(x=DateTime,y=Q_m3s,color=DateTime))+
  scale_color_gradient(low="blue", high="red")+ theme_bw() +theme(legend.position = "none") + theme(text=element_text(size=18))
grid.arrange(C6_p1, C6_p2) 
#hysteresis
C6_p3 <- ggplot(C6_Q_df%>%filter(DateTime>start&DateTime<end) ) + 
  geom_point(aes(x=Q_m3s,y=CDOM_ppb,color=DateTime), size =4)
# Sequential color scheme
C6_Q_df$time <- as.integer(C6_Q_df$DateTime)
labels <- pretty(C6_Q_df$DateTime, 20)
C6_p3+
  scale_color_gradient(
    low="red", high="blue", 
    breaks = as.integer(labels), 
    labels = format(labels, "%m/%d %H:%M")
  ) +
  theme_bw() + 
  theme(text=element_text(size=18))

#what about non storm events?
C6_p4.1 <- ggplot(C6_Q_df%>%filter(DateTime<start|DateTime>end)%>%filter(CDOM_ppb>30) ) + 
  geom_point(aes(x=Q_m3s,y=CDOM_ppb,color=DateTime), size =4)
C6_p4.1
C6_p4.2 <- ggplot(C6_Q_df %>%filter(DateTime<start|DateTime>end)%>%filter(CDOM_ppb>30) ) + 
  geom_point(aes(x=Q_m3s,y=CDOM_ppb,color=DateTime), size =4)
C6_p4.2
C6_p4.3 <- ggplot(C6_Q_df %>%filter(DateTime<start|DateTime>end)%>%filter(CDOM_ppb>30) ) + 
  geom_point(aes(x=Q_m3s,y=WLTemp_c,color=DateTime), size =4)
C6_p4.3

#select one diurnal thing
#start 
start <- as.POSIXct("2019-07-18 06:00",tz="UTC")
end <- as.POSIXct("2019-07-19 06:30",tz="UTC")
C6_p5.1 <- ggplot(C6_Q_df%>%filter(DateTime>start&DateTime<end) ) + 
  geom_point(aes(x=Q_m3s,y=CDOM_ppb,color=DateTime), size =4)
C6_p5.1
#start 
start <- as.POSIXct("2019-07-13 05:45",tz="UTC")
end <- as.POSIXct("2019-07-14 06:40",tz="UTC")
C6_p5.2 <- ggplot(C6_Q_df%>%filter(CDOM_ppb>28.83)%>%filter(DateTime>start&DateTime<end) ) + 
  geom_point(aes(x=Q_m3s,y=CDOM_ppb,color=DateTime), size =4)
C6_p5.2
#start 
start <- as.POSIXct("2019-07-12 06:30",tz="UTC")
end <- as.POSIXct("2019-07-13 06:30",tz="UTC")
C6_p5.3 <- ggplot(C6_Q_df%>%filter(CDOM_ppb>28.83)%>%filter(DateTime>start&DateTime<end) ) + 
  geom_point(aes(x=Q_m3s,y=CDOM_ppb,color=DateTime), size =4)
C6_p5.3


#ricardo
fig1
fig2
