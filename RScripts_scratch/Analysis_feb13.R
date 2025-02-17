#read in data
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(zoo)
library(plotly)
library(EcoHydRology)

#precipitation

precip_df <- read.csv(here::here("Weather_station/M5025_Precipitation.csv"))%>%rename(DateTime=fecha)
precip_df$DateTime <- as.POSIXct(precip_df$DateTime,format="%m/%d/%y %H:%M",tz="UTC")
# Convert minute column to datetime. Find the hour of the minute column using floor_date 
precip_df$DateTime_hour <-  round_date(precip_df$DateTime, unit = "hour")                                   

precip_df_hour <- precip_df %>% select(DateTime_hour,precipt_mm)%>%
  group_by(DateTime_hour) %>% 
  summarise(precipt_mm = sum(precipt_mm))%>%rename(
    DateTime=DateTime_hour
  )

precip_df_hour$Date <-  as.Date(precip_df_hour$DateTime)   

precip_df_day <- precip_df_hour %>% select(Date,precipt_mm)%>%
  group_by(Date) %>% 
  summarise(precipt_mm = sum(precipt_mm))

#now add previous amount of precipitation from day previous
precip_df_day$Date_used <- precip_df_day$Date
precip_df_day$Date <- precip_df_day$Date - 1
precip_summar_previous <- precip_df_day[,c("Date","precipt_mm")]
colnames(precip_summar_previous) <- c("Date","PrecipAccuPreviousDay_mm")
precip_df_day <- precip_df_day[,c("Date_used","precipt_mm")]
colnames(precip_df_day) <- c("Date","precipt_mm")

precip_df_day <- full_join(precip_df_day,precip_summar_previous, by="Date")


#I would like to do a 7 day average 
precip_df_day <- transform(precip_df_day, avg2 = rollmeanr(precipt_mm, 2, fill = NA,na.rm=TRUE))
precip_df_day <- transform(precip_df_day, avg3 = rollmeanr(precipt_mm, 3, fill = NA,na.rm=TRUE))
precip_df_day <- transform(precip_df_day, avg5 = rollmeanr(precipt_mm, 5, fill = NA,na.rm=TRUE))


#colnames(precip_weekAve) <- c("Date","PrecipAccuDay_mm","PrecipAccu_mm_PreviousDay","Precip_mm_ave3")
#add average precip of day and previous
#precip_weekAve$precip_mm_ave2 <- (precip_weekAve$PrecipAccu_mm_PreviousDay + precip_weekAve$PrecipAccuDay_mm)/2


ggplot(precip_df_day ,aes(x=Date,y=precipt_mm)) + geom_point()


####stn01
WL_df <- read.csv(here::here("data_cleaned/WL_01_cleaned.csv"))
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")
CO2_df <- read.csv(here::here("data_cleaned/CO2_01_max10000_cleaned.csv")) 
CO2_df$DateTime <- as.POSIXct(CO2_df$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")

stn01 <- full_join(WL_df,CO2_df,by=c("DateTime","Station"))

ggplot(stn01# %>%filter(DateTime>as.POSIXct("2021-01-01 00:00:00"))
       ,aes(x=DateTime,Q_Ls)) + geom_point()
ggplot(stn01#%>%filter(DateTime>as.POSIXct("2021-01-01 00:00:00"))
       ,aes(x=DateTime,CO2_ppm_adjusted)) + geom_point()

ggplot(stn01%>%filter(DateTime>as.POSIXct("2021-01-01 00:00:00")),aes(x=Q_Ls,CO2_ppm_adjusted)) + geom_point()
ggplot(stn01%>%filter(DateTime>as.POSIXct("2021-01-01 00:00:00")),aes(x=DateTime,Q_Ls)) + geom_point()
ggplot(stn01%>%filter(DateTime>as.POSIXct("2021-01-01 00:00:00")),aes(x=DateTime,CO2_ppm_adjusted*Q_Ls)) + geom_point()

fig <- plot_ly(data = stn01, x = ~DateTime, y = ~Q_Ls)
fig

#### station 2

WL_df <- read.csv(here::here("data_cleaned/WL_02_cleaned.csv"))
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")
CO2_df <- read.csv(here::here("data_cleaned/CO2_02_max10000_cleaned.csv")) 
CO2_df$DateTime <- as.POSIXct(CO2_df$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")

stn02 <- full_join(WL_df,CO2_df,by=c("DateTime","Station"))


ggplot(stn02#%>%filter(DateTime>as.POSIXct("2021-01-01 00:00:00"))
       ,aes(x=DateTime,Q_m3s)) + geom_point()
ggplot(stn02%>%filter(DateTime>as.POSIXct("2021-01-01 00:00:00")),aes(x=DateTime,CO2_ppm_adjusted)) + geom_point()
ggplot(stn02,aes(x=Q_m3s,CO2_ppm_adjusted,color=DateTime)) + geom_point()


### station 04
WL_df <- read.csv(here::here("data_cleaned/WL_04_cleaned.csv"))
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")
CO2_df <- read.csv(here::here("data_cleaned/CO2_04_max10000_cleaned.csv")) 
CO2_df$DateTime <- as.POSIXct(CO2_df$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")

stn04 <- full_join(WL_df,CO2_df,by=c("DateTime","Station"))


ggplot(stn04 %>%filter(DateTime>as.POSIXct("2021-01-01 00:00:00")),aes(x=DateTime,Q_m3s)) + geom_point()
ggplot(stn04%>%filter(DateTime>as.POSIXct("2021-01-01 00:00:00")),aes(x=DateTime,CO2_ppm_adjusted)) + geom_point()

ggplot(stn04%>%filter(DateTime>as.POSIXct("2021-01-01 00:00:00")),aes(x=Q_m3s,CO2_ppm_adjusted)) + geom_point()

###stn03
###
WL_df <- read.csv(here::here("data_cleaned/WL_03_cleaned.csv"))
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")
CO2_df <- read.csv(here::here("data_cleaned/CO2_03_max10000_cleaned.csv")) 
CO2_df$DateTime <- as.POSIXct(CO2_df$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")

stn03 <- full_join(WL_df,CO2_df,by=c("DateTime","Station"))
stn03 <- stn03%>%filter(DateTime > as.POSIXct("2021-03-15 03:30:00"))
###

all_stn <- full_join(stn01%>%select(DateTime,Q_m3s,CO2_ppm_adjusted)%>%rename(Q_m3s_01 = Q_m3s,CO2_ppm_01=CO2_ppm_adjusted),
                     stn02%>%select(DateTime,Q_m3s,CO2_ppm_adjusted)%>%rename(Q_m3s_02 = Q_m3s,CO2_ppm_02=CO2_ppm_adjusted),
                     by="DateTime")
all_stn <- full_join(all_stn,
                     stn03%>%select(DateTime,Q_m3s,CO2_ppm_adjusted)%>%rename(Q_m3s_03 = Q_m3s,CO2_ppm_03=CO2_ppm_adjusted),
                     by="DateTime")
all_stn <- full_join(all_stn,
                     stn04%>%select(DateTime,Q_m3s,CO2_ppm_adjusted)%>%rename(Q_m3s_04 = Q_m3s,CO2_ppm_04=CO2_ppm_adjusted),
                     by="DateTime")


ggplot(all_stn ,aes(x=Q_m3s_01,y=Q_m3s_02)) + geom_point() +
  scale_y_continuous(transform = "log") + scale_x_continuous(transform = "log") +
  geom_abline(intercept = 0, slope = 1)

ggplot(all_stn ,aes(x=CO2_ppm_01,CO2_ppm_02,color=log(Q_m3s_01))) + geom_point() +
  scale_y_continuous(transform = "log") + scale_x_continuous(transform = "log") +
  geom_abline(intercept = 0, slope = 1)

ggplot(all_stn ,aes(x=CO2_ppm_01,CO2_ppm_03)) + geom_point() +
  scale_y_continuous(transform = "log") + scale_x_continuous(transform = "log") +
  geom_abline(intercept = 0, slope = 1)

ggplot(all_stn ,aes(x=CO2_ppm_01,CO2_ppm_04,color=Q_m3s_04)) + geom_point() +
  scale_y_continuous(transform = "log") + scale_x_continuous(transform = "log") 

ggplot(all_stn ,aes(x=CO2_ppm_03,CO2_ppm_04)) + geom_point() +
  scale_y_continuous(transform = "log") + scale_x_continuous(transform = "log") 


### bind in precip data

stn01$Date <-  as.Date(stn01$DateTime)   
stn01_day <- stn01 %>% select(Date,Q_m3s,WLTemp_c)%>%
  group_by(Date) %>% 
  summarise(WLTemp_c_mean = mean(WLTemp_c),
            Q_m3s_mean =mean(Q_m3s))

stn01_day <- full_join(stn01_day,precip_df_day,by="Date")

ggplot(stn01_day,aes(x=avg2,y=Q_m3s_mean,color=avg5)) + geom_point()+ 
  scale_x_continuous(transform = "log") +
  scale_y_continuous(transform = "log")

ggplot(stn01_day,aes(x=avg3,y=Q_m3s_mean)) + geom_point()
ggplot(stn01_day,aes(x=avg5,y=Q_m3s_mean,color=avg2)) + geom_point() #+ 
#  scale_x_continuous(transform = "log") +
#  scale_y_continuous(transform = "log")

ggplot(stn01_day,aes(x=Date,y=precipt_mm,color=avg2)) + geom_point() 
ggplot(stn01_day%>%filter(avg5>0),aes(x=Date,y=avg3,color=avg5)) + geom_point() 
ggplot(stn01_day%>%filter(avg3==0 & avg5>2),aes(x=Date,y=avg5,color=avg5)) + geom_point() 

mean(stn01_day$precipt_mm,na.rm = TRUE)
median(stn01_day$precipt_mm,na.rm = TRUE)

stn01_day_avg2_1.9 <- stn01_day%>%filter(avg2<1.9)

#do it by month year
stn04$month_year <- format(as.Date(stn04$Date), "%Y-%m")

stn01$month_year <- format(as.Date(stn01$Date), "%Y-%m")
stn01_monthyear <- stn01 %>% drop_na(Q_m3s)%>%
  group_by(month_year) %>% 
  summarise(WLTemp_c_mean = mean(WLTemp_c,na.rm = TRUE),
            Q_m3s_max =max(Q_m3s,na.rm = TRUE)
            )%>%
  drop_na(Q_m3s_max)

stn01_monthyear <- left_join(stn01_monthyear,stn01,by=c("Q_m3s_max"="Q_m3s","month_year"))
stn01_monthyear <- unique(stn01_monthyear)

##
storm1_peak = as.POSIXct("2019-07-14 14:30:00",tz="UTC")
storm1_begin = as.POSIXct("2019-07-14 00:45:00",tz="UTC")
storm1_end = as.POSIXct("2019-07-16 08:00:00",tz="UTC")

storm2_peak = as.POSIXct("2019-08-14 07:45:00",tz="UTC")
storm2_begin = as.POSIXct("2019-08-13 18:30:00",tz="UTC")
storm2_end = as.POSIXct("2019-08-15 05:15:00",tz="UTC")

storm3_peak = as.POSIXct("2019-09-20 20:45:00",tz="UTC")
storm3_begin = as.POSIXct("2019-09-20 14:30:00",tz="UTC")
storm3_end = as.POSIXct("2019-09-22 07:00:00",tz="UTC")

storm4_peak = as.POSIXct("2019-10-24 05:00:00",tz="UTC")
storm4_begin = as.POSIXct("2019-10-23 11:00:00",tz="UTC")
storm4_end = as.POSIXct("2019-10-29 00:30:00",tz="UTC")


storm5_peak = as.POSIXct("2021-06-21 04:45:00",tz="UTC")
storm5_begin = as.POSIXct("2021-06-19 00:45:00",tz="UTC")
storm5_end = as.POSIXct("2021-06-23 02:15:00",tz="UTC")

storm6_peak = as.POSIXct("2021-07-01 02:45:00",tz="UTC")
storm6_begin = as.POSIXct("2021-06-29 22:45:00",tz="UTC")
storm6_end = as.POSIXct("2021-07-03 22:30:00",tz="UTC")

storm7_peak = as.POSIXct("2021-08-02 15:30:00",tz="UTC")
storm7_begin = as.POSIXct("2021-08-02 23:00:00",tz="UTC")
storm7_end = as.POSIXct("2021-08-04 19:30:00",tz="UTC")

storm8_peak = as.POSIXct("2021-09-06 17:00:00",tz="UTC")
storm8_begin = as.POSIXct("2021-09-05 00:30:00",tz="UTC")
storm8_end = as.POSIXct("2021-09-07 08:30:00",tz="UTC")

storm9_peak = as.POSIXct("2021-10-21 19:30:00",tz="UTC")
storm9_begin = as.POSIXct("2021-10-20 07:30:00",tz="UTC")
storm9_end = as.POSIXct("2021-10-22 16:00:00",tz="UTC")

storm10_peak = as.POSIXct("2021-11-19 10:00:00",tz="UTC")
storm10_begin = as.POSIXct("2021-11-18 22:15:00",tz="UTC")
storm10_end = as.POSIXct("2021-11-20 06:00:00",tz="UTC")

storm11_peak = as.POSIXct("2021-12-18 15:15:00",tz="UTC")
storm11_begin = as.POSIXct("2021-12-16 09:30:00",tz="UTC")
storm11_end = as.POSIXct("2021-12-24 11:00:00",tz="UTC")

storm12_peak = as.POSIXct("2022-03-15 14:15:00",tz="UTC")
storm12_begin = as.POSIXct("2022-03-13 20:00:00",tz="UTC")
storm12_end = as.POSIXct("2022-03-16 20:15:00",tz="UTC")

storm13_peak = as.POSIXct("2022-04-21 01:00:00",tz="UTC")
storm13_begin = as.POSIXct("2022-04-18 20:00:00",tz="UTC")
storm13_end = as.POSIXct("2022-04-23 05:15:00",tz="UTC")

storm14_peak = as.POSIXct("2022-05-16 11:45:00",tz="UTC")
storm14_begin = as.POSIXct("2022-05-14 07:45:00",tz="UTC")
storm14_end = as.POSIXct("2022-05-22 14:00:00",tz="UTC")

storm15_peak = as.POSIXct("2022-06-25 12:00:00",tz="UTC")
storm15_begin = as.POSIXct("2022-06-22 08:15:00",tz="UTC")
storm15_end = as.POSIXct("2022-06-30 03:15:00",tz="UTC")

storm16_peak = as.POSIXct("2022-07-04 07:30:00",tz="UTC")
storm16_begin = as.POSIXct("2022-07-01 22:00:00",tz="UTC")
storm16_end = as.POSIXct("2022-07-08 21:00:00",tz="UTC")

storm17_peak = as.POSIXct("2022-08-10 12:15:00",tz="UTC")
storm17_begin = as.POSIXct("2022-08-09 06:00:00",tz="UTC")
storm17_end = as.POSIXct("2022-08-18 23:30:00",tz="UTC")

storm18_peak = as.POSIXct("2022-09-11 06:30:00",tz="UTC")
storm18_begin = as.POSIXct("2022-09-10 01:45:00",tz="UTC")
storm18_end = as.POSIXct("2022-09-16 07:15:00",tz="UTC")

storm19_peak = as.POSIXct("2022-10-19 12:15:00",tz="UTC")
storm19_begin = as.POSIXct("2022-10-18 21:45:00",tz="UTC")
storm19_end = as.POSIXct("2022-10-21 21:00:00",tz="UTC")

storm20_peak = as.POSIXct("2022-11-20 22:30:00",tz="UTC")
storm20_begin = as.POSIXct("2022-11-20 12:45:00",tz="UTC")
storm20_end = as.POSIXct("2022-11-23 19:15:00",tz="UTC")

storm21_peak = as.POSIXct("2022-12-14 18:15:00",tz="UTC")
storm21_begin = as.POSIXct("2022-12-14 10:00:00",tz="UTC")
storm21_end = as.POSIXct("2022-12-21 10:30:00",tz="UTC")

storm22_peak = as.POSIXct("2023-01-24 16:00:00",tz="UTC")
storm22_begin = as.POSIXct("2023-01-23 22:15:00",tz="UTC")
storm22_end = as.POSIXct("2023-01-28 00:30:00",tz="UTC")

storm23_peak = as.POSIXct("2023-02-21 06:00:00",tz="UTC")
storm23_begin = as.POSIXct("2023-02-20 15:30:00",tz="UTC")
storm23_end = as.POSIXct("2023-02-26 16:30:00",tz="UTC")

storm24_peak = as.POSIXct("2023-03-04 15:15:00",tz="UTC")
storm24_begin = as.POSIXct("2023-03-03 10:30:00",tz="UTC")
storm24_end = as.POSIXct("2023-03-07 15:45:00",tz="UTC")


stn01_storm1 <- stn01%>%filter(DateTime>storm1_begin&DateTime<storm1_end)
stn01_storm2 <- stn01%>%filter(DateTime>storm2_begin&DateTime<storm2_end)
stn01_storm3 <- stn01%>%filter(DateTime>storm3_begin&DateTime<storm3_end)
stn01_storm4 <- stn01%>%filter(DateTime>storm4_begin&DateTime<storm4_end)
stn01_storm5 <- stn01%>%filter(DateTime>storm5_begin&DateTime<storm5_end)
stn01_storm6 <- stn01%>%filter(DateTime>storm6_begin&DateTime<storm6_end)
stn01_storm7 <- stn01%>%filter(DateTime>storm7_begin&DateTime<storm7_end)
stn01_storm8 <- stn01%>%filter(DateTime>storm8_begin&DateTime<storm8_end)
stn01_storm9 <- stn01%>%filter(DateTime>storm9_begin&DateTime<storm9_end)
stn01_storm10 <- stn01%>%filter(DateTime>storm10_begin&DateTime<storm10_end)
stn01_storm11 <- stn01%>%filter(DateTime>storm11_begin&DateTime<storm11_end)
stn01_storm12 <- stn01%>%filter(DateTime>storm12_begin&DateTime<storm12_end)
stn01_storm13 <- stn01%>%filter(DateTime>storm13_begin&DateTime<storm13_end)
stn01_storm14 <- stn01%>%filter(DateTime>storm14_begin&DateTime<storm14_end)
stn01_storm15 <- stn01%>%filter(DateTime>storm15_begin&DateTime<storm15_end)
stn01_storm16 <- stn01%>%filter(DateTime>storm16_begin&DateTime<storm16_end)
stn01_storm17 <- stn01%>%filter(DateTime>storm17_begin&DateTime<storm17_end)
stn01_storm18 <- stn01%>%filter(DateTime>storm18_begin&DateTime<storm18_end)
stn01_storm19 <- stn01%>%filter(DateTime>storm19_begin&DateTime<storm19_end)
stn01_storm20 <- stn01%>%filter(DateTime>storm20_begin&DateTime<storm20_end)
stn01_storm21 <- stn01%>%filter(DateTime>storm21_begin&DateTime<storm21_end)
stn01_storm22 <- stn01%>%filter(DateTime>storm22_begin&DateTime<storm22_end)
stn01_storm23 <- stn01%>%filter(DateTime>storm23_begin&DateTime<storm23_end)
stn01_storm24 <- stn01%>%filter(DateTime>storm24_begin&DateTime<storm24_end)


stn02_storm1 <- stn02%>%filter(DateTime>storm1_begin&DateTime<storm1_end)
stn02_storm2 <- stn02%>%filter(DateTime>storm2_begin&DateTime<storm2_end)
stn02_storm3 <- stn02%>%filter(DateTime>storm3_begin&DateTime<storm3_end)
stn02_storm4 <- stn02%>%filter(DateTime>storm4_begin&DateTime<storm4_end)
stn02_storm5 <- stn02%>%filter(DateTime>storm5_begin&DateTime<storm5_end)
stn02_storm6 <- stn02%>%filter(DateTime>storm6_begin&DateTime<storm6_end)
stn02_storm7 <- stn02%>%filter(DateTime>storm7_begin&DateTime<storm7_end)
stn02_storm8 <- stn02%>%filter(DateTime>storm8_begin&DateTime<storm8_end)
stn02_storm9 <- stn02%>%filter(DateTime>storm9_begin&DateTime<storm9_end)
stn02_storm10 <- stn02%>%filter(DateTime>storm10_begin&DateTime<storm10_end)
stn02_storm11 <- stn02%>%filter(DateTime>storm11_begin&DateTime<storm11_end)
stn02_storm12 <- stn02%>%filter(DateTime>storm12_begin&DateTime<storm12_end)
stn02_storm13 <- stn02%>%filter(DateTime>storm13_begin&DateTime<storm13_end)
stn02_storm14 <- stn02%>%filter(DateTime>storm14_begin&DateTime<storm14_end)
stn02_storm15 <- stn02%>%filter(DateTime>storm15_begin&DateTime<storm15_end)
stn02_storm16 <- stn02%>%filter(DateTime>storm16_begin&DateTime<storm16_end)
stn02_storm17 <- stn02%>%filter(DateTime>storm17_begin&DateTime<storm17_end)
stn02_storm18 <- stn02%>%filter(DateTime>storm18_begin&DateTime<storm18_end)
stn02_storm19 <- stn02%>%filter(DateTime>storm19_begin&DateTime<storm19_end)
stn02_storm20 <- stn02%>%filter(DateTime>storm20_begin&DateTime<storm20_end)
stn02_storm21 <- stn02%>%filter(DateTime>storm21_begin&DateTime<storm21_end)
stn02_storm22 <- stn02%>%filter(DateTime>storm22_begin&DateTime<storm22_end)
stn02_storm23 <- stn02%>%filter(DateTime>storm23_begin&DateTime<storm23_end)
stn02_storm24 <- stn02%>%filter(DateTime>storm24_begin&DateTime<storm24_end)


stn03_storm5 <- stn03%>%filter(DateTime>storm5_begin&DateTime<storm5_end)
stn03_storm6 <- stn03%>%filter(DateTime>storm6_begin&DateTime<storm6_end)
stn03_storm7 <- stn03%>%filter(DateTime>storm7_begin&DateTime<storm7_end)
stn03_storm8 <- stn03%>%filter(DateTime>storm8_begin&DateTime<storm8_end)
stn03_storm9 <- stn03%>%filter(DateTime>storm9_begin&DateTime<storm9_end)
stn03_storm10 <- stn03%>%filter(DateTime>storm10_begin&DateTime<storm10_end)
stn03_storm11 <- stn03%>%filter(DateTime>storm11_begin&DateTime<storm11_end)
stn03_storm12 <- stn03%>%filter(DateTime>storm12_begin&DateTime<storm12_end)
stn03_storm13 <- stn03%>%filter(DateTime>storm13_begin&DateTime<storm13_end)
stn03_storm14 <- stn03%>%filter(DateTime>storm14_begin&DateTime<storm14_end)
stn03_storm15 <- stn03%>%filter(DateTime>storm15_begin&DateTime<storm15_end)
stn03_storm16 <- stn03%>%filter(DateTime>storm16_begin&DateTime<storm16_end)
stn03_storm17 <- stn03%>%filter(DateTime>storm17_begin&DateTime<storm17_end)
stn03_storm18 <- stn03%>%filter(DateTime>storm18_begin&DateTime<storm18_end)
stn03_storm19 <- stn03%>%filter(DateTime>storm19_begin&DateTime<storm19_end)
stn03_storm20 <- stn03%>%filter(DateTime>storm20_begin&DateTime<storm20_end)
stn03_storm21 <- stn03%>%filter(DateTime>storm21_begin&DateTime<storm21_end)
stn03_storm22 <- stn03%>%filter(DateTime>storm22_begin&DateTime<storm22_end)
stn03_storm23 <- stn03%>%filter(DateTime>storm23_begin&DateTime<storm23_end)
stn03_storm24 <- stn03%>%filter(DateTime>storm24_begin&DateTime<storm24_end)

stn04_storm1 <- stn04%>%filter(DateTime>storm1_begin&DateTime<storm1_end)
stn04_storm2 <- stn04%>%filter(DateTime>storm2_begin&DateTime<storm2_end)
stn04_storm3 <- stn04%>%filter(DateTime>storm3_begin&DateTime<storm3_end)
stn04_storm4 <- stn04%>%filter(DateTime>storm4_begin&DateTime<storm4_end)
stn04_storm5 <- stn04%>%filter(DateTime>storm5_begin&DateTime<storm5_end)
stn04_storm6 <- stn04%>%filter(DateTime>storm6_begin&DateTime<storm6_end)
stn04_storm7 <- stn04%>%filter(DateTime>storm7_begin&DateTime<storm7_end)
stn04_storm8 <- stn04%>%filter(DateTime>storm8_begin&DateTime<storm8_end)
stn04_storm9 <- stn04%>%filter(DateTime>storm9_begin&DateTime<storm9_end)
stn04_storm10 <- stn04%>%filter(DateTime>storm10_begin&DateTime<storm10_end)
stn04_storm11 <- stn04%>%filter(DateTime>storm11_begin&DateTime<storm11_end)
stn04_storm12 <- stn04%>%filter(DateTime>storm12_begin&DateTime<storm12_end)
stn04_storm13 <- stn04%>%filter(DateTime>storm13_begin&DateTime<storm13_end)
stn04_storm14 <- stn04%>%filter(DateTime>storm14_begin&DateTime<storm14_end)
stn04_storm15 <- stn04%>%filter(DateTime>storm15_begin&DateTime<storm15_end)
stn04_storm16 <- stn04%>%filter(DateTime>storm16_begin&DateTime<storm16_end)
stn04_storm17 <- stn04%>%filter(DateTime>storm17_begin&DateTime<storm17_end)
stn04_storm18 <- stn04%>%filter(DateTime>storm18_begin&DateTime<storm18_end)
stn04_storm19 <- stn04%>%filter(DateTime>storm19_begin&DateTime<storm19_end)
stn04_storm20 <- stn04%>%filter(DateTime>storm20_begin&DateTime<storm20_end)
stn04_storm21 <- stn04%>%filter(DateTime>storm21_begin&DateTime<storm21_end)
stn04_storm22 <- stn04%>%filter(DateTime>storm22_begin&DateTime<storm22_end)
stn04_storm23 <- stn04%>%filter(DateTime>storm23_begin&DateTime<storm23_end)
stn04_storm24 <- stn04%>%filter(DateTime>storm24_begin&DateTime<storm24_end)



ggplot(stn01_storm1,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn01_storm1,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

#ggplot(stn01_storm2,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
#ggplot(stn01_storm3,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
#ggplot(stn01_storm4,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
#ggplot(stn01_storm5,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()

ggplot(stn01_storm6,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn01_storm6,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

ggplot(stn01_storm7,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn01_storm7,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

ggplot(stn01_storm8,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn01_storm8,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

ggplot(stn01_storm9,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn01_storm9,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

ggplot(stn01_storm10,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn01_storm10,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

ggplot(stn01_storm11,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn01_storm11,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

ggplot(stn01_storm12,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn01_storm12,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

ggplot(stn01_storm13,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn01_storm13,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

ggplot(stn01_storm14,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn01_storm14,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

#ggplot(stn01_storm15,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
#ggplot(stn01_storm16,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
#ggplot(stn01_storm17,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
#ggplot(stn01_storm18,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn01_storm19,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn01_storm19,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

ggplot(stn01_storm20,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn01_storm20,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

#ggplot(stn01_storm21,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
#ggplot(stn01_storm22,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
#ggplot(stn01_storm23,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
#ggplot(stn01_storm24,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()



ggplot(stn02_storm1,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn02_storm2,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn02_storm5,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn02_storm6,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn02_storm10,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn02_storm11,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn02_storm14,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()

ggplot(stn03_storm5,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn03_storm6,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn03_storm7,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn03_storm8,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn03_storm9,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn03_storm10,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn03_storm11,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn03_storm12,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn03_storm13,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn03_storm14,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()


ggplot(stn04_storm5,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn04_storm5,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

ggplot(stn04_storm6,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn04_storm6,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

ggplot(stn04_storm7,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn04_storm7,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

ggplot(stn04_storm8,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn04_storm8,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

ggplot(stn04_storm9,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn04_storm9,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

ggplot(stn04_storm10,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn04_storm10,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=DateTime)) + geom_point()

ggplot(stn04_storm11,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn04_storm12,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn04_storm13,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn04_storm14,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn04_storm15,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn04_storm20,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn04_storm21,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn04_storm22,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn04_storm23,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()
ggplot(stn04_storm24,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=DateTime)) + geom_point()

stn01_storm <- rbind(stn01_storm1,stn01_storm2,stn01_storm6,stn01_storm7,stn01_storm8,stn01_storm9,stn01_storm10,
                     stn01_storm11,stn01_storm12,stn01_storm13,stn01_storm14,stn01_storm19,stn01_storm20)
stn01_storm$month <- format(as.Date(stn01_storm$Date), "%m")

stn04_storm <- rbind(stn04_storm1,stn04_storm2,stn04_storm6,stn04_storm7,stn04_storm8,stn04_storm9,stn04_storm10,
                     stn04_storm11,stn04_storm12,stn04_storm13,stn04_storm14,stn04_storm19,stn04_storm20,
                     stn04_storm21,stn04_storm22,stn04_storm23,stn04_storm24)
stn04_storm$month <- format(as.Date(stn04_storm$Date), "%m")

stn02_storm <- rbind(stn02_storm1,stn02_storm2,stn02_storm5,stn02_storm6,stn02_storm10,
                     stn02_storm11,stn02_storm14)
stn02_storm$month <- format(as.Date(stn04_storm$Date), "%m")
stn02_storm$year_month <- format(as.Date(stn02_storm$DateTime), "%Y-%m")


stn03_storm <- rbind(stn03_storm5,stn03_storm6,stn03_storm7,stn03_storm8,stn03_storm9,stn03_storm10,
                     stn03_storm11,stn03_storm12,stn03_storm13,stn03_storm14)
stn03_storm$month <- format(as.Date(stn03_storm$Date), "%m")
stn03_storm$year_month <- format(as.Date(stn03_storm$DateTime), "%Y-%m")


ggplot(stn01_storm,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=month_year)) + geom_point()
ggplot(stn01_storm,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=month_year)) + geom_point()

ggplot(stn02_storm,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=year_month)) + geom_point()
ggplot(stn02_storm,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=year_month)) + geom_point()

ggplot(stn03_storm,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=year_month)) + geom_point()

ggplot(stn04_storm,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=month_year)) + geom_point()
ggplot(stn04_storm,aes(x=Q_m3s,y=CO2_ppm_adjusted*Q_m3s,color=month_year)) + geom_point()


ggplot(stn01_storm,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=month)) + geom_point() +
  scale_x_continuous(transform = "log") +scale_y_continuous(transform = "log")
ggplot(stn04_storm,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=month_year)) + geom_point() +
  scale_x_continuous(transform = "log") +scale_y_continuous(transform = "log")



######what about not storm?
summary(stn04$Q_m3s)
summary(stn01$Q_m3s)

stn01$month <- format(as.Date(stn01$Date), "%m")
stn04$month <- format(as.Date(stn04$Date), "%m")
stn04_belowmedian <- stn04%>%filter(Q_m3s<.0064) 
stn01_belowmedian <- stn04%>%filter(Q_m3s<.019) 


ggplot(stn01_belowmedian,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=month_year)) + geom_point()+
  scale_x_continuous(transform = "log") +scale_y_continuous(transform = "log")
ggplot(stn01_belowmedian,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=month)) + geom_point()+
  scale_x_continuous(transform = "log") +scale_y_continuous(transform = "log")

ggplot(stn04_belowmedian,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=month_year)) + geom_point() +
  scale_x_continuous(transform = "log") +scale_y_continuous(transform = "log")
ggplot(stn04_belowmedian,aes(x=Q_m3s,y=CO2_ppm_adjusted,color=month)) + geom_point() +
  scale_x_continuous(transform = "log") +scale_y_continuous(transform = "log")

all_stn$year_month <- format(as.Date(all_stn$DateTime), "%Y-%m")
all_stn$month <- format(as.Date(all_stn$DateTime), "%m")

ggplot(all_stn,aes(x=CO2_ppm_01,y=CO2_ppm_04,color=year_month)) + geom_point() +
  scale_x_continuous(transform = "log") +scale_y_continuous(transform = "log")


#all_stn_test$ <- all_stn%>%filter(CO2_ppm_04 < )

fig <- plot_ly(data = stn01, x = ~DateTime, y = ~CO2_ppm_adjusted)
fig <- plot_ly(data = stn01, x = ~DateTime, y = ~CO2_ppm_adjusted*Q_m3s)

fig <- plot_ly(data = stn04, x = ~DateTime, y = ~CO2_ppm_adjusted)
fig <- plot_ly(data = stn04, x = ~DateTime, y = ~CO2_ppm_adjusted*Q_m3s)

fig <- plot_ly(data = stn02, x = ~DateTime, y = ~CO2_ppm_adjusted)
fig <- plot_ly(data = stn02, x = ~DateTime, y = ~CO2_ppm_adjusted*Q_m3s)

fig <- plot_ly(data = stn02, x = ~DateTime, y = ~Q_m3s)
fig <- plot_ly(data = stn01, x = ~DateTime, y = ~Q_m3s)

fig <- plot_ly(stn04, x = ~DateTime)
fig <- fig %>% add_trace(y = ~CO2_ppm_adjusted, name = 'trace 0',mode = 'lines')
fig <- fig %>% add_trace(y = ~WL_m*10000, name = 'trace 1', mode = 'lines+markers')

all_stn <- all_stn[ order(all_stn$DateTime , decreasing = TRUE ),]

fig <- plot_ly(all_stn, x = ~DateTime)
fig <- fig %>% add_trace(y = ~CO2_ppm_02, name = 'trace 0',mode = 'lines')
fig <- fig %>% add_trace(y = ~CO2_ppm_03, name = 'trace 1', mode = 'lines+markers')

