#build dataframes
library(here)
library(dplyr)
library(streamMetabolizer)
library(unitted)
library(lubridate)


p4 <- plot_ly(M5025_SolarRed, x = ~DateTime, y = ~SolarRad_W.m2, type = 'scatter', mode = 'markers') 
ggplot(M5025_SolarRed,aes(x=DateTime,y=SolarRad_W.m2)) + geom_point()
#solar 
M5025_SolarRed <- read.csv(here::here("Weather_station/M5025_SolarRed.csv"))
M5025_SolarRed$DateTime <- as.POSIXct(M5025_SolarRed$fecha,format="%m/%d/%y %H:%M",tz="UTC") 

#station 1
WL_01 <- read.csv(here::here("data_cleaned/WL_01_cleaned.csv"))
WL_01$DateTime <- as.POSIXct(WL_01$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
WL_01 <- unique(WL_01)
DO_01 <- read.csv(here::here("data_cleaned/DO_01_cleaned.csv"))%>%select(!X)%>%drop_na(DateTime)
DO_01$DateTime <- as.POSIXct(DO_01$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
DO_01 <- unique(DO_01)

#rm(WL_01,DO_01)
#station 2
WL_02 <- read.csv(here::here("data_cleaned/WL_02_cleaned.csv"))
WL_02$DateTime <- as.POSIXct(WL_02$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
DO_02 <- read.csv(here::here("data_cleaned/DO_02_cleaned.csv"))
DO_02$DateTime <- as.POSIXct(DO_02$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

#rm(WL_02,DO_02)
#station 3
WL_03 <- read.csv(here::here("data_cleaned/WL_03_cleaned.csv"))
WL_03$DateTime <- as.POSIXct(WL_03$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
DO_03 <- read.csv(here::here("data_cleaned/DO_03_cleaned.csv"))
DO_03$DateTime <- as.POSIXct(DO_03$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

Stn03 <- full_join(WL_03,DO_03, by=c("DateTime","Station"))
rm(WL_03,DO_03)
#station 4 - duplicates in this one
WL_04 <- read.csv(here::here("data_cleaned/WL_04_cleaned.csv"))
WL_04$DateTime <- as.POSIXct(WL_04$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
WL_04 <- unique(WL_04)
DO_04 <- read.csv(here::here("data_cleaned/DO_04_cleaned.csv"))
DO_04$DateTime <- as.POSIXct(DO_04$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
DO_04 <- unique(DO_04)
DO_04 <- DO_04%>%drop_na(DateTime)

Stn04 <- full_join(WL_04,DO_04, by=c("DateTime","Station"))

#station 5
WL_05 <- read.csv(here::here("data_cleaned/WL_05_cleaned.csv"))
WL_05$DateTime <- as.POSIXct(WL_05$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
DO_05 <- read.csv(here::here("data_cleaned/DO_05_cleaned.csv"))
DO_05$DateTime <- as.POSIXct(DO_05$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

Stn05 <- full_join(WL_05,DO_05, by=c("DateTime","Station"))
rm(WL_05,DO_05)
#station 6
WL_06 <- read.csv(here::here("data_cleaned/WL_06_cleaned.csv"))
WL_06$DateTime <- as.POSIXct(WL_06$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
DO_06 <- read.csv(here::here("data_cleaned/DO_06_cleaned.csv"))
DO_06$DateTime <- as.POSIXct(DO_06$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

Stn06 <- full_join(WL_06,DO_06, by=c("DateTime","Station"))
rm(WL_06,DO_06)


#now edit the data frame and read out I guess

#solar.time | POSIXct,POSIXt|                 | required	
#DO.obs     |	numeric	      | mgO2 L^-1       | required	
#DO.sat     | numeric       | mgO2 L^-1       | required	
#depth      | numeric       | m               | required	
#temp.water | numeric       | degC            | required	
#light      | numeric       | umol m^-2 s^-1  | required	
#discharge  | numeric       | m^3 s^-1        | optional	

#check ?mm_data for more info

Stn01 <- full_join(WL_01,DO_01, by=c("DateTime","Station"))
Stn02 <- full_join(WL_02,DO_02, by=c("DateTime","Station"))

#calc DO sat
Stn01$DO_sat <- calc_DO_sat(temp=u(Stn01$WLTemp_c,"degC"), press=u(Stn01$AirPres_kpa*10,"mb"), sal=u(0,"PSU")) # units are checked
Stn02$DO_sat <- calc_DO_sat(temp=u(Stn02$WLTemp_c,"degC"), press=u(Stn02$AirPres_kpa*10,"mb"), sal=u(0,"PSU")) # units are checked

# now what about solar.time and light
# GMT-5
Stn01$DateTime_UTC <- Stn01$DateTime + hours(5)
Stn01$solar.time <- convert_UTC_to_solartime(Stn01$DateTime_UTC,-78.200147, time.type = c("apparent solar", "mean solar"))

Stn02$DateTime_UTC <- Stn02$DateTime + hours(5)
Stn02$solar.time <- convert_UTC_to_solartime(Stn02$DateTime_UTC,-78.200147, time.type = c("apparent solar", "mean solar"))

#i need to convert wl to depth, but for now wl will do I suppose

#and what about light?
#max light based on la virgin in W/m2
#convert to umol/m2/s: *4.57
#convert to PAR *.455

Stn01$light <- calc_light(Stn01$solar.time,0.327992,-78.200147,max.PAR = u(1400*4.57*0.455, "umol m^-2 s^-1"),
#  attach.units = deprecated()
)
Stn02$light <- calc_light(Stn02$solar.time,0.327992,-78.200147,max.PAR = u(1400*4.57*0.455, "umol m^-2 s^-1"),
                          #  attach.units = deprecated()
)

#depth
Stn01$Q_m3s_unitted <- unitted(Stn01$Q_m3s, units = "m^3 s^-1")
Stn01$depth <- calc_depth(Stn01$Q_m3s_unitted, c = u(0.409, "m"), f = u(0.294, ""))

Stn02$Q_m3s_unitted <- unitted(Stn02$Q_m3s, units = "m^3 s^-1")
Stn02$depth <- calc_depth(Stn02$Q_m3s_unitted, c = u(0.409, "m"), f = u(0.294, ""))


Stn01 <- Stn01%>%select(solar.time,DO_mgL,DO_sat,depth,WLTemp_c,light,Q_m3s_unitted)
colnames(Stn01) <- c("solar.time","DO.obs","DO.sat","depth","temp.water","light","discharge")
Stn02 <- Stn02%>%select(solar.time,DO_mgL,DO_sat,depth,WLTemp_c,light,Q_m3s)
colnames(Stn02) <- c("solar.time","DO.obs","DO.sat","depth","temp.water","light","discharge")

#Write out
write.csv(Stn01,here::here("metabolizer_dataframe/stn01_df_preliminary.csv"),row.names = FALSE)
write.csv(Stn02,here::here("metabolizer_dataframe/stn02_df_preliminary.csv"),row.names = FALSE)

