#build dataframes
library(here)
library(dplyr)
library(streamMetabolizer)
library(unitted)
library(lubridate)
library(MALDIquant)
library(zoo)
library(tidyr)
library(ggplot2)

#below is what is required for the stream metabolizer.

#solar.time | POSIXct,POSIXt|                 | required	
#DO.obs     |	numeric	      | mgO2 L^-1       | required	
#DO.sat     | numeric       | mgO2 L^-1       | required	
#depth      | numeric       | m               | required	
#temp.water | numeric       | degC            | required	
#light      | numeric       | umol m^-2 s^-1  | required	
#discharge  | numeric       | m^3 s^-1        | optional	

#Discharge is NOT needed if you do not pool k600. Discharge IS needed if you want to do partially pooled data
#you can use Q to calculate depth (though that is based on an equations developed for US streams, NOT paramo streams)
#you can also do depth calculated based on the stage data collected by amy and kayla

#so let's do both and run both, right? That's a good idea right?

#Read in cleaned data for station 2

#water level
WL_01 <- read.csv(here::here("data_cleaned/WL_01_cleaned.csv"))
WL_01$DateTime <- as.POSIXct(WL_01$DateTime,format="%Y-%m-%d %H:%M",tz="UTC")
#DO
DO_01 <- read.csv(here::here("data_cleaned/DO_01_cleaned.csv"))
DO_01$DateTime <- as.POSIXct(DO_01$DateTime,format="%Y-%m-%d %H:%M",tz="UTC")
#rbind
Stn01 <- full_join(WL_01,DO_01, by=c("DateTime","Station"))

#the first DO measurement is 2019-10-1 0:00, so filter for after that
Stn01 <- Stn01%>%filter(DateTime > as.POSIXct("2019-07-12 17:00:00",tz="UTC"))
#the last DO measurement is 2022-05-31 00:00:00
Stn01 <- Stn01%>%filter(DateTime < as.POSIXct("2022-05-31 00:00:00",tz="UTC"))

#calc DO sat using stream metabolizer funtion
Stn01$DO_sat <- calc_DO_sat(temp=u(Stn01$WLTemp_c,"degC"), press=u(Stn01$AirPres_kpa*10,"mb"), sal=u(0,"PSU")) # units are checked

# convert to solar.time
# locat time is GMT-5
Stn01$DateTime_UTC <- Stn01$DateTime + hours(5)
Stn01$solar.time <- convert_UTC_to_solartime(Stn01$DateTime_UTC,-78.200147, time.type = c("apparent solar", "mean solar"))

#max light based on la virgin in W/m2
#convert to umol/m2/s: *4.57
#convert to PAR *.455
Stn01$light <- calc_light(Stn01$solar.time,0.327992,-78.200147,max.PAR = u(1400*4.57*0.455, "umol m^-2 s^-1"),
                          #  attach.units = deprecated()
)

#depth  
#There are a few ways to do this. 
#There is a stream metabolizer function that calculated depth from discharge based on an equation in Raymod yyyy
#I also want to use stage data 

#I developed my own inputs for calc_depth based on measurments in the stream

Stn01$Q_m3s_unitted <- unitted(Stn01$Q_m3s, units = "m^3 s^-1")
#this is the  units using estimations from US streams (we used this for July 10 dataframe)
#Stn02$depth <- calc_depth(Stn02$Q_m3s_unitted, c = u(0.409, "m"), f = u(0.294, ""))
#this is the coefficients developed using our site specific measurments:
Stn01$depth <- calc_depth(Stn01$Q_m3s_unitted, c = u(0.6383832, "m"), f = u(0.3108922, ""))


#change names
Stn01 <- Stn01%>%select(solar.time,DO_mgL,DO_sat,depth,WLTemp_c,light,Q_m3s)
colnames(Stn01) <- c("solar.time","DO.obs","DO.sat","depth","temp.water","light","discharge")

#now clean for metabolizer. 
#it needs to have even time intervals, so use maite's code to fix that plz

##Iclude rows when missing
Stn01 <- Stn01%>%drop_na(solar.time)
first<-min(Stn01$solar.time)
last<-max(Stn01$solar.time)
solar.timeREF<-seq(first, last, by="15 min")

Stn01$ref<-match.closest(as.numeric(Stn01$solar.time), as.numeric(solar.timeREF), tolerance = 100, nomatch = NA)
df<-data.frame(ref2=seq(1:length(solar.timeREF)), solar.timeREF=solar.timeREF)

Dat2<-Stn01[match(df$ref2, Stn01$ref),]
Dat2$solar.time<-solar.timeREF

Dat2$ref <- NULL

############### now plot it, babe
ggplot(Dat2,aes(x=solar.time,y=DO.obs)) + geom_point()
ggplot(Dat2,aes(x=solar.time,y=as.numeric(depth))) + geom_point()
ggplot(Dat2,aes(x=solar.time,y=as.numeric(discharge))) + geom_point()

##################3

#now fill in NAs
Dat2$DO.obs<-na.spline(Dat2$DO.obs, maxgap = 3, na.rm = FALSE)
Dat2$DO.sat<-na.spline(as.numeric(Dat2$DO.sat), maxgap = 3, na.rm = FALSE)
Dat2$temp.water<-na.spline(Dat2$temp.water, maxgap = 4, na.rm = FALSE)
Dat2$depth<-na.spline(as.numeric(Dat2$depth), maxgap = 144, na.rm = FALSE)
Dat2$light<-na.spline(Dat2$light, maxgap = 3, na.rm = FALSE)
Dat2$discharge<-na.spline(Dat2$discharge, maxgap = 144, na.rm = FALSE)

#check for dubplicates etc

dat_check <- as.data.frame(Dat2$solar.time)
colnames(dat_check) <- c("time_1")
dat_check$time_2 <- dat_check$time_1
library(janitor)
duplicated(dat_check$solar.time)

for(i in 1:nrow(dat_check)) {
  dat_check$time_2[i] <- dat_check$time_1[i+1]
  # do stuff with row
}
#calc diff - make sure they are all 15min
dat_check$difftime <- difftime(dat_check$time_2,dat_check$time_1,units="mins")



#Write out
#write.csv(Dat2,here::here("metabolizer_dataframe/stn01_df_feb12.csv"),row.names = FALSE)

