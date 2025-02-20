#build dataframes
library(here)
library(dplyr)
library(streamMetabolizer)
library(unitted)
library(lubridate)
library(MALDIquant)
library(zoo)

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
WL_03 <- read.csv(here::here("data_cleaned/WL_03_cleaned.csv"))
WL_03$DateTime <- as.POSIXct(WL_03$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
#DO
DO_03 <- read.csv(here::here("data_cleaned/DO_03_cleaned.csv"))
DO_03$DateTime <- as.POSIXct(DO_03$DateTime,format="%Y-%m-%d %H:%M",tz="UTC")
#rbind
Stn03 <- full_join(WL_03,DO_03, by=c("DateTime","Station"))

#the first DO measurement is 2021-06-10 15:00:00, so filter for after that
Stn03 <- Stn03%>%filter(DateTime > as.POSIXct("2021-06-12 12:00:00",tz="UTC"))
#the last DO measurement is 2023-04-01 11:30:00 
Stn03 <- Stn03%>%filter(DateTime < as.POSIXct("2022-10-12 11:30:00
",tz="UTC"))

#calc DO sat using stream metabolizer funtion
Stn03$DO_sat <- calc_DO_sat(temp=u(Stn03$WLTemp_c,"degC"), press=u(Stn03$AirPres_kpa*10,"mb"), sal=u(0,"PSU")) # units are checked

# convert to solar.time
# locat time is GMT-5
Stn03$DateTime_UTC <- Stn03$DateTime + hours(5)
Stn03$solar.time <- convert_UTC_to_solartime(Stn03$DateTime_UTC,-78.200147, time.type = c("apparent solar", "mean solar"))

#max light based on la virgin in W/m2
#convert to umol/m2/s: *4.57
#convert to PAR *.455
Stn03$light <- calc_light(Stn03$solar.time,0.327992,-78.200147,max.PAR = u(1400*4.57*0.455, "umol m^-2 s^-1"),
                          #  attach.units = deprecated()
)

#depth  
#There are a few ways to do this. 
#There is a stream metabolizer function that calculated depth from discharge based on an equation in Raymod yyyy
#I also want to use stage data 

#I developed my own inputs for calc_depth based on measurments in the stream

Stn03$Q_m3s_unitted <- unitted(Stn03$Q_m3s, units = "m^3 s^-1")
#this is the  units using estimations from US streams (we used this for July 10 dataframe)
#Stn02$depth <- calc_depth(Stn02$Q_m3s_unitted, c = u(0.409, "m"), f = u(0.294, ""))
#this is the coefficients developed using our site specific measurments:
Stn03$depth <- calc_depth(Stn03$Q_m3s_unitted, c = u(0.6383832, "m"), f = u(0.3108922, ""))


#change names
Stn03 <- Stn03%>%select(solar.time,DO_mgL,DO_sat,depth,WLTemp_c,light,Q_m3s)
colnames(Stn03) <- c("solar.time","DO.obs","DO.sat","depth","temp.water","light","discharge")

#now clean for metabolizer. 
#it needs to have even time intervals, so use maite's code to fix that plz

##Iclude rows when missing
Stn03 <- Stn03%>%drop_na(solar.time)
first<-min(Stn03$solar.time)
last<-max(Stn03$solar.time)
solar.timeREF<-seq(first, last, by="15 min")

Stn03$ref<-match.closest(as.numeric(Stn03$solar.time), as.numeric(solar.timeREF), tolerance = 100, nomatch = NA)
df<-data.frame(ref2=seq(1:length(solar.timeREF)), solar.timeREF=solar.timeREF)

Dat2<-Stn03[match(df$ref2, Stn03$ref),]
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
#write.csv(Dat2,here::here("metabolizer_dataframe/stn03_df_feb11_2.csv"),row.names = FALSE)

