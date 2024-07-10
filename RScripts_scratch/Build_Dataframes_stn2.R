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
WL_02 <- read.csv(here::here("data_cleaned/WL_02_cleaned.csv"))
WL_02$DateTime <- as.POSIXct(WL_02$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

#DO
DO_02 <- read.csv(here::here("data_cleaned/DO_02_cleaned.csv"))
DO_02$DateTime <- as.POSIXct(DO_02$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

#rbind
Stn02 <- full_join(WL_02,DO_02, by=c("DateTime","Station"))
#the first DO measurment is 2019-07-18 14:00:00, so filter for after that
Stn02 <- Stn02%>%filter(DateTime > as.POSIXct("2019-07-18 14:00:00",tz="UTC"))

#calc DO sat using stream metabolizer funtion
Stn02$DO_sat <- calc_DO_sat(temp=u(Stn02$WLTemp_c,"degC"), press=u(Stn02$AirPres_kpa*10,"mb"), sal=u(0,"PSU")) # units are checked

# convert to solar.time
# locat time is GMT-5
Stn02$DateTime_UTC <- Stn02$DateTime + hours(5)
Stn02$solar.time <- convert_UTC_to_solartime(Stn02$DateTime_UTC,-78.200147, time.type = c("apparent solar", "mean solar"))

#max light based on la virgin in W/m2
#convert to umol/m2/s: *4.57
#convert to PAR *.455
Stn02$light <- calc_light(Stn02$solar.time,0.327992,-78.200147,max.PAR = u(1400*4.57*0.455, "umol m^-2 s^-1"),
                          #  attach.units = deprecated()
)

#depth  
#There are a few ways to do this. 
#There is a stream metabolizer function that calculated depth from discharge based on an equation in Raymod yyyy
#I also want to use stage data 

#for now let's stick to the calc_depth

Stn02$Q_m3s_unitted <- unitted(Stn02$Q_m3s, units = "m^3 s^-1")
Stn02$depth <- calc_depth(Stn02$Q_m3s_unitted, c = u(0.409, "m"), f = u(0.294, ""))







#change names
Stn02 <- Stn02%>%select(solar.time,DO_mgL,DO_sat,depth,WLTemp_c,light,Q_m3s)
colnames(Stn02) <- c("solar.time","DO.obs","DO.sat","depth","temp.water","light","discharge")

#now clean for metabolizer. 
#it needs to have even time intervals, so use maite's code to fix that plz

##Iclude rows when missing
Stn02 <- Stn02%>%drop_na(solar.time)
first<-min(Stn02$solar.time)
last<-max(Stn02$solar.time)
solar.timeREF<-seq(first, last, by="15 min")

Stn02$ref<-match.closest(as.numeric(Stn02$solar.time), as.numeric(solar.timeREF), tolerance = 100, nomatch = NA)
df<-data.frame(ref2=seq(1:length(solar.timeREF)), solar.timeREF=solar.timeREF)

Dat2<-Stn02[match(df$ref2, Stn02$ref),]
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
#library(janitor)
duplicated(dat_check$solar.time)

for(i in 1:nrow(dat_check)) {
  dat_check$time_2[i] <- dat_check$time_1[i+1]
  # do stuff with row
}
#calc diff
dat_check$difftime <- difftime(dat_check$time_2,dat_check$time_1,units="mins")



#Write out
write.csv(Dat2,here::here("metabolizer_dataframe/stn02_df_july10.csv"),row.names = FALSE)

