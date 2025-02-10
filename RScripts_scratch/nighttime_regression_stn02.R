#nighttime regression 

#this script uses the nighttime_reg_multiple function.
#first read in function 


#### OBTAINING K WITH NIGHT TIME REGRESSIONS ####
### IT NEEDS A FILE WITH A  TIME, TEMPERATURE, DISSOLVED OXYGEN, and DISSOLVED OXYGEN AT 100% sat
### IMPORTANT, WITH THE HEADINGS "solar.time", "temp.water", "DO.obs" , "DO.sat".
### It performs several regressions for each day, moving the window of data for the regression, and saving the best regression
### You need to add several parameters: 
###     site = the file with the data
###     reg_start = the time to start the regressions (as  HH:MM:SS; '18:00:00')
###     reg_size = thelength of the period for the regressions (in hours)
###     num_reg = the number of regressions you want to do
###     timesteps = the timesteps of your data, in minutes
### It should be run as: data.out <- night_fun(site=data, reg_start= '17:00:00', reg_size=2, num_reg = 5, timesteps=10)
### This example would do the night time regression of the periods: 17-19;18-20;19-21;20-22;21-23 and save the best one
### The output consists in a dataframe with the day, slope,  slope.se, intercept, r2, and k600 as (d-1)


#use this reference to calc %saturation
#https://www.waterontheweb.org/under/waterquality/oxygen.html

library(here)
library(readr)
library(ggplot2)
library(plotly)
library(streamMetabolizer)
library(tidyverse)
library(scales)
library(ggpubr)
library(zoo)

baro <- read.csv(here::here("data_cleaned/Baro_cleaned.csv"))
#convert local time to solar time
baro$local.time <- as.POSIXct(baro$DateTime,format="%Y-%m-%d %H:%M", tz='America/Guayaquil')
baro$solar.time <- calc_solar_time(baro$local.time, longitude=-78.202283)
baro$DateTime <- NULL

#read in lux
stn02_lux_df <- read.csv(here::here("data_cleaned/LUX_abovewater_02_cleaned.csv"))
stn02_lux_df$local.time <- as.POSIXct(stn02_lux_df$DateTime,format="%Y-%m-%d %H:%M", tz='America/Guayaquil')
stn02_lux_df$solar.time <- calc_solar_time(stn02_lux_df$local.time, longitude=-78.202283)
stn02_lux_df$DateTime <- NULL

#read in discharge
stn02_Q_df <- read.csv(here::here("data_cleaned/WL_02_cleaned.csv"))%>%select(DateTime,Q_m3s)
stn02_Q_df$local.time <- as.POSIXct(stn02_Q_df$DateTime,format="%Y-%m-%d %H:%M", tz='America/Guayaquil')
stn02_Q_df$solar.time <- calc_solar_time(stn02_Q_df$local.time, longitude=-78.202283)
stn02_Q_df$DateTime <- NULL

stn02_O2_df <- read.csv("data_cleaned/DO_02_cleaned.csv")
stn02_O2_df$local.time <- as.POSIXct(stn02_O2_df$DateTime,format="%Y-%m-%d %H:%M", tz='America/Guayaquil')
stn02_O2_df$solar.time <- calc_solar_time(stn02_O2_df$local.time, longitude=-78.202283)
stn02_O2_df$DateTime <- NULL

stn02_df <- left_join(stn02_O2_df,baro,by=c("local.time","solar.time"))
stn02_df <- left_join(stn02_df,stn02_lux_df,by=c("local.time","solar.time","Station"))
stn02_df <- left_join(stn02_df,stn02_Q_df,by=c("local.time","solar.time"))

#calculate % saturation column

#cp = equilibrium oxygen concntration at nonstandard pressure, mg/L
#Cp = Ceq_standardP * P_atm[(1-P_wv/P_atm)(1-omega*P_atm)/(1-P_wv)/(1-omega)]

#Ceq_standardP = equilibrium o2 concntrationat standar pressure of 1 atm, mg/L
#Ceq_standardP = exp[7.7117 - 1.31403 • ln(t + 45.93)]
#unit: mg/L

stn02_df$Ceq_standardP <- exp(7.7117 - 1.31403 * log(stn02_df$DOTemp_c + 45.93))

#P_atm = nonstandard pressure
#to convert kpa to atm, divide by 101.3
#unit: atm
stn02_df$P_atm <- stn02_df$AirPres_kpa/101.3

#convert celcius to Kelvin
stn02_df$DOTemp_K <- stn02_df$DOTemp_c + 273.15 

#P_wv = partial pressure of water vapor
# In P_wv = 11.8571 - (3840.70/T)-(216961/T^2)
#units: atm
stn02_df$P_wv <- exp(11.8571 - (3840.70/stn02_df$DOTemp_K)-(216961/stn02_df$DOTemp_K^2))

#omega = 0.000975-(1.426*10^-5*t) + (6.436*10^-8*t^2)
stn02_df$omega = 0.000975-(1.426*10^-5*stn02_df$DOTemp_c) + (6.436*10^-8*stn02_df$DOTemp_c^2)

#now calculate C_p
stn02_df$C_p <- stn02_df$Ceq_standardP*stn02_df$P_atm*(
  (1-stn02_df$P_wv/stn02_df$P_atm)*(1-stn02_df$omega*stn02_df$P_atm)/
    (1- stn02_df$P_wv)/(1- stn02_df$omega ))

#now % saturation
stn02_df$percent_sat <- (100* stn02_df$DO_mgL)/stn02_df$C_p


#Select columns and rename:
#"solar.time", "temp.water", "DO.obs" , "DO.sat"
#is it actually local time that I need?
#and add light
stn02_df_subset <- stn02_df %>%select(solar.time,local.time,DOTemp_c,DO_mgL,C_p,Lux,Q_m3s)%>%rename(
  temp.water=DOTemp_c,DO.obs=DO_mgL,DO.sat=C_p,light=Lux)

#set additional parameters:
#stn04_df_subset <- stn04_df_subset%>%
#  filter(solar.time>"2021-06-11 00:00:00"&solar.time<"2021-08-11 00:00:00")%>%
#  drop_na(light)
stn02_df_subset <- stn02_df_subset%>%
 # drop_na(light)%>%
  drop_na(DO.sat)#%>%filter(DO.obs > 0)

#site = the file with the data
#reg_start = the time to start the regressions (as  HH:MM:SS; '18:00:00')
#reg_size = thelength of the period for the regressions (in hours)
#num_reg = the number of regressions you want to do
#timesteps = the timesteps of your data, in minutes

data.out <- night_fun_noplots(site=stn01_df_subset, reg_start= '14:00:00', reg_size=2, num_reg = 5, timesteps=15)

#plot
ggplot(stn04_O2_df,aes(x=DateTime,y=percent_sat)) + geom_point()



###


site=stn02_df_subset
reg_start= '17:00:00'
reg_size=4
num_reg = 3
timesteps=15

#night_fun_noplots <- function(site, reg_start, reg_size, num_reg , timesteps){
  
  #first step is to make a new file with the daily mean values. This used only to be able to select each day, and take the temperature for the k600 calcs
  daily = site %>% group_by(Daily = format(local.time, "%Y-%m-%d")) %>%
    summarise_all(funs(mean=mean(., na.rm=T)))
  
  daily <- daily%>%filter(Daily<"2019-10-08" | Daily>"2019-10-24")%>%
    filter(Daily<"2019-11-01" | Daily>"2019-11-01")%>%
    filter(Daily!="2019-11-05")%>%filter(Daily!="2019-11-06")%>%
    filter(Daily<"2019-11-08"|Daily>"2019-12-22")%>%
    filter(Daily<"2019-12-31"|Daily>"2021-07-12")%>%
    filter(Daily!="2021-07-27")%>%
    filter(Daily!="2021-10-24")%>%
    filter(Daily<"2021-11-03"|Daily>"2022-03-17")%>%
    filter(Daily!="2022-06-22")%>%
    filter(Daily!="2022-06-30")%>%
    filter(Daily!="2022-10-12")%>%
    filter(Daily!="2022-12-01")%>%
    filter(Daily!="2022-12-08")
  

  
  
  

  
  #initializing variables
  daily$slope <- NA
  daily$r2    <- NA
  daily$intercept <- NA
  daily$slope.se <- NA
  daily$k600 <- NA
  daily$Q_m3s <- NA

  
  
  #the main loop to go through each day/night
  for(i in 2:(length(daily$Daily)-1)){
    
    print(paste("day", i))
    #extract the day and the day after to isolate the night period
    day1 <- as.Date(daily$local.time_mean[i])
    day2 <- as.Date(daily$local.time_mean[i+1])
    hour1 <- '11:00:00'
    
    #and make a POSIX.ct object to be able to subset the file later
    Date1 <- as.POSIXct( paste(day1 , hour1))
    Date2 <- as.POSIXct( paste(day2 , hour1))
    
    test <- site %>%  select(local.time,DO.sat,DO.obs)%>%
      filter(local.time> Date1 & local.time < Date2) %>%      #select the two days to do the NTR
      mutate(oxyf1 = rollmean(x = DO.obs, 7, align = "right", fill = NA) ) %>%      #Smooth O2 data  
      mutate(deltaO2= (oxyf1-lag(oxyf1))/timesteps*1440,     #calculate dC/dt and convert the values to  mgO2 L-1 day-1. 
             O2def= DO.sat - DO.obs) %>%           #calculate the oxygen deficit in mgO2/L
      na.omit() #and remove the NAs, because after smoothing the head and tail have a couple NAs
    
    
    #a sequence to do several regressions. It can be changed, now it makes 6 regressions
    twindow <- seq(from=1, to=num_reg*3600, by=3600 )
    
    #new data frame to store the "varying regressions" for each day
    varyreg <- data.frame(slope=double(length(twindow)),intercept=double(length(twindow)),slope.se=double(length(twindow)), 
                          r2=double(length(twindow)), reg_start= rep(Date1, times=length(twindow)))
    
    #second loop to do the linear regressions multiple times
    for( j in 1:length(twindow)){
      
      
      #first it defines the period to make the regression, using the twindow sequence to move the window
      reg_per1 <- as.POSIXct(as.POSIXct(paste(day1 , reg_start))+twindow[j])
      reg_per2 <-  reg_per1 + reg_size*3600
      
      #for each window, subset the data to make the regression
      nightr_d <- subset(test, local.time > reg_per1 & local.time < reg_per2)
      
      
      #make a linear regression and store all variables of interest
      fit1 <- lm( nightr_d$deltaO2~nightr_d$O2def)
      varyreg$slope[j] <- summary(fit1)$coefficients[2, 1]
      varyreg$r2[j]    <- summary(fit1)$r.squared
      varyreg$slope.se[j] <- summary(fit1)$coefficients[2, 2]
      varyreg$intercept[j] <-  summary(fit1)$coefficients[1, 1]
      varyreg$reg_start[j]    <- as.POSIXct(reg_per1)
    }
    
    #now we select only the values that have a positive slope
    varyreg2 <- subset(varyreg, slope>=0)
    
    #in case of no values that are positive, in order to not get any error, I just use the full dataset, but that day will be useless
    if(nrow(varyreg2)==0){ varyreg2 =varyreg }
    
    #find which of the regressions was the best
    bestie <- which.max(varyreg2$r2)
    
    #and save the relevant variables
    daily$slope[i] <- varyreg2$slope[bestie]
    daily$intercept[i] <- varyreg2$intercept[bestie]
    daily$slope.se[i] <- varyreg2$slope.se[bestie]
    daily$r2[i]    <- varyreg2$r2[bestie]
    
    #finally convert the slope to k600 with the temperature correction #from raymond et al., 2012
    daily$k600[i]<- ((600/(1800.6-(120.1*daily$temp.water_mean[i])+(3.7818*daily$temp.water_mean[i]^2)-(0.047608*daily$temp.water_mean[i]^3)))^-0.5)*varyreg2$slope[bestie]
    
    #also I want a summary of Q
    test2 <- site %>%  select(local.time,Q_m3s)%>%
      filter(local.time> Date1 & local.time < Date2)
    daily$Q_m3s[i]    <- mean(test2$Q_m3s,na.rm=TRUE)
  }

        ##### THIS IS A BONUS, TO MAKE A COMPOSITE PLOT OF [O2], dO2, LIGHT AND THE REGRESSION ####
    ####It is disconnected by default, if you want to use it make sure you have light data also
    
    #recover the best regression and make it again
    reg_per1 <- varyreg2$reg_start[bestie] #as.POSIXct(as.POSIXct(paste(day1 , reg_start))+twindow[bestie])
    reg_per2 <-  reg_per1 + reg_size*3600
    nightr_d <- subset(test, local.time> reg_per1 & local.time < reg_per2)
    
    #make a plot of the regression
    night_regfit<- 
      ggplot(data= nightr_d, aes(x=O2def, y=deltaO2))+
      geom_point()+
      geom_smooth(method=lm)+
      stat_cor(label.y.npc = 0.9) +
      stat_regline_equation( label.y.npc = 1)
    labs(x= expression(O[2]~deficit~(mg~O[2]~L^-1) ),y=expression(Delta~O[2]~(mg~O[2]~L^-1~d^-1)))+
      theme(axis.text = element_text(size=14), axis.title = element_text(size=14))
    
    # a plot for the O2 deficit
    o2def <-ggplot(data=test)+
      geom_line(aes(x=as.POSIXct(local.time), y=DO.sat-DO.obs), size=2, color="blue3")+
      scale_x_datetime(date_breaks = "4 hour", labels=date_format("%H"))+
      geom_vline(xintercept = as.numeric(reg_per1), linetype=4)+
      geom_vline(xintercept = as.numeric(reg_per2), linetype=4)+
      labs(x="hour" ,y=expression(O[2]~deficit~(mg~O[2]~L^-1) ))+
      theme(axis.text = element_text(size=12), axis.title = element_text(size=12))
    
    #and the delta O2
    o2_delta <- ggplot(data=test)+
      geom_line(aes(x=as.POSIXct(local.time), y=deltaO2), size=2, color="darkred")+
      scale_x_datetime(date_breaks = "4 hour", labels=date_format("%H"))+
      geom_vline(xintercept = as.numeric(reg_per1), linetype=4)+
      geom_vline(xintercept = as.numeric(reg_per2), linetype=4)+
      labs(x="hour" ,y=expression(Delta~O[2]~(mg~O[2]~L^-1~d^-1)))+
      theme(axis.text = element_text(size=12), axis.title = element_text(size=12)) 
    
    #the light
    lightp <- ggplot(data=test)+
      geom_line(aes(x=as.POSIXct(local.time), y=light), size=2, color="goldenrod2")+
      scale_x_datetime(date_breaks = "4 hour", labels=date_format("%H"))+
      labs(x="hour" ,y="light")+
      theme(axis.text = element_text(size=12), axis.title = element_text(size=12)) 
    
    #make a pannel with them
    pan1<-  ggarrange(o2def, o2_delta, lightp, night_regfit , ncol = 2, nrow = 2)
    
    name <- deparse(substitute(site))
    
    #and export it with the name of the site and the date as title
    pan1 %>%
      ggexport(filename = paste(name,'_', day1,'.png', sep=""),res = 170, width = 1000, height = 1000)
    ####
    
 # }
  
  #and return the daily file
  daily
  #}




plot(r2~k600,data=daily)
  
ggplot(daily,aes(x=r2,y=slope)) + geom_point()

ggplot(daily %>%filter(r2>.6)%>%filter(slope>0)
       ,aes(x=Q_m3s,y=slope)) + geom_point()

ggplot(daily %>%
         filter(r2>.6)%>%
         filter(local.time_mean>"2021-07-01"&local.time_mean<"2021-08-01")%>%
         filter(slope>0)
       ,aes(x=Q_m3s*1000,y=k600)) + geom_point()

ggplot(daily %>%
         filter(r2>.7)%>%filter(slope>0)%>%
         filter(local.time_mean>"2019-07-18")%>%
         filter(local.time_mean<"2019-09-14" | local.time_mean > "2021-07-01")%>%
         filter(local.time_mean< "2021-09-18" | local.time_mean >"2022-03-03")
       ,aes(x=Q_m3s,y=k600,color=local.time_mean)) + geom_point()

ggplot(daily %>%
         filter(r2>.6)%>%filter(slope>0)%>%
         filter(local.time_mean>"2019-06-18" & local.time_mean <"2019-08-31")
       ,aes(x=Q_m3s,y=k600,color=local.time_mean)) + geom_point()
ggplot(daily %>%
         filter(r2>.6)%>%filter(slope>0)%>%
         filter(local.time_mean>"2021-06-10" & local.time_mean <"2021-08-31")
       ,aes(x=Q_m3s,y=k600,color=local.time_mean)) + geom_point()

ggplot(daily %>%
         filter(r2>.6)%>%filter(slope>0)%>%
         filter(local.time_mean>"2022-06-10" & local.time_mean <"2022-08-31")
       ,aes(x=Q_m3s,y=k600,color=local.time_mean)) + geom_point()
