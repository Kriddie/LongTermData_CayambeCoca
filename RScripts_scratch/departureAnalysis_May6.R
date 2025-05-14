#departure metrics

departure_metrics <- read.csv(here::here("Departure_files/departure_metrics_may6.csv"))
departure_metrics$date <- as.Date(departure_metrics$date)
#read in other explanitory variabiles
stn01_LUX <- read.csv(here::here("data_cleaned/LUX_abovewater_01_cleaned.csv"))
stn01_LUX$DateTime <- as.POSIXct(stn01_LUX$DateTime,format="%m/%d/%Y %H:%M",tz='UTC')
stn01_LUX$date <- as.Date(stn01_LUX$DateTime)
stn01_LUX_summary <- stn01_LUX%>%group_by(date)%>%
  summarise(LUX_dayave=mean(LUX))

# la virgin
weather_station <- read.csv(here::here("Weather_station/M5025_SolarRed.csv"))
weather_station$date <- gsub('([0-9]+) .*', '\\1', weather_station$fecha)
weather_station$date <- as.Date(weather_station$date,format="%m/%d/%y")
weather_station_summary <- weather_station%>%group_by(date)%>%
  summarise(SolarRad_W.m2_dayave=mean(SolarRad_W.m2))

#metabolism

df_stn1_metab <- read.csv(here::here("StreamMetabolizer/Predictions_bestones/station1_calclight_march9.csv"))%>%drop_na(GPP_mean)%>%filter(GPP_Rhat < 1.05)%>%filter(ER_Rhat < 1.05)%>%filter(GPP_Rhat < 1.05)%>%filter(GPP_75pct >= 0)
df_stn1_metab$GPP_mean <- ifelse(df_stn1_metab$GPP_mean < 0,0,df_stn1_metab$GPP_mean )
df_stn1_metab$date <- as.Date(df_stn1_metab$date)


#join
departure_metrics <- left_join(departure_metrics,stn01_LUX_summary,by="date")
departure_metrics <- left_join(departure_metrics,weather_station_summary,by="date")
departure_metrics <- left_join(departure_metrics,df_stn1_metab%>%select(date,GPP_mean,ER_mean),by="date")

#check rising limbs
WL_02 <- read.csv(here::here("data_cleaned/WL_02_cleaned.csv"))
WL_02$DateTime_1 <- as.POSIXct(WL_02$DateTime,format="%Y-%m-%d %H:%M",tz="UTC")
WL_02$DateTime <- ifelse(is.na(WL_02$DateTime_1)==TRUE,paste(WL_02$DateTime,"00:00:00",sep=" "),WL_02$DateTime )
WL_02$DateTime <- as.POSIXct(WL_02$DateTime,format="%Y-%m-%d %H:%M",tz="UTC")
WL_02$DateTime_1 <- NULL

#rising limb days (days that are entirely on rising limbs)
#2021-06-19, 2021-06-20, 2021-06-30, 2021-07-18, 2022-06-23,2022-07-03

check_plot <- plot_ly(WL_02, x = ~DateTime, y = ~Q_m3s, type = 'scatter', mode = 'lines') 

##plot all of it
ggplot(departure_metrics,aes(x=meanCO2dep,y=meanO2dep)) + geom_point()

########Best ones IMO#######

#offset v DO_ave is strong. Q is not related if we remove high flows
p1 <- ggplot(departure_metrics #%>%filter(Q_m3s_ave<0.02)
             ,aes(x=log(Q_m3s_ave),y=offset)) + geom_point()
p2 <- ggplot(departure_metrics #%>%filter(Q_m3s_ave<0.02)
             ,aes(x=DO_mgL_ave,y=offset,color=log(Q_m3s_ave*1000))) + geom_point()

p3.1 <- ggplot(departure_metrics # %>%filter(Q_m3s_ave<0.02)
             ,aes(x=Q_m3s_ave,y=DO_mgL_ave)) + geom_point()
p3.2 <- ggplot(departure_metrics # %>%filter(Q_m3s_ave<0.02)
             ,aes(x=Q_mgL_delta,y=DO_mgL_ave)) + geom_point()

#strong relationship btw stretch and GPP, even better when high delta Q removed 
#stretch and ER not related
p5.1 <- ggplot(departure_metrics
             , aes(x=GPP_mean,y=stretch,color=log(Q_mgL_delta))) + geom_point()
p5.2 <- ggplot(departure_metrics%>%filter(Q_mgL_delta<.02)
               , aes(x=GPP_mean,y=stretch,color=DO_mgL_ave)) + geom_point()
p5.3 <- ggplot(departure_metrics%>%filter(Q_mgL_delta<.008)
             , aes(x=GPP_mean,y=stretch,color=log(Q_mgL_delta))) + geom_point()


#test slope
p7.1 <- ggplot(departure_metrics #%>%filter(date>as.Date("2021-01-01")&date<as.Date("2021-12-01"))
               ,aes(x=minO2dep,y=slope,color=date)) + geom_point() #+ ylim(c(0,5))
p7.2 <- ggplot(departure_metrics%>%filter(date<as.Date("2021-01-01"))
             ,aes(x=minO2dep,y=slope,color=DO_mgL_ave)) + geom_point()# + ylim(c(0,5))
p7.3 <- ggplot(departure_metrics%>%filter(date>as.Date("2021-12-01"))
               ,aes(x=minO2dep,y=slope,color=DO_mgL_ave)) + geom_point() #+ ylim(c(0,5))


########END Best ones IMO#######
#don't bother with this
p6 <- ggplot(departure_metrics %>%filter(DO_mgL_ave>3) #%>%filter(Q_mgL_delta<.008)
             ,aes(x=ER_mean,y=minO2dep)) + geom_point()

#test plots
ptest <- ggplot(departure_metrics ,aes(x=LUX_dayave,y=EQ)) + geom_point() +
  scale_x_continuous(transform="log") + scale_y_continuous(transform="log")

ptest <- ggplot(departure_metrics,aes(x=LUX_dayave,y=stretch)) + geom_point() +
  scale_x_continuous(transform="log") + scale_y_continuous(transform="log")
ptest <- ggplot(departure_metrics,aes(x=SolarRad_W.m2_dayave,y=stretch)) + geom_point()

ptest <- ggplot(departure_metrics,aes(x=date,y=DO_mgL_ave)) + geom_point()
ptest <- ggplot(departure_metrics ,aes(x=DO_mgL_ave,y=width)) + geom_point()

#DOC photodegredation changes the slope so I could look at the relationship between LUX and slope (or slope during high LUX)
#look at slope during high DO and slope during low DO.(hypoxia vs no hypoxia): a
#during low light 
#light vs DO hysteresis. might tell you something about photodegredation.
      #width or hyst look at temp 

#mass balance? Nah.


#change over time
ggplot(departure_metrics ,aes(x=date,y=stretch)) + geom_point()
ggplot(departure_metrics%>%filter(date>"2021-01-01") ,aes(x=date,y=SolarRad_W.m2_dayave)) + geom_line()
ggplot(departure_metrics%>%filter(date>"2021-01-01") ,aes(x=date,y=LUX_dayave)) + geom_point()



p1 <- plot_ly(departure_metrics, x = ~date, y = ~DO_mgL_ave, type = 'scatter', mode = 'lines') 
