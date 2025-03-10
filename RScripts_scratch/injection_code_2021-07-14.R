# clean up injection data from 2021

#So If I a am remebering correctly, I took the below waterfall co2 sensor out 
#and during the injection I put the sensor continually further upstream

inj_45m <- read.csv(here::here("Inj_2021July14/Injection_CO2_45m_cambell_2021-07-14.csv"),skip=3)
colnames(inj_45m) <- c("DateTime","pCO2_45m","remove")
inj_45m$DateTime <- as.POSIXct(inj_45m$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")
inj_45m <- inj_45m%>%select(DateTime,pCO2_45m)

inj_80m <- read.csv(here::here("Inj_2021July14/Injection_CO2_80m_2021-07-14.csv"),skip=6)%>%rename(pCO2_80m = Voltage..ppm.)
inj_80m$DateTime <- as.POSIXct(paste(inj_80m$Date,inj_80m$Time,sep=" "),format="%m/%d/%Y %I:%M:%S %p",tz="UTC")
inj_80m <- inj_80m%>%select(DateTime,pCO2_80m)

inj_120m <- read.csv(here::here("Inj_2021July14/Injection_CO2_120m_2021-07-14.csv"),skip=6)%>%rename(pCO2_120m = Voltage..ppm.)
inj_120m$DateTime <- as.POSIXct(paste(inj_120m$Date,inj_120m$Time,sep=" "),format="%m/%d/%Y %I:%M:%S %p",tz="UTC")
inj_120m <- inj_120m%>%select(DateTime,pCO2_120m)

WL_df <- read.csv(here::here("data_cleaned/WL_02_cleaned.csv"))
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")

inj_df <- full_join(inj_80m,inj_120m,by="DateTime")
inj_df <- left_join(inj_df,WL_df,by="DateTime")

library(zoo)

inj_df <- inj_df%>%filter(DateTime >= as.POSIXct("2021-07-14 09:45:00",tz="UTC")&
                            DateTime <= as.POSIXct("2021-07-14 16:00:00",tz="UTC" ))
inj_df$AirPres_kpa <- na.approx(inj_df$AirPres_kpa)
inj_df$WLTemp_c <- na.approx(inj_df$WLTemp_c)

#convert but I don't know if they are new or old so that's bad, I'm guesing both new?

# Now correct the adjusted ppm 
#inj_df$pCO2_80m_adj <- (inj_df$pCO2_80m  )* (1 + (1013 - inj_df$AirPres_kpa* 10) * 0.0015)
#this one:
inj_df$pCO2_120m_adj <- (inj_df$pCO2_120m  )* (1 + (1013 - inj_df$AirPres_kpa* 10) * 0.0015)

#
#this one: 178
inj_df$pCO2_80m_adj <- inj_df$pCO2_80m * (1 + (1013 - inj_df$AirPres_kpa*10) * 0.0015) * (1 - (25 - inj_df$WLTemp_c) * 0.003)
#inj_df$pCO2_120m_adj <- inj_df$pCO2_120m * (1 + (1013 - inj_df$AirPres_kpa*10) * 0.0015) * (1 - (25 - inj_df$WLTemp_c) * 0.003)

##check data
p <- plot_ly(inj_df,
             x = ~DateTime, y = ~pCO2_120m_adj, type = 'scatter', mode = 'markers') 

p2 <- add_trace(p, x = inj_df$DateTime, y = inj_df$pCO2_80m_adj, 
                type = "scatter")

p2
#write.csv(inj_df,here::here("MergedFiles/Inj_df_2019July14.csv"))


ggplot(inj_df#%>%filter(DateTime <= as.POSIXct("2021-07-14 12:00:00",tz="UTC"))
       %>%filter(DateTime <= as.POSIXct("2021-07-14 14:00:00",tz="UTC"))
       ) +
  geom_point(aes(x=DateTime,y=pCO2_120m_adj),color="blue") +
  geom_point(aes(x=DateTime,y=pCO2_80m_adj),color="green")
