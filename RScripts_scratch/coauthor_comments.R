

#Ricardo wanted more details about discharge and seasonality
#notes on high v low flow

#October to February (low)
allstation_data_3 <- read.csv(here::here("Whitmore_etal_Biogeochemistry/allstation_data_3.csv"))
allstation_data_3$DateTime <- as.POSIXct(allstation_data_3$DateTime,format="%Y-%m-%d %H:%M:%S")
allstation_data_3$Day <- as.Date(allstation_data_3$DateTime)
allstation_data_3_summ <- allstation_data_3%>%group_by(month_number,Day)%>%
  summarise(mean_daily_Q_Ls = mean(Q_m3s_02*1000),
            sd_daily_Q_Ls = sd(Q_m3s_02*1000),
            median_daily_Q_Ls = median(Q_m3s_02*1000))

WL_05 <- read.csv(here::here("data_cleaned/WL_05_cleaned.csv"))
WL_05$DateTime <- as.POSIXct(WL_05$DateTime,format="%Y-%m-%d %H:%M:%S")
WL_05$Day <- as.Date(WL_05$DateTime)

WL_05$month_number <- sub('.*?[-_]', '', WL_05$DateTime)
WL_05$month_number <-  gsub("-.+", "", WL_05$month_number)
WL_05$month_number <- as.numeric(WL_05$month_number)
WL_05_summ <- WL_05%>%group_by(month_number,Day)%>%
  summarise(mean_daily_Q_Ls = mean(Q_m3s*1000),
            sd_daily_Q_Ls = sd(Q_m3s*1000),
            median_daily_Q_Ls = median(Q_m3s*1000))

low_flow_df <- allstation_data_3_summ%>%filter(month_number<=2|month_number>=10)
median(low_flow_df$mean_daily_Q_Ls,na.rm = TRUE)
mean(low_flow_df$mean_daily_Q_Ls,na.rm = TRUE)
sd(low_flow_df$mean_daily_Q_Ls,na.rm = TRUE)

low_flow_05_df <- WL_05_summ%>%filter(month_number<=2|month_number>=10)
median(low_flow_05_df$mean_daily_Q_Ls,na.rm = TRUE)
mean(low_flow_05_df$mean_daily_Q_Ls,na.rm = TRUE)
sd(low_flow_05_df$mean_daily_Q_Ls,na.rm = TRUE)

#June to August (high)
high_flow_df <- allstation_data_3_summ%>%filter(month_number>2&month_number<10)
median(high_flow_df$mean_daily_Q_Ls,na.rm = TRUE)
mean(high_flow_df$mean_daily_Q_Ls,na.rm = TRUE)
sd(high_flow_df$mean_daily_Q_Ls,na.rm = TRUE)

high_flow_05_df <- WL_05_summ%>%filter(month_number>2&month_number<10)
median(high_flow_05_df$mean_daily_Q_Ls,na.rm = TRUE)
mean(high_flow_05_df$mean_daily_Q_Ls,na.rm = TRUE)
sd(high_flow_05_df$mean_daily_Q_Ls,na.rm = TRUE)

#average air temp
allstation_data_3$Day <- as.Date(allstation_data_3$DateTime)
airtemp_sum <- allstation_data_3 %>%group_by(Day)%>%
  summarise(AirTemp_c=mean(AirTemp_c))

#precipitation
precip_df <- read.csv(here::here("Weather_station/M5025_Precipitation.csv"))
precip_df$DateTime <- as.POSIXct(precip_df$fecha,format="%m/%d/%y %H:%M",tz='UTC')
precip_df$Day <- as.Date(precip_df$DateTime)

precip_accu_df <- precip_df%>%group_by(Day)%>%
  summarise(day_accu_mm = sum(precipt_mm))
precip_accu_df$month_number <- sub('.*?[-_]', '', precip_accu_df$Day)
precip_accu_df$month_number <-  gsub("-.+", "", precip_accu_df$month_number)
precip_accu_df$month_number <- as.numeric(precip_accu_df$month_number)

mean(precip_accu_df$day_accu_mm,na.rm = TRUE)
sd(precip_accu_df$day_accu_mm,na.rm = TRUE)

low_precip_df <- precip_accu_df%>%filter(month_number<=2|month_number>=10)
high_precip_df <- precip_accu_df%>%filter(month_number>2&month_number<10)
mean(low_precip_df$day_accu_mm,na.rm = TRUE)
sd(low_precip_df$day_accu_mm,na.rm = TRUE)
median(low_precip_df$day_accu_mm,na.rm = TRUE)

mean(high_precip_df$day_accu_mm,na.rm = TRUE)
sd(high_precip_df$day_accu_mm,na.rm = TRUE)
median(high_precip_df$day_accu_mm,na.rm = TRUE)

###what is 18,000 ppm in umol/L at 8.1 Celsius?

pCO2_ppm = 18000
AirPres_kpa = 62.4
WaterTemp_c = 8.1

#calc pCO2
kH_STP_mol.l.atm = .035*1/0.986923
D_K = 2400 
T_STP_K = 298.15

#air
air_pressure_atm = AirPres_kpa / 101.3
water_pressure_atm = AirPres_kpa / 101.3 + 0.000967841

#stn01
##CO2 in uatm
pCO2_w_atm = pCO2_ppm /10^6* water_pressure_atm
#henry's constant adjust for temp
KH_mol.l.atm = kH_STP_mol.l.atm * exp(D_K*(1/(WaterTemp_c +273.15) - 1/T_STP_K))
##CO2 umol/L
CO2_mol.L = KH_mol.l.atm*pCO2_w_atm
