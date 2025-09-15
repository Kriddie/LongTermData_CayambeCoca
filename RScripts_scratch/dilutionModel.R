
#dilution model
library(dplyr)
library(zoo)


#A simple dilution model: 
#used to calculate the change in dissolved CO2 concentration between each time step attributable solely to an increase/decrease in water volume. 
#The increase/decrease in water volume between each time step was calculated from the change in discharge; 
#the dilution related change in CO2 concentration could then be calculated by adjusting the volume fraction CO2 concentration, 
#assuming no change in the volume of CO2. 
#The observed minus the dilution-modeled value is referred to hereafter as “excess CO2”; positive values indicate an addition of CO2 to the stream, 
#negative values indicate an overall loss from the stream. 
#Excess CO2 is therefore defined as the change in CO2 concentration not explained by dilution.



WL_02 <- read.csv(here::here("data_cleaned/WL_02_cleaned.csv"))
WL_02$DateTime_1 <- as.POSIXct(WL_02$DateTime,format="%Y-%m-%d %H:%M",tz="UTC")
WL_02$DateTime <- ifelse(is.na(WL_02$DateTime_1)==TRUE,paste(WL_02$DateTime,"00:00:00",sep=" "),WL_02$DateTime )
WL_02$DateTime <- as.POSIXct(WL_02$DateTime,format="%Y-%m-%d %H:%M",tz="UTC")
WL_02$DateTime_1 <- NULL
WL_02$Q_Ls <- WL_02$Q_m3s*1000
#CO2

CO2_df <- read.csv(here::here("data_cleaned/CO2_01_max10000_cleaned.csv"))

CO2_df$DateTime_1 <- as.POSIXct(CO2_df$DateTime,format="%Y-%m-%d %H:%M",tz="UTC")
CO2_df$DateTime <- ifelse(is.na(CO2_df$DateTime_1)==TRUE,paste(CO2_df$DateTime,"00:00:00",sep=" "),CO2_df$DateTime )
CO2_df$DateTime <- as.POSIXct(CO2_df$DateTime,format="%Y-%m-%d %H:%M",tz="UTC")
CO2_df$DateTime_1 <- NULL

CO2_df <- unique(CO2_df)
CO2_df <- CO2_df%>%drop_na(CO2_ppm_adjusted)


#convert to umol per l
kH_STP_mol.l.atm = .035*1/0.986923
D_K = 2400 
T_STP_K = 298.15
CO2_df$pCO2_air_ppm <- 418.53 # 2022 average manoa
CO2_df$air_pressure_atm <- CO2_df$Total_hPa / 1013.25 - 0.000967841
CO2_df$pCO2_air_atm <-  CO2_df$pCO2_air_ppm / 10^6  * CO2_df$air_pressure_atm
CO2_df$pCO2_w_atm <- CO2_df$CO2_ppm_adjusted / 10^6 
#henry's constant adjust for temp
CO2_df$KH_mol.l.atm <- kH_STP_mol.l.atm * exp(D_K*(1/(CO2_df$WLTemp_01_c+273.15) - 1/T_STP_K))
CO2_df$KH_mol.m3.atm <- CO2_df$KH_mol.l.atm * 1000
CO2_df$CO2_sat_mol.L <- CO2_df$KH_mol.l.atm*CO2_df$pCO2_air_atm
CO2_df$CO2_sat_umol.L <- CO2_df$CO2_sat_mol.L*10^6
CO2_df$CO2_mol.L <- CO2_df$KH_mol.l.atm*CO2_df$pCO2_w_atm
CO2_df$CO2_umol.L <- CO2_df$CO2_mol.L*10^6

CO2_df <- CO2_df%>%select(DateTime,CO2_ppm_adjusted,pCO2_air_ppm,air_pressure_atm,CO2_umol.L,CO2_sat_umol.L)
#bind
Stn01 <- full_join(WL_02,CO2_df, by=c("DateTime"))
Stn01 <- Stn01%>%drop_na(Q_m3s)%>%drop_na(CO2_ppm_adjusted)

#data frame evey 15 min
library(lubridate)
df_15min <- as.data.frame(seq(ymd_hm('2019-07-12 12:30'),ymd_hm('2022-10-12 14:45'), by = '15 mins'))
df_15min <- df_15min%>%rename(DateTime=`seq(ymd_hm("2019-07-12 12:30"), ymd_hm("2022-10-12 14:45"), by = "15 mins")`)
df_15min$DateTime <- as.POSIXct(df_15min$DateTime,format="%Y-%m-%d %H:%M:%S",tz='UTC')

Stn01 <- left_join(df_15min,Stn01, by='DateTime')
#calculate 3-day rolling average
Stn01 <- Stn01 %>%
  mutate(Q_Ls_avg3 = rollmean(Q_Ls, k=3, fill=NA, align='right'))%>%
  mutate(CO2_umol.L_avg3 = rollmean(CO2_umol.L, k=3, fill=NA, align='right'))

Stn01 <-  Stn01 %>%
  mutate(Q_Ls_minus15min = lag(Q_Ls))%>%
  mutate(CO2_umol.L_minus15min = lag(CO2_umol.L)) %>%
  mutate(Q_Ls_ave3_minus15min = lag(Q_Ls_avg3))%>%
  mutate(CO2_umol.L_ave3_minus15min = lag(CO2_umol.L_avg3))
Stn01 <- Stn01[order(Stn01$DateTime),]
#Stn01$Q_Ls_delta <- Stn01$Q_Ls_minus15min - Stn01$Q_Ls

# C₁*V₁ = C₂*V₂
#C₂ = (C₁*V₁)/V₂
#expacted
Stn01$CO2_umol.L_expected <- Stn01$CO2_umol.L_minus15min*Stn01$Q_Ls_minus15min/Stn01$Q_Ls
Stn01$CO2_umol.L_excess <- Stn01$CO2_umol.L -  Stn01$CO2_umol.L_expected

Stn01$CO2_umol.L_expected_ave3 <- Stn01$CO2_umol.L_ave3_minus15min*Stn01$Q_Ls_ave3_minus15min/Stn01$Q_Ls
Stn01$CO2_umol.L_excess_ave3 <- Stn01$CO2_umol.L -  Stn01$CO2_umol.L_expected_ave3

#percent access
Stn01$CO2_percentExcess <- Stn01$CO2_umol.L_excess/Stn01$CO2_umol.L
Stn01$CO2_percentExcess_ave3 <- Stn01$CO2_umol.L_excess_ave3/Stn01$CO2_umol.L

all_stn <- Stn01
#now extract storms
storm1_peak = as.POSIXct("2019-07-14 14:30:00",tz="UTC")
storm1_begin = as.POSIXct("2019-07-14 00:45:00",tz="UTC")
storm1_end = as.POSIXct("2019-07-15 00:00:00",tz="UTC")

#storm1.1_peak = as.POSIXct("2019-07-15 02:30:00",tz="UTC")
storm1.1_begin = as.POSIXct("2019-07-15 00:45:00",tz="UTC")
storm1.1_end = as.POSIXct("2019-07-16 08:00:00",tz="UTC")

storm2_peak = as.POSIXct("2019-08-14 07:45:00",tz="UTC")
storm2_begin = as.POSIXct("2019-08-13 18:30:00",tz="UTC")
storm2_end = as.POSIXct("2019-08-15 05:15:00",tz="UTC")

storm2.1_peak = as.POSIXct("2019-08-04 18:30:00",tz="UTC")
storm2.1_begin = as.POSIXct("2019-08-01 13:45:00",tz="UTC")
storm2.1_end = as.POSIXct("2019-08-08 06:15:00",tz="UTC")

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
storm7_begin = as.POSIXct("2021-08-01 23:00:00",tz="UTC")
storm7_end = as.POSIXct("2021-08-04 12:00:00",tz="UTC")

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


allstn_storm1 <- all_stn%>%filter(DateTime>storm1_begin-12*60*60&DateTime<storm1_end)
allstn_storm1.1 <- all_stn%>%filter(DateTime>storm1.1_begin-12*60*60&DateTime<storm1.1_end)
allstn_storm2 <- all_stn%>%filter(DateTime>storm2_begin-12*60*60&DateTime<storm2_end)
allstn_storm2.1 <- all_stn%>%filter(DateTime>storm2.1_begin-12*60*60&DateTime<storm2.1_end)
allstn_storm3 <- all_stn%>%filter(DateTime>storm3_begin-12*60*60&DateTime<storm3_end)
allstn_storm4 <- all_stn%>%filter(DateTime>storm4_begin-12*60*60&DateTime<storm4_end)
allstn_storm5 <- all_stn%>%filter(DateTime>storm5_begin-12*60*60&DateTime<storm5_end)
allstn_storm6 <- all_stn%>%filter(DateTime>storm6_begin-12*60*60&DateTime<storm6_end)
allstn_storm7 <- all_stn%>%filter(DateTime>storm7_begin-12*60*60&DateTime<storm7_end)
allstn_storm8 <- all_stn%>%filter(DateTime>storm8_begin-12*60*60&DateTime<storm8_end)
allstn_storm9 <- all_stn%>%filter(DateTime>storm9_begin-12*60*60&DateTime<storm9_end)
allstn_storm10 <- all_stn%>%filter(DateTime>storm10_begin-12*60*60&DateTime<storm10_end)
allstn_storm11 <- all_stn%>%filter(DateTime>storm11_begin-12*60*60&DateTime<storm11_end)
allstn_storm12 <- all_stn%>%filter(DateTime>storm12_begin-12*60*60&DateTime<storm12_end)
allstn_storm13 <- all_stn%>%filter(DateTime>storm13_begin-12*60*60&DateTime<storm13_end)
allstn_storm14 <- all_stn%>%filter(DateTime>storm14_begin-12*60*60&DateTime<storm14_end)
allstn_storm15 <- all_stn%>%filter(DateTime>storm15_begin-12*60*60&DateTime<storm15_end)
allstn_storm16 <- all_stn%>%filter(DateTime>storm16_begin-12*60*60&DateTime<storm16_end)
allstn_storm17 <- all_stn%>%filter(DateTime>storm17_begin-12*60*60&DateTime<storm17_end)
allstn_storm18 <- all_stn%>%filter(DateTime>storm18_begin-12*60*60&DateTime<storm18_end)

allstn_storm1$storm_name <- "storm1"
allstn_storm1.1$storm_name <- "storm1.1"
allstn_storm2$storm_name <- "storm2"
allstn_storm2.1$storm_name <- "storm2.1"
allstn_storm3$storm_name <- "storm3"
allstn_storm4$storm_name <- "storm4"
allstn_storm5$storm_name <- "storm5"
allstn_storm6$storm_name <- "storm6"
allstn_storm7$storm_name <- "storm7"
allstn_storm8$storm_name <- "storm8"
allstn_storm9$storm_name <- "storm9"
allstn_storm10$storm_name <- "storm10"
allstn_storm11$storm_name <- "storm11"
allstn_storm12$storm_name <- "storm12"
allstn_storm13$storm_name <- "storm13"
allstn_storm14$storm_name <- "storm14"
allstn_storm15$storm_name <- "storm15"
allstn_storm16$storm_name <- "storm16"
allstn_storm17$storm_name <- "storm17"
allstn_storm18$storm_name <- "storm18"

#now calc time elapsed since storm start
allstn_storm1$time_elapsed <- allstn_storm1$DateTime - storm1_begin
allstn_storm1.1$time_elapsed <- allstn_storm1.1$DateTime - storm1.1_begin
allstn_storm2$time_elapsed <- allstn_storm2$DateTime - storm2_begin
allstn_storm2.1$time_elapsed <- allstn_storm2.1$DateTime - storm2.1_begin
allstn_storm3$time_elapsed <- allstn_storm3$DateTime - storm3_begin
allstn_storm4$time_elapsed <- allstn_storm4$DateTime - storm4_begin
allstn_storm5$time_elapsed <- allstn_storm5$DateTime - storm5_begin
allstn_storm6$time_elapsed <- allstn_storm6$DateTime - storm6_begin
allstn_storm7$time_elapsed <- allstn_storm7$DateTime - storm7_begin
allstn_storm8$time_elapsed <- allstn_storm8$DateTime - storm8_begin
allstn_storm9$time_elapsed <- allstn_storm9$DateTime - storm9_begin
allstn_storm10$time_elapsed <- allstn_storm10$DateTime - storm10_begin
allstn_storm11$time_elapsed <- allstn_storm11$DateTime - storm11_begin
allstn_storm12$time_elapsed <- allstn_storm12$DateTime - storm12_begin
allstn_storm13$time_elapsed <- allstn_storm13$DateTime - storm13_begin
allstn_storm14$time_elapsed <- allstn_storm14$DateTime - storm14_begin
allstn_storm15$time_elapsed <- allstn_storm15$DateTime - storm15_begin
allstn_storm16$time_elapsed <- allstn_storm16$DateTime - storm16_begin
allstn_storm17$time_elapsed <- allstn_storm17$DateTime - storm17_begin
allstn_storm18$time_elapsed <- allstn_storm18$DateTime - storm18_begin


allstn_storm <- rbind(allstn_storm1,allstn_storm1.1,allstn_storm2,allstn_storm2.1,allstn_storm3,allstn_storm4,allstn_storm5,allstn_storm6,allstn_storm7,allstn_storm8,allstn_storm9,allstn_storm10,allstn_storm11,allstn_storm12,allstn_storm13,allstn_storm14,allstn_storm15,allstn_storm16,allstn_storm17,allstn_storm18)
allstn_storm$month <- format(as.Date(allstn_storm$Date), "%m")
allstn_storm$year_month <- format(as.Date(allstn_storm$DateTime), "%Y-%m")

allstn_storm_summary <- allstn_storm%>%drop_na(Q_Ls)%>%drop_na(CO2_umol.L_excess)%>%group_by(storm_name)%>%
  summarise(Q_Ls=max(Q_Ls,na.rm = TRUE),
            CO2_umol.L_excess=max(CO2_umol.L_excess,na.rm = TRUE))

allstn_storm_summary2 <- left_join(allstn_storm_summary,
                                   allstn_storm%>%select(storm_name,DateTime,Q_Ls)%>%rename(Qpeak_DateTime=DateTime),
                                   by=c("storm_name","Q_Ls"))

allstn_storm_summary2 <- left_join(allstn_storm_summary2,
                                   allstn_storm%>%select(storm_name,DateTime,CO2_umol.L_excess)%>%rename(CO2excess_DateTime=DateTime),
                                   by=c("storm_name","CO2_umol.L_excess"))

allstn_storm_summary2$Time_diff <- allstn_storm_summary2$Qpeak_DateTime-allstn_storm_summary2$CO2excess_DateTime

all_stn <-  all_stn%>%
  mutate(CO2_umol.L_excess_ave7 = rollmean(CO2_umol.L_excess, k=7, fill=NA, align='right'))


####### excess CO2

#no storms 3,4, 5, 12
ggplot(all_stn %>%filter(DateTime>storm1_begin-12*60*60&
                           DateTime<storm1_end)) +
  geom_point(aes(x=DateTime,y=CO2_umol.L_excess),color="grey50") +
  geom_point(aes(x=DateTime,y=Q_Ls),color="blue")+
  geom_smooth(aes(x=DateTime,y=CO2_umol.L_excess*5),color="grey10",method = lm, formula = y ~ splines::bs(x, 10), se = FALSE) 

ggplot(all_stn %>%filter(DateTime>storm1.1_begin-12*60*60&
                           DateTime<storm1.1_end)) +
  geom_point(aes(x=DateTime,y=CO2_umol.L_excess),color="grey50") +
  geom_point(aes(x=DateTime,y=Q_Ls),color="blue")+
  geom_smooth(aes(x=DateTime,y=CO2_umol.L_excess*5),color="grey10",method = lm, formula = y ~ splines::bs(x, 10), se = FALSE) 

ggplot(all_stn %>%filter(DateTime>storm8_begin-12*60*60&
                           DateTime<storm8_end)) +
  geom_point(aes(x=DateTime,y=CO2_umol.L_excess),color="grey50") +
  geom_point(aes(x=DateTime,y=Q_Ls),color="blue")+
  geom_smooth(aes(x=DateTime,y=CO2_umol.L_excess*5),color="grey10",method = lm, formula = y ~ splines::bs(x, 10), se = FALSE) 

ggplot(all_stn %>%filter(DateTime>storm8_begin-24*60*60&
                                DateTime<storm8_end)) +
  geom_point(aes(x=DateTime,y=CO2_umol.L_excess),color="grey50") +
  geom_point(aes(x=DateTime,y=Q_Ls),color="blue")+
  geom_smooth(aes(x=DateTime,y=CO2_umol.L_excess*5),color="grey10",method = lm, formula = y ~ splines::bs(x, 10), se = FALSE) 

ggplot(all_stn %>%filter(DateTime>storm10_begin-24*60*60&
                                DateTime<storm10_end)) +
  geom_point(aes(x=DateTime,y=CO2_umol.L_excess),color="grey50") +
  geom_point(aes(x=DateTime,y=Q_Ls*5),color="blue")+
  # geom_point(aes(x=DateTime,y=Q_Ls*CO2_umol.L/300),color="red") +
  #geom_smooth(aes(x=DateTime,y=CO2_umol.L_excess*5),method = "loess") +
  geom_smooth(aes(x=DateTime,y=CO2_umol.L_excess*5),color="grey10",method = lm, formula = y ~ splines::bs(x, 10), se = FALSE) +
  theme_bw(base_size = 14)




########
ggplot(all_stn %>%filter(DateTime>as.POSIXct("2021-01-01 00:00:00"))) +
  geom_point(aes(x=DateTime,y=CO2_umol.L*100),color="red") +
  geom_point(aes(x=DateTime,y=Q_Ls*1000),color="blue")+
  geom_point(aes(x=DateTime,y=CO2_umol.L*Q_Ls),color="grey50")

ggplot(all_stn #%>%filter(DateTime>as.POSIXct("2021-01-01 00:00:00"))
       ) +
  geom_point(aes(x=Q_Ls,y=CO2_umol.L*Q_Ls,color=DateTime))

  

ggplot(all_stn %>%filter(DateTime>storm11_begin#-200*60*60
                        &DateTime<storm11_end#+200*60*60
                        ) ,aes(x=DateTime,y=CO2_umol.L_excess)) + geom_point()

ggplot(all_stn %>%filter(DateTime>storm8_begin-24*60*60&DateTime<storm8_end)) +
  geom_point(aes(x=DateTime,y=CO2_umol.L_excess),color="black") +
  geom_point(aes(x=DateTime,y=Q_Ls),color="blue")+
  geom_point(aes(x=DateTime,y=Q_Ls*CO2_umol.L/300),color="red") +
  #geom_smooth(aes(x=DateTime,y=CO2_umol.L_excess*5),method = "loess") +
 geom_smooth(aes(x=DateTime,y=CO2_umol.L_excess*5),method = lm, formula = y ~ splines::bs(x, 20), se = FALSE) 


ggplot(allstn_storm) +
  geom_point(aes(x=time_elapsed,y=CO2_umol.L_excess_ave3,color=year_month)) +
  geom_line(aes(x=time_elapsed,y=Q_Ls, color=year_month))




ggplot(allstn_storm
       ,aes(x=time_elapsed,y=CO2_umol.L_excess)) + geom_point()
ggplot(allstn_storm
       ,aes(x=DateTime,y=CO2_umol.L_excess_ave3)) + geom_point()

ggplot(all_stn%>%filter(CO2_percentExcess*100>-50)
       ,aes(x=DateTime,y=CO2_percentExcess*100)) + geom_point()

ggplot(all_stn %>%filter(CO2_percentExcess*100>-50)
       ,aes(x=DateTime,y=CO2_umol.L)) + geom_point()

hist(allstn_storm$CO2_percentExcess*100)

##EC
EC_df <- read.csv(here::here("data_cleaned/EC_01_cleaned.csv"))
EC_df$DateTime <- as.POSIXct(EC_df$DateTime,format="%Y-%m-%d %H:%M",tz="UTC")

EC_df <- left_join(Stn01,EC_df,by='DateTime')
EC_df <- EC_df%>%drop_na(EC_uS_lowrange)%>%drop_na(Q_m3s)

ggplot(EC_df,aes(x=Q_Ls,y=EC_uS_lowrange,color=DateTime)) + geom_point()
ggplot(EC_df %>%filter(DateTime>storm9_begin
                        &DateTime<storm9_end
) ,aes(x=Q_Ls,y=EC_uS_lowrange,color=DateTime)) + geom_point()
