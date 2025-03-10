#methane

library(here)
library(dplyr)
library(tidyr)
library(lubridate)

df1 <- read.csv(here::here("WaterChem/PondPaper_k600.csv"))%>%
  dplyr::select(DateTime,Site,Watertemp_c,pCH4_ppm,CH4_umol.L)%>%
  filter(Site=="Wetland12")
df1$Site <- "Gavilan Center"
df1$Site2 <- "Center"
df1$DateTime <- as.POSIXct(df1$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")

df2 <- read.csv(here::here("WaterChem/Drone_Gavi_sampling_GHG_2025.csv"))%>%
  dplyr::select(DateTime,Site,Site2,Watertemp_c,pCH4_ppm,CH4_umol.L)%>%
  drop_na(CH4_umol.L)%>%filter(Site2=="waypt11"|Site2=="waypt12")
df2$Site <- "Gavilan Edges"
df2$DateTime <- as.POSIXct(df2$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")


df3 <- read.csv(here::here("WaterChem/Methane_2022.csv"))%>%
  filter(Site=="Gavi-main")%>%rename(CH4_umol.L=CH4_umol_L.1,Site2=Location)
df3$Site <- "Gavilan Outlet"
df3$DateTime <- as.POSIXct(paste(df3$Date_collected,df3$Time_collected),format="%m/%d/%y %H:%M",tz="UTC")

#convert to ppm
#henry's constant CH4
kH_STP_mol.L.atm = .0014182
dlnHcppersperK = 1600
kH_STP_mol.l.atm = .035*1/0.986923
D_K = 2400 
T_STP_K = 298.15
df3$KH_CH4_mol.L.atm <- kH_STP_mol.L.atm*exp(dlnHcppersperK*(1/(df3$Watertemp_c+273.15)-1/T_STP_K))

#Ambient CH4 concentration ppb	1910.97 average 2021 from Moaa
CH4_air_ppb <- 1910.97 
df3$CH4_air_atm <- df3$AirPress_Field_kPa/101.3 * CH4_air_ppb * 10^-9
df3$CH4_sat_umol.L <- df3$CH4_air_atm*df3$KH_CH4_mol.L.atm * 10^6
df3$CH4_pSat <- df3$CH4_umol.L/df3$CH4_sat_umol.L *100

#calculate the partial pressure of ch4
df3$pCH4_ppm <- df3$CH4_umol.L /df3$KH_CH4_mol.L.atm
df3 <- df3%>%dplyr::select(Site,Site2,DateTime,Watertemp_c,pCH4_ppm,CH4_umol.L)%>%drop_na(CH4_umol.L)
#bind
df <- rbind(df1,df2,df3)

#wl
WL_df <- read.csv(here::here("data_cleaned/WL_02_cleaned.csv"))
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")

#DO
DO1_df <- read.csv(here::here("data_cleaned/DO_01_cleaned.csv"))
DO1_df$DateTime <- as.POSIXct(DO1_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")

DO2_df <- read.csv(here::here("data_cleaned/DO_02_cleaned.csv"))
DO2_df$DateTime <- as.POSIXct(DO2_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")
test <- DO2_df%>%filter(DateTime > as.POSIXct("2022-11-06 10:30:00",tz="UTC")&
                  DateTime < as.POSIXct("2023-5-03 15:30:00",tz="UTC"))

ggplot(test,aes(x=DateTime,y=DO_mgL,color=DateTime)) + geom_point()


DO_df <- inner_join(DO1_df,DO2_df,by="DateTime")
ggplot(DO_df,aes(x=DO_mgL.x,y=DO_mgL.y,color=DateTime)) + geom_point()


DO2_df <- DO2_df%>%filter(DateTime > as.POSIXct("2022-06-06 10:30:00",tz="UTC")&
                          DateTime < as.POSIXct("2022-12-03 15:30:00",tz="UTC"))


DO_inj1 <- read.csv(here::here("DO/DO_injection/Injection_DO_xn10_2022-07-01.csv"),skip=1)%>%
  rename(DateTime=Date.Time..GMT.05.00,DO_mgL=DO.conc..mg.L..LGR.S.N..20645539..SEN.S.N..20645539.,DOTemp_c=Temp...C..LGR.S.N..20645539..SEN.S.N..20645539.)%>%
  dplyr::select(DateTime,DO_mgL,DOTemp_c)
DO_inj1$Station <- "inj_n10"
DO_inj1$DateTime <- as.POSIXct(DO_inj1$DateTime, format="%m/%d/%y %I:%M:%S %p",tz="UTC")

DO_inj2 <- read.csv(here::here("DO/DO_injection/Injection_DO_x0m_2022-07-06.csv"),skip=1)%>%
  rename(DateTime=Date.Time..GMT.05.00,DO_mgL=DO.conc..mg.L..LGR.S.N..20645539..SEN.S.N..20645539.,DOTemp_c=Temp...C..LGR.S.N..20645539..SEN.S.N..20645539.)%>%
  dplyr::select(DateTime,DO_mgL,DOTemp_c)
DO_inj2$DateTime <- as.POSIXct(DO_inj2$DateTime, format="%m/%d/%y %I:%M:%S %p",tz="UTC")
DO_inj2$DateTime <- round_date(DO_inj2$DateTime,"15 mins")
DO_inj2$Station <- "inj_0m"

DO_df <- rbind(DO2_df,DO_inj1,DO_inj2)


#join

#DO_df <- left_join(DO_df,WL_df,by="DateTime")

df_full <- left_join(df,DO_df,by="DateTime")
df_full <- left_join(df_full,WL_df,by="DateTime")

library("stringr")								 

df_full$Site2 <-  str_remove_all(df_full$Site2," ")


#write out df
#write.csv(df_full,here::here("WaterChem/methane_df_merged.csv"))

#plot 
ggplot(df_full%>%drop_na(CH4_umol.L)%>%filter(Site=="Gavilan Outlet")%>%
         filter(Site2=="0m"),
       aes(x=DO_mgL,y=CH4_umol.L,color=DateTime)) + geom_point()

ggplot(df_full%>%drop_na(CH4_umol.L)%>%filter(Site=="Gavilan Outlet")%>%
         filter(Site2=="50m"),
       aes(x=DO_mgL,y=CH4_umol.L,color=DateTime)) + geom_point()

ggplot(df_full%>%drop_na(CH4_umol.L)%>%filter(Site=="Gavilan Outlet")%>%
         filter(Site2=="100m"),
       aes(x=DO_mgL,y=CH4_umol.L,color=DateTime)) + geom_point()

ggplot(df_full %>%drop_na(CH4_umol.L)%>%filter(Site=="Gavilan Outlet")%>%
         filter(DateTime==as.POSIXct("2022-06-06 12:30:00",tz="UTC"))
     ,
       aes(x=Site2,y=CH4_umol.L,color=as.factor(DateTime))) + geom_point()

##

ggplot(df_full %>%drop_na(CH4_umol.L)%>%filter(Site=="Gavilan Outlet")%>%
         filter(Site2=="0m"),
       aes(x=Q_m3s,y=CH4_umol.L,color=DateTime)) + geom_point()

ggplot(DO_df ,
       aes(x=Q_m3s,y=DO_mgL,color=DateTime)) + geom_point() +
  scale_y_log10() + scale_x_log10() 



ggplot(df_full%>%drop_na(CH4_umol.L)%>%filter(Site=="Gavilan Center"),
       aes(x=DO_mgL,y=CH4_umol.L,color=DateTime)) + geom_point()

ggplot(df_full%>%drop_na(CH4_umol.L)%>%filter(Site=="Gavilan Edges"),
       aes(x=DO_mgL,y=CH4_umol.L,color=DateTime)) + geom_point()
