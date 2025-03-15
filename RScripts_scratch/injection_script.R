#how far does wetland CO2 go downstream?

#injecion date: 2022-06-06
#time:
####stn01
WL_df <- read.csv(here::here("data_cleaned/WL_01_cleaned.csv"))
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")
CO2_df <- read.csv(here::here("data_cleaned/CO2_01_max10000_cleaned.csv")) 
CO2_df$DateTime <- as.POSIXct(CO2_df$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")

stn01 <- full_join(WL_df,CO2_df,by=c("DateTime","Station"))

#### station 2
WL_df <- read.csv(here::here("data_cleaned/WL_02_cleaned.csv"))
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")
CO2_df <- read.csv(here::here("data_cleaned/CO2_02_max10000_cleaned.csv")) 
CO2_df$DateTime <- as.POSIXct(CO2_df$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")

stn02 <- full_join(WL_df,CO2_df,by=c("DateTime","Station"))

###stn03
WL_df <- read.csv(here::here("data_cleaned/WL_03_cleaned.csv"))
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")
CO2_df <- read.csv(here::here("data_cleaned/CO2_03_max10000_cleaned.csv")) 
CO2_df$DateTime <- as.POSIXct(CO2_df$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")

stn03 <- full_join(WL_df,CO2_df,by=c("DateTime","Station"))
stn03 <- stn03%>%filter(DateTime > as.POSIXct("2021-03-15 03:30:00"))

### station 04
WL_df <- read.csv(here::here("data_cleaned/WL_04_cleaned.csv"))
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")
CO2_df <- read.csv(here::here("data_cleaned/CO2_04_max10000_cleaned.csv")) 
CO2_df$DateTime <- as.POSIXct(CO2_df$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")

stn04 <- full_join(WL_df,CO2_df,by=c("DateTime","Station"))

### join all
all_stn <- full_join(stn01%>%select(DateTime,Q_m3s,CO2_ppm_adjusted)%>%rename(Q_m3s_01 = Q_m3s,CO2_ppm_01=CO2_ppm_adjusted),
                     stn02%>%select(DateTime,Q_m3s,CO2_ppm_adjusted)%>%rename(Q_m3s_02 = Q_m3s,CO2_ppm_02=CO2_ppm_adjusted),
                     by="DateTime")
all_stn <- full_join(all_stn,
                     stn03%>%select(DateTime,Q_m3s,CO2_ppm_adjusted)%>%rename(Q_m3s_03 = Q_m3s,CO2_ppm_03=CO2_ppm_adjusted),
                     by="DateTime")
all_stn <- full_join(all_stn,
                     stn04%>%select(DateTime,Q_m3s,CO2_ppm_adjusted)%>%rename(Q_m3s_04 = Q_m3s,CO2_ppm_04=CO2_ppm_adjusted),
                     by="DateTime")

all_stn <- all_stn%>%filter(DateTime < as.POSIXct("2023-04-01 11:15:00",tz="UTC"))
all_stn$Date <- as.Date(all_stn$DateTime) 


#read in Injection data
df_2022June06 <- read.csv(here::here("Injection_data/Inj_CO2_2022-06-06.csv"))
df_2022June06$DateTime <- as.POSIXct(df_2022June06$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")
df_2022July06 <- read.csv(here::here("Injection_data/Inj_CO2_2022-07-06.csv"))
df_2022July06$DateTime <- as.POSIXct(df_2022July06$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")
df_2021July14 <- read.csv(here::here("Injection_data/Inj_df_2021July14.csv"))
df_2021July14$DateTime <- as.POSIXct(df_2021July14$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")


df_2022June06_compare <- left_join(df_2022June06,all_stn%>%filter(Date==as.Date("2022-06-06")),by="DateTime")
df_2022July06_compare <- left_join(df_2022June06,all_stn%>%filter(Date==as.Date("2022-07-06")),by="DateTime")
df_2021July14_compare <- left_join(df_2022June06,all_stn%>%filter(Date==as.Date("2021-07-14")),by="DateTime")

ggplot(all_stn%>%filter(DateTime > as.POSIXct("2021-06-05 00:00:00")&DateTime < as.POSIXct("2022-07-20 00:00:00")))+
  geom_point(aes(x=DateTime,y=CO2_ppm_03))

ggplot(all_stn%>%filter(DateTime > as.POSIXct("2021-06-05 00:00:00")#&DateTime < as.POSIXct("2022-07-07 00:00:00")
                        ))+
  geom_point(aes(x=DateTime,y=CO2_ppm_04))

plot_ly(data=all_stn%>%filter(DateTime > as.POSIXct("2021-06-05 00:00:00")), x=~DateTime,y= ~CO2_ppm_04)
