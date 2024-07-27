#explore stn02 metabolism data

# i need to get some findings for my AGU abstract

library(dplyr)
Stn02_df_2022_10_16 <- read_csv("MergedFiles/Stn02_df_2022-10-16.csv")
Stn02_df_2022_10_16$DateTime <- as.POSIXct(Stn02_df_2022_10_16$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
Stn02_df_2022_10_16$date <- as.Date(Stn02_df_2022_10_16$DateTime)

Stn02_co2_summary <- Stn02_df_2022_10_16%>%group_by(date)%>%
  summarise(
    ppm_ave = mean(ppm,na.rm = TRUE)
  )

WL_02 <- read.csv(here::here("data_cleaned/WL_02_cleaned.csv"))
WL_02$DateTime <- as.POSIXct(WL_02$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")


stn02 <- read.csv(here::here("metabolizer_dataframe/stn02_df_july10.csv"))
stn02$solar.time <- as.POSIXct(stn02$solar.time,format="%Y-%m-%d %H:%M:%S",tz="UTC")
stn02$date <- as.Date(stn02$solar.time)

stn02_summary <- stn02 %>%group_by(date)%>%
  summarise(
    discharge_mean = mean(discharge,na.rm = TRUE),
    temp_mean = mean(temp.water,na.rm = TRUE),
    depth_mean = mean(depth,na.rm = TRUE),    
  )

stn02_summary <- full_join(stn02_summary,data_inspection,by="date")
stn02_summary <- full_join(stn02_summary,Stn02_co2_summary,by="date")

stn02_summary$month <- format(stn02_summary$date, "%m")

ggplot(stn02_summary,aes(x=discharge_mean,y=ER.daily)) + geom_point()
ggplot(stn02_summary,aes(x=log(discharge_mean),y=ER.daily,color=month)) +
  geom_point()
ggplot(stn02_summary,aes(x=log(discharge_mean),y=GPP.daily,color=month)) +
  geom_point()
ggplot(stn02_summary,aes(x=ER.daily,y=GPP.daily,color=month)) +
  geom_point()

ggplot(stn02_summary,aes(x=date,y=ER.daily-GPP.daily,color=month)) +
  geom_point()

ggplot(stn02_summary,aes(x=month,y=GPP.daily,color=month)) +
  geom_point()
ggplot(stn02_summary,aes(x=month,y=GPP.daily/-ER.daily,color=month)) +
  geom_point()

ggplot(stn02_summary%>%filter(date>"2021-06-01"& date < "2022-01-01")) +
  geom_line(aes(x=date,y=discharge_mean*10),color="blue",linewidth=1) +
  geom_line(aes(x=date,y=GPP.daily),color="red") + 
  geom_line(aes(x=date,y=-ER.daily),color="black") + 
  geom_line(aes(x=date,y=ppm_ave*.001),color="grey") 

  
ggplot(stn02_summary%>%filter(date<"2019-09-01")) +
    geom_line(aes(x=date,y=discharge_mean*10),color="blue",linewidth=1) +
    geom_line(aes(x=date,y=GPP.daily),color="red") + 
  geom_line(aes(x=date,y=-ER.daily*.1),color="black") + 
  geom_line(aes(x=date,y=ppm_ave*.01),color="grey") 

ggplot(stn02_summary%>%filter(date>"2021-06-01"),aes(x=discharge_mean,y=GPP.daily,color=month)) +
  geom_point()

ggplot(stn02_summary%>%filter(date>"2021-06-01"),aes(x=-ER.daily,y=GPP.daily,color=date)) +
  geom_point()

ggplot(stn02_summary%>%filter(date>"2021-06-01"),aes(x=-ER.daily,y=ppm_ave,color=date)) +
  geom_point()
ggplot(stn02_summary%>%filter(date>"2021-06-01"),aes(x=GPP.daily,y=ppm_ave,color=date)) +
  geom_point()



