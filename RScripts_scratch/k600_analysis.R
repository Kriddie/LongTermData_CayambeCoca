##time to think about k600.


#stream reach above waterfall 

#chamber method

#empirical equations

#CO2 injection


#waterfall in isolation

df <- read.csv(here::here("MergedFiles/all_2019_Data.csv"))%>%
  select(DateTime,Inj.x,Chlorophylla_ug.L,Phycocyanin_ppb,Turbidity_NTU,V1_adjusted,V2_adjusted,V3_adjusted,V4_adjusted,V1,V2,V3,V4)%>%
  rename(Inj = Inj.x)
df$DateTime <- as.POSIXct(df$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")

WL_df <- read.csv(here::here("data_cleaned/WL_02_cleaned.csv"))
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")

df <- left_join(df,WL_df,by="DateTime")
injection_df <- df%>%filter(Inj=="Yes")

injection_df_1 <- injection_df%>%filter(DateTime< "2019-07-22")
injection_df_2 <- injection_df%>%filter(DateTime > "2019-07-22" & DateTime < "2019-07-24")
injection_df_3 <- injection_df%>%filter(DateTime > "2019-07-24"& DateTime < "2019-07-29")
injection_df_4 <- injection_df%>%filter(DateTime > "2019-07-29"& DateTime < "2019-08-05")
injection_df_5 <- injection_df%>%filter(DateTime > "2019-08-05"& DateTime < "2019-08-07")
injection_df_6 <- injection_df%>%filter(DateTime > "2019-08-07")

#these are the ones that have all data
#injection_df_2,injection_df_3,injection_df_5,injection_df_6

ggplot(injection_df_5) + 
  geom_point(aes(x=DateTime,y=V1_adjusted),color="red") +
  geom_point(aes(x=DateTime,y=V2_adjusted),color="blue")+
  geom_point(aes(x=DateTime,y=V3_adjusted),color="green")+
  geom_point(aes(x=DateTime,y=V4_adjusted),color="orange")


#C6 data while I'm at it
C6_df$Phycocyanin_ppb <- df%>%drop_na(Chlorophylla_ug.L)

ggplot(full_df,aes(x=Q_m3s,y=Chlorophylla_ug.L,color=DateTime)) + geom_point() +
  scale_y_continuous(transform = "log") + scale_x_continuous(transform = "log")
ggplot(full_df,aes(x=Q_m3s,y=Turbidity_NTU,color=DateTime)) + geom_point()+
  scale_y_continuous(transform = "log") + scale_x_continuous(transform = "log")
ggplot(full_df,aes(x=Q_m3s,y=Phycocyanin_ppb,color=DateTime)) + geom_point()+
  scale_y_continuous(transform = "log") + scale_x_continuous(transform = "log")
