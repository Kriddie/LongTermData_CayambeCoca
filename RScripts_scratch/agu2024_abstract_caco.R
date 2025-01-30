#explore stn02 metabolism data

# i need to get some findings for my AGU abstract
library(readr)
library(dplyr)
library(ggpubr)



load("C:/Users/whitm/OneDrive - University of North Carolina at Chapel Hill/StreamMetabolizer_results/model_stn02_partially-pooled_all.RData")
data_inspection <- get_params(mm)

plot_metab_preds(mm)

Stn01_2023_03_29 <- read_csv("MergedFiles/Stn01_2023-03-29.csv")
Stn01_2023_03_29$DateTime <- as.POSIXct(Stn01_2023_03_29$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
Stn01_2023_03_29$date <- as.Date(Stn01_2023_03_29$DateTime)

Stn01_co2_summary <- Stn01_2023_03_29%>%group_by(date)%>%
  summarise(
    ppm_ave = mean(adjusted_ppm,na.rm = TRUE)
  )


#Stn02_df_2022_10_16 <- read_csv("MergedFiles/Stn02_df_2022-10-16.csv")
#Stn02_df_2022_10_16$DateTime <- as.POSIXct(Stn02_df_2022_10_16$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
#Stn02_df_2022_10_16$date <- as.Date(Stn02_df_2022_10_16$DateTime)

#Stn02_co2_summary <- Stn02_df_2022_10_16%>%group_by(date)%>%
#  summarise(
#    ppm_ave = mean(ppm,na.rm = TRUE))

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
stn02_summary <- full_join(stn02_summary,Stn01_co2_summary,by="date")

stn02_summary$month <- format(stn02_summary$date, "%m")

stn02_summary$co2_load <- stn02_summary$ppm_ave * stn02_summary$discharge_mean

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
    geom_line(aes(x=date,y=discharge_mean),color="blue",linewidth=1) +
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

ggplot(stn02_summary,aes(x=date,y=co2_load,color=date)) +
  geom_point()

####plot for diego

stno2_2019 <- stn02_summary%>%filter(date<"2019-08-14")
stno2_2019_metab <- stno2_2019%>%select(date,GPP.daily,GPP.daily.sd,ER.daily,ER.daily.sd)

# plot the point plot 
p1<-ggplot(stno2_2019_metab) +  
  geom_point(aes(x=date, y=GPP.daily),color="darkgreen",size=1)+ 
  geom_line(aes(x=date, y=GPP.daily),color="darkgreen",linewidth=1)+ 
  geom_errorbar(aes(x=date,ymin=GPP.daily-GPP.daily.sd, ymax=GPP.daily+GPP.daily.sd), width=.5, 
                position=position_dodge(0.05),color="darkgreen") +  
  geom_point(aes(x=date, y=ER.daily),color="orange",size=1)+
  geom_line(aes(x=date, y=ER.daily),color="orange",linewidth=1)+
  geom_errorbar(aes(x=date,ymin=ER.daily-ER.daily.sd, ymax=ER.daily+ER.daily.sd), width=.5, 
                position=position_dodge(0.05),color="orange")  +
  ylab("GPP/ER [m^-2 d^-1]") +
  theme_bw(base_size = 16)+ 
  theme(axis.title.x=element_blank())

p1

p3<-ggplot(WL_02, aes(x=date, y=discharge_mean*1000)) +  
  geom_bar(stat = "identity",color="blue",fill="blue") +
   ylab("mean daily Q [L/s]") +
  theme_bw(base_size = 16)+ 
  theme(axis.title.x=element_blank()#,
      #  axis.text.x=element_blank()
      )

p3

p4<-ggplot(Stn01_2023_03_29%>%filter(date<"2019-08-14"), aes(x=DateTime, y=adjusted_ppm)) +  
  geom_line(linewidth=1,color="black") + ylab("CO2 [ppm]") +
  theme_bw(base_size = 16)+ 
  theme(axis.title.x=element_blank()#,
#    axis.text.x=element_blank())
) 

p4

ggarrange(p3, p4, p1, ncol = 1)




