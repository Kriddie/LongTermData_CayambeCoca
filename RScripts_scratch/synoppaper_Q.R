#Q summary for synoptic paper
library(dplyr)
#first run LongTermData_stn scripts
#colm
WL_05_subset_july6 <- WL_05%>%filter(DateTime > as.POSIXct("2021-07-06 11:00:00") &
                                 DateTime < as.POSIXct("2021-07-06 15:00:00"))
WL_05_subset_july7 <- WL_05%>%filter(DateTime > as.POSIXct("2021-07-07 11:00:00") &
                                       DateTime < as.POSIXct("2021-07-07 15:00:00"))
WL_05_subset_july9 <- WL_05%>%filter(DateTime > as.POSIXct("2021-07-09 11:00:00") &
                                       DateTime < as.POSIXct("2021-07-09 15:00:00"))

WL_06_subset_july6 <- WL_06%>%filter(DateTime > as.POSIXct("2021-07-06 11:00:00") &
                                       DateTime < as.POSIXct("2021-07-06 15:00:00"))
WL_06_subset_july7 <- WL_06%>%filter(DateTime > as.POSIXct("2021-07-07 11:00:00") &
                                       DateTime < as.POSIXct("2021-07-07 15:00:00"))
WL_06_subset_july9 <- WL_06%>%filter(DateTime > as.POSIXct("2021-07-09 11:00:00") &
                                       DateTime < as.POSIXct("2021-07-09 15:00:00"))

stn05_Q <- rbind(WL_05_subset_july6,WL_05_subset_july7,WL_05_subset_july9)
stn06_Q <- rbind(WL_06_subset_july6,WL_06_subset_july7,WL_06_subset_july9)

stn05_Q_mean <- mean(stn05_Q$Q_m3s)
stn06_Q_mean <- mean(stn06_Q$Q_m3s)

#gavi

WL_01_subset_july18 <- WL_01%>%filter(DateTime > as.POSIXct("2021-07-18 11:00:00") &
                                       DateTime < as.POSIXct("2021-07-18 15:00:00"))
WL_01_subset_july22 <- WL_01%>%filter(DateTime > as.POSIXct("2021-07-22 11:00:00") &
                                       DateTime < as.POSIXct("2021-07-22 15:00:00"))

WL_02_subset_july18 <- WL_02%>%filter(DateTime > as.POSIXct("2021-07-18 11:00:00") &
                                        DateTime < as.POSIXct("2021-07-18 15:00:00"))
WL_02_subset_july22 <- WL_02%>%filter(DateTime > as.POSIXct("2021-07-22 11:00:00") &
                                        DateTime < as.POSIXct("2021-07-22 15:00:00"))

WL_03_subset_july18 <- WL_03%>%filter(DateTime > as.POSIXct("2021-07-18 11:00:00") &
                                        DateTime < as.POSIXct("2021-07-18 15:00:00"))
WL_03_subset_july22 <- WL_03%>%filter(DateTime > as.POSIXct("2021-07-22 11:00:00") &
                                        DateTime < as.POSIXct("2021-07-22 15:00:00"))

WL_04_subset_july18 <- WL_04%>%filter(DateTime > as.POSIXct("2021-07-18 11:00:00") &
                                        DateTime < as.POSIXct("2021-07-18 15:00:00"))
WL_04_subset_july22 <- WL_04%>%filter(DateTime > as.POSIXct("2021-07-22 11:00:00") &
                                        DateTime < as.POSIXct("2021-07-22 15:00:00"))

stn01_Q <- rbind(WL_01_subset_july18,WL_01_subset_july22)
stn02_Q <- rbind(WL_02_subset_july18,WL_02_subset_july22)
stn03_Q <- rbind(WL_03_subset_july18,WL_03_subset_july22)
stn04_Q <- rbind(WL_04_subset_july18,WL_04_subset_july22)

stn01_Q_mean <- mean(stn01_Q$Q_m3s,na.rm = TRUE)
stn02_Q_mean <- mean(stn02_Q$Q_m3s)
stn03_Q_mean <- mean(stn03_Q$Q_m3s)
stn04_Q_mean <- mean(stn04_Q$Q_m3s)

p4 <- plot_ly(WL_04, x = ~DateTime, y = ~Q_m3s, type = 'scatter', mode = 'markers') 
p1 <- plot_ly(WL_01, x = ~DateTime, y = ~Q_m3s, type = 'scatter', mode = 'markers') 

