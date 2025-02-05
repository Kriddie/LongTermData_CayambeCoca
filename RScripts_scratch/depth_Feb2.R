#modeling depth

library(here)
library(dplyr)
library(lubridate)
library(ggplot2)
#depth is a very important parameter in stream metabolizer, but it
#is not easy to estimate.
# we want to estimate "z hat" wich is the mean water depth across the length and width of the reach (m)

#I want to know - what is the length of the reach?
    # metabolism reach length = -ln(1-0.8)*v/K[02]
    # where v is the cross-section averaged river velocity in m d−1
#calculate v
  #dividing daily average discharge by the estimated cross-sectional channel area for that day: 
    #v = Q/A[fm]
    #where Q = discharge
    #where A[Fm]
#calculate Afm - is the field measured channel cross-sectional area
    # A[fm] = m*GH+b
    # where GH is gauge height and
    # where m and b for this equation are model coefficients determined from 
        #a linear regression of the field measured cross-sectional channel area against 
          #measured gage height for the days of the field measurements.



##### Step 1
#calculate A[fm] for station 4

#read in WL station data
df <- read.csv(here::here("data_cleaned/WL_04_cleaned.csv"))
df$DateTime <- as.POSIXct(df$DateTime,format="%Y-%m-%d %H:%M:%S", tz="UTC")
#build rating curve between area and GH
#GH = WL_m
#find WL for time stamp
timestamp <- as.POSIXct("2022-07-27 12:15",tz="UTC")
df%>%filter(DateTime==timestamp)

#break it up into 2: Before 7/18/21

#here is the equation  Before 7/18/21
  #Area =  1.1168x + 0.0095R² = 0.6446
#after 7/18/21
  #Area = 0.9563x - 0.0621

df_sub1 <- df%>%filter(DateTime < as.POSIXct("2021-07-18 02:45",tz="UTC"))
df_sub2 <- df%>%filter(DateTime > as.POSIXct("2021-07-18 06:00",tz="UTC"))

df_sub1$Area <- 1.1168*df_sub1$WL_m + 0.0095
df_sub2$Area <- 0.9563*df_sub2$WL_m - 0.0621

WL_04_df <- rbind(df_sub1,df_sub2)

#now calc velocity #v = Q/A[fm]
WL_04_df$v_ms <- WL_04_df$Q_m3s/WL_04_df$Area




p3 <- plot_ly(df, x = ~DateTime, y = ~v_ms, type = 'scatter', mode = 'markers') 
