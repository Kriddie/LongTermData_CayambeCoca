#modeling depth

library(here)
library(dplyr)
library(lubridate)
library(ggplot2)
library(lubridate)
library(lme4)
library(lmerTest)
library(jtools)
library(extraoperators)
library(JWileymisc)
library(multilevelTools)
library(tidyr)
#depth is a very important parameter in stream metabolizer, but it
#is not easy to estimate.
# we want to estimate "z hat" wich is the mean water depth across the length and width of the reach (m)

#look at how depth relates to average depth

WidthDepth <- read.csv(here::here("WidthDepth/WidthDepth_summary_2.csv"))
WidthDepth$DateTime <- as.POSIXct(paste(WidthDepth$Date,WidthDepth$Time,sep=" "),format="%m/%d/%Y %H:%M", tz="UTC")
WidthDepth$DateTime <- round_date(WidthDepth$DateTime, unit="15 mins")

df <- read.csv(here::here("data_cleaned/WL_04_cleaned.csv"))%>%select(DateTime,WL_m,Q_m3s)%>%rename(WL_stn04=WL_m,Q_stn04=Q_m3s)
df$DateTime <- as.POSIXct(df$DateTime,format="%Y-%m-%d %H:%M:%S", tz="UTC")
WidthDepth <- left_join(WidthDepth,df,by="DateTime")

df <- read.csv(here::here("data_cleaned/WL_01_cleaned.csv"))%>%select(DateTime,WL_m,Q_m3s)%>%rename(WL_stn01=WL_m,Q_stn01=Q_m3s)
df$DateTime <- as.POSIXct(df$DateTime,format="%Y-%m-%d %H:%M", tz="UTC")
WidthDepth <- left_join(WidthDepth,df,by="DateTime")

df <- read.csv(here::here("data_cleaned/WL_02_cleaned.csv"))%>%select(DateTime,WL_m,Q_m3s)%>%rename(WL_stn02=WL_m,Q_stn02=Q_m3s)
df$DateTime <- as.POSIXct(df$DateTime,format="%Y-%m-%d %H:%M", tz="UTC")
WidthDepth <- left_join(WidthDepth,df,by="DateTime")

df <- read.csv(here::here("data_cleaned/WL_03_cleaned.csv"))%>%select(DateTime,WL_m,Q_m3s)%>%rename(WL_stn03=WL_m,Q_stn03=Q_m3s)
df$DateTime <- as.POSIXct(df$DateTime,format="%Y-%m-%d %H:%M", tz="UTC")
WidthDepth <- left_join(WidthDepth,df,by="DateTime")

WidthDepth$Date <- as.Date(WidthDepth$Date,format="%m/%d/%Y")
WidthDepth$source <- "2021_metabolism"

#thalwag to average depth 
lm_1 <- lm(Average.depth ~ Thalwag.depth,data=WidthDepth)
summary(lm_1)
#Average.depth = Thalwag.depth*0.69473 +  2.40099
#r2 = 0.7736

#with 2022 data added
#Average.depth = Thalwag.depth*0.74636 +  2.13520    
#r2 = 0.8153


#we also have some data from 2021

df <- read.csv(here::here("data_cleaned/WL_04_cleaned.csv"))%>%select(DateTime,WL_m,Q_m3s)%>%rename(WL_stn04=WL_m,Q_stn04=Q_m3s)
df$DateTime <- as.POSIXct(df$DateTime,format="%Y-%m-%d %H:%M:%S", tz="UTC")

df_2021 <- read.csv(here::here("WidthDepth/DepthWidthVelocity_InjectionNotes2021.csv"))
df_2021$Site_ID <- paste(df_2021$site,df_2021$distance_m,sep="_")
df_2021$DateTime <- as.POSIXct(paste(df_2021$Date,"13:00:00",sep=" "),format="%m/%d/%Y %H:%M", tz="UTC")
df_2021 <- left_join(df_2021,df,by="DateTime")
df_2021$source <- "2021_injection"
df_2021$Date <- as.Date(df_2021$Date,format="%m/%d/%Y")


ggplot(df_2021,aes(x=Q_stn04,y=depth_cm ,color=Site_ID)) + geom_point() +
  geom_line() +
  scale_y_continuous(transform = "log")+
  scale_x_continuous(transform = "log")

#and then we have that full reach data from 2021
geomorph_df <- read.csv(here::here("WidthDepth/WidthDepth_Geomorph_2024-08-30.csv"))%>%select(Date,Time,DateTime,x,width,depth,notes)
geomorph_df$DateTime <- as.POSIXct(geomorph_df$DateTime,format="%m/%d/%Y %H:%M")
df <- read.csv(here::here("data_cleaned/WL_04_cleaned.csv"))%>%select(DateTime,WL_m,Q_m3s)%>%rename(WL_stn04=WL_m,Q_stn04=Q_m3s)
df$DateTime <- as.POSIXct(df$DateTime,format="%Y-%m-%d %H:%M:%S", tz="UTC")
geomorph_df <- left_join(geomorph_df,df,by="DateTime")
geomorph_df$depth <- as.numeric(geomorph_df$depth)
geomorph_df$Date <- as.Date(geomorph_df$Date,format = "%m/%d/%Y")
geomorph_df$source <- "geomorph"

#summarize all
WidthDepth_summary2 <- WidthDepth%>%
  group_by(Date,source) %>%
  summarise(
    Thalwag.depth = mean(Thalwag.depth,na.rm=TRUE),
    width_cm = mean(width..cm.,na.rm=TRUE),
    Q_stn04 = mean(Q_stn04),
    WL_stn04 = mean(WL_stn04))
WidthDepth_summary2$velocity_ms <- NA
df_2021_summary <- df_2021 %>%
  group_by(Date,source) %>%
  summarise(
    Thalwag.depth = mean(depth_cm,na.rm=TRUE),
    width_cm = mean(width_cm,na.rm=TRUE),
    velocity_ms = mean(velocity_ms,na.rm=TRUE),
    Q_stn04 = mean(Q_stn04),
    WL_stn04 = mean(WL_stn04))
geomorph_summary2 <- geomorph_df%>%drop_na(depth)%>%
  group_by(Date,source) %>%
  summarise(
    Thalwag.depth = mean(depth,na.rm=TRUE),
    width_cm = mean(width,na.rm=TRUE),
    Q_stn04 = mean(Q_stn04),
    WL_stn04 = mean(WL_stn04))
geomorph_summary2$velocity_ms <- NA

df_all <- rbind(WidthDepth_summary2,df_2021_summary,geomorph_summary2)
df_all$Average.depth <- df_all$Thalwag.depth*0.69473 +  2.40099


ggplot(df_all,aes(x=Average.depth,y=width_cm)) + geom_point()
ggplot(df_all ,aes(x=Q_stn04,y=Average.depth,color=source)) + geom_point()+
  scale_y_continuous(transform="log") + scale_x_continuous(transform="log")

ggplot(df_all ,aes(x=Q_stn04,y=velocity_ms,color=source)) + geom_point()+
  scale_y_continuous(transform="log") + scale_x_continuous(transform="log")

ggplot(df_all ,aes(x=Q_stn04,y=width_cm,color=source)) + geom_point()+
  scale_y_continuous(transform="log") + scale_x_continuous(transform="log")

#now fit this equation
#D=c*Q^f
#D =average depth
#Q = discharge
#c = average depth at unit discharge
#f is unitless coeficient

# Sample data
x <- df_all$Q_stn04
y <- df_all$Average.depth/100

# Fit nonlinear model: y = m * x^p
model <- nls(y ~ c * x^f, start = list(c = .4, f = .3))
summary(model) # Display summary
coef(model) # Extract estimated coefficients

c =0.6383832
f = 0.3108922 

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
