#Area -rating curve

#this script is to help create the dataframe to develope a
#linear relationship between cross-section are of the stream
#and depth

#STEPS:
  #1. read in data
  #2. #find WL for time stamp
  #3. enter into excel doc - "Area_ratingcurve"
  #4. Make empirical relationship


#build rating curve between area and GH
#GH = WL_m
#find WL for time stamp

timestamp <- as.POSIXct("2022-07-27 09:30",tz="UTC")
df%>%filter(DateTime==timestamp)

p3 <- plot_ly(df, x = ~DateTime, y = ~WL_m, type = 'scatter', mode = 'markers') 


############
#station 02
############
df <- read.csv(here::here("data_cleaned/WL_02_cleaned.csv"))
df$DateTime <- as.POSIXct(df$DateTime,format="%Y-%m-%d %H:%M:%S", tz="UTC")

#not the strongest relationship:
#y = 0.2841x + 0.0203
#R² = 0.1519

df$Area <- df$WL_m * 0.2841 + 0.0203
#now calc velocity #v = Q/A[fm]
df$v_ms <- df$Q_m3s/df$Area

K_02 = 20/(24*60*60)
df$reach_length = .7*df$v_ms/K_02

p3 <- plot_ly(df, x = ~DateTime, y = ~reach_length, type = 'scatter', mode = 'markers') 


##############
##Station 04##
##############

df <- read.csv(here::here("data_cleaned/WL_04_cleaned.csv"))
df$DateTime <- as.POSIXct(df$DateTime,format="%Y-%m-%d %H:%M:%S", tz="UTC")

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