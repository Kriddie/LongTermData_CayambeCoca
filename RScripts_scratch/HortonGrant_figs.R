#play with data to make horton grant figure

library(lubridate)
library(dplyr)
library(ggplot2)
library(here)
#plot

plot(Stn01$DO_mgL, Stn01$adjusted_ppm)

plot(Stn01$Q_Ls, Stn01$adjusted_ppm)

plot(Stn01$Q_Ls, Stn01$DO_mgL)


###

Stn01$Date <- as.Date(Stn01$DateTime)

Stn01_pivot <- Stn01 %>% 
  group_by(Date) %>% 
  summarise(ppm_mean = mean(ppm),
            WL_m_mean = mean(WL_m),
            Q_Ls_mean = mean(Q_Ls),
            DO_mgL_mean = mean(DO_mgL),
            WLTemp_c_mean = mean(WLTemp_c),
            DO_mgL_std = std(DO_mgL))

Stn01_pivot$Month <- as.numeric(format(as.Date(Stn01_pivot$Date),"%m"))

Stn01_pivot$ppm_mean[Stn01_pivot$ppm_mean > 10000] <- 10000
Stn01_pivot$ppm_mean[Stn01_pivot$ppm_mean < 20] <- NA


#wet season == June. July, August


Stn01_pivot_wet <- Stn01_pivot%>%filter(Month >= 6 & Month <= 8)
#Stn01_pivot_wet <- Stn01_pivot_wet$DO_mgL_mean
Stn01_pivot_wet$season <- "wet"
Stn01_pivot_dry <- Stn01_pivot%>%filter(Month <= 6 | Month > 8)
Stn01_pivot_dry$season <- "dry"

Stn01_pivot_season <- rbind(Stn01_pivot_dry,Stn01_pivot_wet)

hist(Stn01_pivot_wet$ppm_mean)
hist(Stn01_pivot_dry$ppm_mean)
hist(Stn01_pivot_wet$DO_mgL_mean)
hist(Stn01_pivot_dry$DO_mgL_mean)

# Overlaid histograms

#plot 1A
ggplot(Stn01_pivot_season, aes(x=DO_mgL_mean, color=season,fill=season)) +
  geom_histogram( alpha=0.5, position="identity", bins = 10) + 
  theme_classic()  +
  geom_vline(xintercept = 2, linetype="dotted", color = "black", size=1.5) +
  theme(text = element_text(size = 20)) +
  xlab("Dissolved oxygen [mg/L]")

#plot 1B
ggplot(Stn01_pivot_season, aes(x=DO_mgL_std, color=season,fill=season)) +
  geom_histogram( alpha=0.5, position="identity", bins = 40) + 
  theme_classic()  +
  xlim(-5,5) +
  theme(text = element_text(size = 20)) +
  xlab("Dissolved oxygen [mg/L]")

##########
ggplot(Stn01_pivot_season, aes(x=ppm_mean, color=season,fill=season)) +
  geom_histogram( alpha=0.5, position="identity", bins = 10) + 
  theme_classic() 


#plot 2A
ggplot(Stn01_pivot_season, aes(x=DO_mgL_mean, y=ppm_mean, fill=season)) +
  geom_point(size=4, shape=21, color="black") +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  xlab("Dissolved oxygen [mg/L]") + ylab("CO2 [ppm]")

#plot 2B
ggplot(Stn01%>%drop_na(Q_Ls), aes(x=DO_mgL, y=ppm,color=log(Q_Ls))) +
  geom_point(size=4, shape=21) +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  xlab("Dissolved oxygen [mg/L]") + ylab("CO2 [ppm]")

ggplot(Stn01%>%drop_na(Q_Ls), aes(x=DO_mgL, y=Q_Ls)) +
  geom_point(size=4, shape=21) +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  xlab("Dissolved oxygen [mg/L]") + ylab("Q [L/s]")

########
precipt_df <- read.csv(here::here("Weather_station/M5025_Precipitation.csv")) 
precipt_df$fecha <- as.POSIXct(precipt_df$fecha, format="%m/%d/%y %H:%M", tz="UTC") 
precipt_df$Date <- as.Date(precipt_df$fecha)

precipt_pivot <- precipt_df %>% 
  group_by(Date) %>% 
  summarise(precipt_mm_d = sum(precipt_mm))


p <-ggplot(data=precipt_pivot%>%filter(Date> as.Date("2022-01-01")&Date < as.Date("2023-01-01")), aes(x=Date, y=precipt_mm_d)) +
  geom_bar(stat="identity")
#############


