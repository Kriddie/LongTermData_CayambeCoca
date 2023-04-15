#play with data to make horton grant figure

library(lubridate)
library(dplyr)
library(ggplot2)
library(here)
#plot

Stn01 <- read.csv(here::here("All_Station_Data/Stn01_2023-03-29.csv"))
Stn01$DateTime <- as.POSIXct(Stn01$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

#convert to % saturation
#https://www.waterontheweb.org/under/waterquality/oxygen.html
Stn01$DO_cstp <- exp(7.7117-1.31403*log(Stn01$DOTemp_c+45.93))
Stn01$P_wv <- 11.8571-(3840.7/(Stn01$DOTemp_c+273.15))-(216.961/(Stn01$DOTemp_c+273.15)^2)
Stn01$theta <- .000975 - (1.426 * 10^-5 * Stn01$DOTemp_c) - (6.436*10^-8* Stn01$DOTemp_c )
Stn01$Cp <- Stn01$DO_cstp*(Stn01$AirPres_kpa*0.00986923)*
  ((1-Stn01$P_wv )*(1-Stn01$theta*(Stn01$AirPres_kpa*0.00986923))/(1-Stn01$P_wv)/(1-Stn01$theta))

Stn01$DO_sat <- (100*Stn01$DO_mgL/Stn01$Cp)

#convert CO2 to  % saturation

#set constants
kH_STP_mol.L.atm = .035
D_K = 2400 
T_STP_K = 298.15
#calculate henry's law constant using 
Stn01$KH_mol.L.atm <- kH_STP_mol.L.atm * exp(D_K*(1/(Stn01$DOTemp_c + 273.15) - 1/T_STP_K))
UatmToatm <- 10^6
#calculate mass equivalence of CO2 in water
Stn01$CO2_water_umolperL <- (Stn01$adjusted_ppm/UatmToatm) * Stn01$KH_mol.L.atm 


Stn01$CO2_air_uatm <- 450 * Stn01$AirPres_kpa*(0.00986923/10^6)

#Stn01$kH <- exp(-58.0931+(90.5069*(100/(Stn01$DOTemp_c+273.15)))+(22.294*(log((Stn01$DOTemp_c+273.15)/100))))
Stn01$CO2_air_umolperL <- Stn01$CO2_air_uatm * Stn01$KH_mol.L.atm
Stn01$CO2_water_umolperL <- Stn01$adjusted_ppm /  10^6 * Stn01$KH_mol.L.atm
Stn01$CO2_sat <- Stn01$CO2_water_umolperL/Stn01$CO2_air_umolperL *100


###

Stn01$Date <- as.Date(Stn01$DateTime)

Stn01_pivot <- Stn01 %>% 
  group_by(Date) %>% 
  summarise(ppm_mean = mean(ppm),
            CO2_sat_mean = mean(CO2_sat),
            WL_m_mean = mean(WL_m),
            Q_Ls_mean = mean(Q_Ls),
            DO_mgL_mean = mean(DO_mgL),
            DO_sat_mean = mean(DO_sat),
            WLTemp_c_mean = mean(WLTemp_c),
            DO_mgL_std = sd(DO_mgL),)

Stn01_pivot$Month <- as.numeric(format(as.Date(Stn01_pivot$Date),"%m"))

Stn01_pivot$ppm_mean[Stn01_pivot$ppm_mean > 10000] <- 10000
Stn01_pivot$ppm_mean[Stn01_pivot$ppm_mean < 20] <- NA


#wet season == June. July, August


Stn01_pivot_wet <- Stn01_pivot%>%filter(Month >= 6 & Month <= 7)
#Stn01_pivot_wet <- Stn01_pivot_wet$DO_mgL_mean
Stn01_pivot_wet$season <- "Wet Season"
Stn01_pivot_dry <- Stn01_pivot%>%filter(Month <= 6 | Month > 7)
Stn01_pivot_dry$season <- "Dry season"

#Stn01_pivot_dry <- Stn01_pivot%>%filter(Month >= 11)
#Stn01_pivot_dry$season <- "Dry months: December-Jan"

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
  geom_vline(xintercept = 2, linetype="dotted", color = "red", size=1.5) +
  theme(text = element_text(size = 20)) +
  xlab("Dissolved oxygen [mg/L]") 

#plot 1B
ggplot(Stn01_pivot_season, aes(x=DO_mgL_std, color=season,fill=season)) +
  geom_histogram( alpha=0.5, position="identity", bins = 40) + 
  theme_classic()  +
  theme(text = element_text(size = 20)) +
  xlab("Dissolved oxygen [mg/L]")

##########
ggplot(Stn01_pivot_season, aes(x=ppm_mean, color=season,fill=season)) +
  geom_histogram( alpha=0.5, position="identity", bins = 10) + 
  theme_classic() 

#Picture of sensor set up, the scatter plot over the histogram,

##switch DO and Co2 to %sat

#coming from anoxia


#plot 2A
ggplot(Stn01_pivot_season%>%filter(DO_sat_mean<100) , aes(x=DO_sat_mean, y=CO2_sat_mean, fill=season)) +
  geom_point(size=4, shape=21, color="black") +
  theme_classic() +
  theme(text = element_text(size = 20)) +
 # x = expression("% Saturation DO") +
#  ylab("% Saturation CO2") +
  labs(y=expression("% sat "*CO[2]), x = expression("% sat DO")) +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  theme(legend.title= element_blank())


ggplot(Stn01_pivot_season , aes(x=DO_mgL_mean, y=ppm_mean, fill=season)) +
  geom_point(size=4, shape=21, color="black") +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  xlab("Dissolved oxygen [mg/L]") + ylab("CO2 [ppm]") +
  # Right -> inside the plot area
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  theme(legend.title= element_blank())

ggplot(Stn01_pivot_season, aes(x=log(DO_mgL_mean), y=Q_Ls_mean, fill=season)) +
  geom_point(size=4, shape=21, color="black") +
  theme_classic() +
  theme(text = element_text(size = 20)) 

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


