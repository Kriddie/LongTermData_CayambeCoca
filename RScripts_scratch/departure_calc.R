library(streamMetabolizer)
library(unitted)
WL_02 <- read.csv(here::here("data_cleaned/WL_02_cleaned.csv"))
WL_02$DateTime <- as.POSIXct(WL_02$DateTime,format="%Y-%m-%d %H:%M",tz="UTC")

DO_df <- read.csv(here::here("data_cleaned/DO_01_cleaned.csv"))
DO_df$DateTime <- as.POSIXct(DO_df$DateTime,format="%m/%d/%Y %H:%M",tz="UTC")
DO_df$DOTemp_c <- ifelse(DO_df$DOTemp_c > 30, (DO_df$DOTemp_c-23)/9*5,DO_df$DOTemp_c)

CO2_df <- read.csv(here::here("data_cleaned/CO2_01_max10000_cleaned.csv")) 
CO2_df$DateTime <- as.POSIXct(CO2_df$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")

#convert to umol per l
kH_STP_mol.l.atm = .035*1/0.986923
D_K = 2400 
T_STP_K = 298.15
CO2_df$pCO2_air_ppm <- 418.53 # 2022 average manoa
CO2_df$air_pressure_atm <- CO2_df$Total_hPa / 1013.25 - 0.000967841
CO2_df$pCO2_air_atm <-  CO2_df$pCO2_air_ppm / 10^6  * CO2_df$air_pressure_atm
CO2_df$pCO2_w_atm <- CO2_df$CO2_ppm_adjusted / 10^6 
#henry's constant adjust for temp
CO2_df$KH_mol.l.atm <- kH_STP_mol.l.atm * exp(D_K*(1/(CO2_df$WLTemp_01_c+273.15) - 1/T_STP_K))
CO2_df$KH_mol.m3.atm <- CO2_df$KH_mol.l.atm * 1000
CO2_df$CO2_sat_mol.L <- CO2_df$KH_mol.l.atm*CO2_df$pCO2_air_atm
CO2_df$CO2_sat_umol.L <- CO2_df$CO2_sat_mol.L*10^6
CO2_df$CO2_mol.L <- CO2_df$KH_mol.l.atm*CO2_df$pCO2_w_atm
CO2_df$CO2_umol.L <- CO2_df$CO2_mol.L*10^6

CO2_df <- CO2_df%>%select(DateTime,CO2_ppm_adjusted,pCO2_air_ppm,air_pressure_atm,CO2_umol.L,CO2_sat_umol.L)

#df_stn1_metab <- read.csv(here::here("StreamMetabolizer/Predictions_bestones/station1_calclight_march9.csv"))%>%drop_na(GPP_mean)
#df_stn1_metab$date <- as.Date(df_stn1_metab$date)

Stn01 <- full_join(WL_02,DO_df, by=c("DateTime"))
Stn01 <- full_join(Stn01,CO2_df, by=c("DateTime"))



Stn01$DO_sat <- calc_DO_sat(temp=u(Stn01$WLTemp_c,"degC"), press=u(Stn01$AirPres_kpa*10,"mb"), sal=u(0,"PSU")) # units are checked

#convert to umol
Stn01$DO_sat_umolperL <- as.numeric(Stn01$DO_sat)/1000 / 32 * 10^6
Stn01$DO_umolperL <- Stn01$DO_mgL/1000 / 32 * 10^6
Stn01$DO_departure <- Stn01$DO_sat_umolperL - Stn01$DO_umolperL

#convert to umol
Stn01$DO_sat_umolperL <- as.numeric(Stn01$DO_sat)/1000 / 32 * 10^6
Stn01$DO_umolperL <- Stn01$DO_mgL/1000 / 32 * 10^6
Stn01$DO_departure <- Stn01$DO_umolperL - Stn01$DO_sat_umolperL 

Stn01$CO2_departure <-  Stn01$CO2_umol.L - Stn01$CO2_sat_umol.L 


ggplot(Stn01 %>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500)
       ,aes(x=CO2_departure,y=DO_departure,color=DateTime)) + geom_point()

start_time <- as.POSIXct("2019-07-01 00:00:00",tz="UTC")
end_time <- as.POSIXct("2019-09-01 00:00:00",tz="UTC")

ggplot(Stn01 %>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500)%>%
         filter(DateTime > start_time & DateTime < end_time)
       ,aes(x=CO2_departure,y=DO_departure,color=DateTime)) + geom_point()




####

start_time <- as.POSIXct("2019-07-01 00:00:00",tz="UTC")
end_time <- as.POSIXct("2019-08-01 00:00:00",tz="UTC")
July_2019 <- Stn01 %>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500)%>%
  filter(DateTime > start_time & DateTime < end_time)
July_2019$month_name <- "July_2019"

start_time <- as.POSIXct("2019-08-01 00:00:00",tz="UTC")
end_time <- as.POSIXct("2019-09-01 00:00:00",tz="UTC")
aug_2019 <- Stn01 %>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500)%>%
  filter(DateTime > start_time & DateTime < end_time)
aug_2019$month_name <- "aug_2019"

start_time <- as.POSIXct("2021-06-01 00:00:00",tz="UTC")
end_time <- as.POSIXct("2021-07-01 00:00:00",tz="UTC")
june_2021 <- Stn01 %>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500)%>%
  filter(DateTime > start_time & DateTime < end_time)
june_2021$month_name <- "june_2021"

start_time <- as.POSIXct("2021-07-01 00:00:00",tz="UTC")
end_time <- as.POSIXct("2021-08-01 00:00:00",tz="UTC")
july_2021 <- Stn01 %>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500)%>%
  filter(DateTime > start_time & DateTime < end_time)
july_2021$month_name <- "July 2021"

start_time <- as.POSIXct("2021-08-01 00:00:00",tz="UTC")
end_time <- as.POSIXct("2021-09-01 00:00:00",tz="UTC")
aug_2021 <- Stn01 %>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500)%>%
  filter(DateTime > start_time & DateTime < end_time)
aug_2021$month_name <- "aug_2021"

start_time <- as.POSIXct("2021-09-01 00:00:00",tz="UTC")
end_time <- as.POSIXct("2021-10-01 00:00:00",tz="UTC")
sept_2021 <- Stn01 %>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500)%>%
  filter(DateTime > start_time & DateTime < end_time)
sept_2021$month_name <- "Sept. 2021"

start_time <- as.POSIXct("2021-10-01 00:00:00",tz="UTC")
end_time <- as.POSIXct("2021-11-01 00:00:00",tz="UTC")
oct_2021 <- Stn01 %>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500)%>%
  filter(DateTime > start_time & DateTime < end_time)
oct_2021$month_name <- "oct_2021"

start_time <- as.POSIXct("2022-04-01 00:00:00",tz="UTC")
end_time <- as.POSIXct("2022-05-01 00:00:00",tz="UTC")
april_2022 <- Stn01 %>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500)%>%
  filter(DateTime > start_time & DateTime < end_time)
april_2022$month_name <- "april_2022"

df_all <- rbind(July_2019,aug_2019,june_2021,july_2021,aug_2021,sept_2021,oct_2021,april_2022)

#centroid calc
df_all_centroid <- df_all%>%group_by(month_name)%>%summarise(
  Centroid_co2 = mean(CO2_departure,na.rm=TRUE),
  Centroid_o2 = mean(DO_departure,na.rm=TRUE)
)
#calc offset
df_all_centroid$offset <- df_all_centroid$Centroid_co2 + df_all_centroid$Centroid_o2 

library(magrittr)
library(ConfidenceEllipse)

data(glass, package = "ConfidenceEllipse")

ellipse_99 <- confidence_ellipse(glass, x = SiO2, y = Al2O3, conf_level = 0.99)

#calc slope
library(broom)
df_all_slope <- df_all %>% 
  group_by(month_name) %>% 
  nest %>% 
  mutate(modelout = map(data, ~ lm(DO_departure ~ CO2_departure, data = .x) %>%
                          tidy %>% 
                          filter(term == "CO2_departure") %>% 
                          select(slope = estimate))) %>%
  select(-data) %>%
  unnest(cols = c(modelout))

df_all_summ <- full_join(df_all_centroid, df_all_slope, by="month_name")

#all slope
model <-  lm(DO_departure~ CO2_departure, data = Stn01%>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500))
as.numeric(model$coefficients[2])
model_2 <-  lm(CO2_umol.L ~ DO_umolperL, data = Stn01 %>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500))
model_2$coefficients[2]

p <- ggplot() + 
  geom_point(data=Stn01%>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500),aes(x=CO2_departure,y=DO_departure),size=2,color="grey70") + 
  geom_point(data=df_all%>%filter(month_name=="July 2021"|month_name=="Sept. 2021"),aes(x=CO2_departure,y=DO_departure,color=month_name),size=1) + 
    geom_abline(intercept = as.numeric(model$coefficients[1]), slope = as.numeric(model$coefficients[2]),color="black",linetype="dashed",linewidth=1) + 
  geom_abline(intercept = 0, slope = -1,color="blue",linetype="dashed",linewidth=1) + 
    annotate('text', x = 55, y = 25, 
           label = expression('RQ = 1.9'~frac(CO[2], O[2])), size = 3, angle='318',color="black") +
  annotate('text', x = 50, y = -75, 
           label = '1:-1', size = 3, angle='300',color="blue") +
  xlab(expression(CO[2] ~'departure ('~mu*'mol' ~ L^-1~')')) +
  ylab(expression(O[2] ~'departure ('~mu*'mol' ~ L^-1~')')) +
  scale_color_manual(name="Month collected",labels=c("July 2021", "Sept. 2021"), values=c("#5e3c99", "#e66101")) +
  xlim(-10,900) + ylim(-250, 100) +
  theme_bw(base_size = 16)


