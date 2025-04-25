library(streamMetabolizer)
library(unitted)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(cowplot)

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

ave_calc <- Stn01%>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500)%>%drop_na(CO2_departure)%>%drop_na(DO_departure)
p <- ggplot() + 
  geom_point(data=Stn01%>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500)#%>%filter(DateTime>as.POSIXct("2021-05-20 00:00:00"))
             ,
             aes(x=CO2_departure,y=DO_departure),size=2,color="grey70") + 
  geom_point(data=df_all%>%filter(month_name=="July 2021"|month_name=="Sept. 2021"),aes(x=CO2_departure,y=DO_departure,color=month_name),size=1) + 
  geom_abline(intercept = as.numeric(model$coefficients[1]), slope = as.numeric(model$coefficients[2]),color="black",linetype="dashed",linewidth=1) + 
  geom_abline(intercept = 0, slope = -1,color="blue",linetype="dashed",linewidth=1) + 
  annotate('text', x = 60, y = 40, 
           label = expression('RQ = 1.9'~frac(CO[2], O[2])), size = 4, angle='330',color="black") +
  annotate('text', x = 40, y = -75, 
           label = '1:-1', size = 4, angle='300',color="blue") +
  xlab(expression(CO[2] ~'departure ('~mu*'mol' ~ L^-1~')')) +
  ylab(expression(O[2] ~'departure ('~mu*'mol' ~ L^-1~')')) +
  scale_color_manual(name="Month collected",labels=c("July 2021", "Sept. 2021"), values=c("#5e3c99", "#e66101")) +
  xlim(-110,900) + ylim(-250, 100) +
#  xlim(-110,900) + ylim(-900, 75) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = mean(ave_calc$CO2_departure), y = mean(ave_calc$DO_departure), 
                   xend = (mean(ave_calc$CO2_departure)-mean(ave_calc$DO_departure))/2, yend = -(mean(ave_calc$CO2_departure)-mean(ave_calc$DO_departure))/2),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"), linewidth=1,linetype="solid") +
  geom_segment(aes(xend = mean(ave_calc$CO2_departure), yend = mean(ave_calc$DO_departure), 
                   x = (mean(ave_calc$CO2_departure)-mean(ave_calc$DO_departure))/2, y = -(mean(ave_calc$CO2_departure)-mean(ave_calc$DO_departure))/2),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"), linewidth=1,linetype="solid") +
  theme_bw(base_size = 20) +
  theme(axis.line=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        ) +
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal',
        legend.title = element_text(size = 14), 
                legend.text = element_text(size = 14))


###

#time series of RQ or regress with O2 or temp
Stn01_week <- Stn01 %>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500)#%>%filter(DateTime>as.POSIXct("2021-05-20 00:00:00"))
Stn01_week$Date <- as.Date(Stn01_week$DateTime)
Stn01_week$Year <- lubridate::year(Stn01_week$DateTime)
Stn01_week$Month <- lubridate::month(Stn01_week$DateTime)


Stn01_week$week_no <- lubridate::week(Stn01_week$DateTime)
Stn01_week$week_no_continuous <- ifelse(Stn01_week$Year==2019, Stn01_week$week_no,ifelse(Stn01_week$Year==2021,Stn01_week$week_no+52,Stn01_week$week_no+52*2))
Stn01_week <- unique(Stn01_week)

#now buid datadrame
new_df <- Stn01_week%>%select(week_no_continuous)
new_df <- unique(new_df)
new_df$RQ <- NA

for(i in 1:nrow(new_df)){
  week_select <- new_df$week_no_continuous[i]
  Stn01_week_select <- Stn01_week%>%filter(week_no_continuous==week_select)%>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500)
  model_2 <- lm(CO2_umol.L ~ DO_umolperL, data = Stn01_week_select)
  new_df$RQ[i] <- model_2$coefficients[2]
}

#bind in data
new_df_bind <- Stn01_week%>%select(Year,Month,week_no_continuous)
new_df_bind <- unique(new_df_bind)
new_df <- full_join(new_df,new_df_bind,by="week_no_continuous")

ggplot(new_df,aes(x=week_no_continuous,y=RQ,color=Year)) + geom_point()
ggplot(new_df,aes(x=Month,y=RQ,color=Year)) + geom_point()

#######
#now buid datadrame
#######
new_df <- Stn01_week%>%select(Date)
new_df <- unique(new_df)
new_df$RQ <- NA
new_df$offset <- NA

for(i in 1:nrow(new_df)){
  Date_select <- new_df$Date[i]
  Stn01_Date_select <- Stn01_week%>%filter(Date==Date_select)%>%filter(DO_mgL>0,CO2_ppm_adjusted < 18500)
  model_2 <- lm(CO2_umol.L ~ DO_umolperL, data = Stn01_Date_select)
  new_df$RQ[i] <- model_2$coefficients[2]
  
  #centroid calc
  df_date_centroid <- Stn01_Date_select%>%summarise(
    Centroid_co2 = mean(CO2_departure,na.rm=TRUE),
    Centroid_o2 = mean(DO_departure,na.rm=TRUE))
  #calc offset
  new_df$offset[i] <- df_date_centroid$Centroid_co2 + df_date_centroid$Centroid_o2 
  
}

#bind in data
new_df_bind <- Stn01_week %>%group_by(Year,Month,Date)%>%
  summarise(Q_ave = mean(Q_m3s,na.rm = T),
            DO_ave = mean(DO_mgL,na.rm = T),
            WLTemp_ave = mean(WLTemp_c,na.rm=T))
  
new_df_bind <- unique(new_df_bind)
new_df <- full_join(new_df,new_df_bind,by="Date")

ggplot(new_df%>%filter(Date<"2021-01-01")%>%filter(RQ>-20)%>%filter(Q_ave<.02),
       aes(x=Date,y=offset,color=Q_ave)) + geom_point()
ggplot(new_df%>%filter(Date>"2021-01-01"&Date<"2022-01-01")%>%filter(RQ>-20)%>%filter(Q_ave<.02),
       aes(x=Date,y=offset,color=Q_ave)) + geom_point()
ggplot(new_df%>%filter(Date>"2022-01-01")%>%filter(RQ>-20)%>%filter(Q_ave<.02),
       aes(x=Date,y=offset,color=Q_ave)) + geom_point()

ggplot(new_df%>%filter(RQ>-20)%>%filter(Q_ave<.02),aes(x=DO_ave,y=offset,color=Q_ave)) + geom_point()

ggplot(new_df%>%filter(RQ>-20)%>%filter(Q_ave<.02),aes(x=WLTemp_ave,y=RQ,color=Q_ave)) + geom_point()

ggplot(new_df%>%filter(RQ>-20)%>%filter(Q_ave<.02),aes(x=Date,y=offset,color=Q_ave)) + geom_point()



#keep this one
ggplot(new_df%>%filter(RQ>-20)%>%filter(Q_ave<.04),aes(x=Q_ave,y=offset,color=Q_ave)) + 
  geom_point() + scale_y_log10()

ggplot(new_df%>%filter(RQ>-20)%>%filter(Q_ave<.04),aes(x=Q_ave,y=RQ,color=Q_ave)) + 
  geom_point()# + scale_y_log10()


#keep this one
ggplot(new_df%>%filter(RQ>-20)%>%filter(Q_ave<.04),aes(x=DO_ave,y=offset#,color=Q_ave*1000
                                                       )
                                                       ) + 
  geom_point(size=3) + scale_y_continuous(transform = "log",breaks=c(100,150,250,400,600,900)) + scale_color_continuous(name=expression('Discharge'~'(L'~s^-1~')')) +
  xlab(expression(O[2] ~'('~mu*'mol' ~ L^-1~')')) + ylab("Daily offset") +
  theme_bw(base_size = 18) 


###

p2 <- ggplot(new_df%>%filter(RQ>-20)%>%filter(Q_ave<.04),aes(x=DO_ave,y=offset*sqrt(2)/2,color=Q_ave*1000
                                                       )) + 
  geom_point(size=3) + scale_y_continuous(transform = "log",breaks=c(50,100,150,250,400,600,900)) + 
  scale_color_continuous(name=expression('Discharge'~'(L'~s^-1~')')) +
  xlab(expression('Average DO ('~mu*'mol' ~ L^-1~')')) + ylab(expression('Daily offset ('~mu*'mol' ~ L^-1~')')) +
  theme_bw(base_size = 12) 



###full plot
full_p1 <- plot_grid(p2,NULL,rel_heights = c(1,.8), nrow=2)
full_p2 <- plot_grid(p,full_p1,rel_widths = c(1,.8), labels="auto",nrow=1)

##FULL PLOT 2

p3 <- ggdraw(p  #+ theme_half_open(12)
       ) +
  draw_plot(p2, .55, .55, .45, .45) +
  draw_plot_label(
    c("a", "b"),
    c(0, 0.53),
    c(1, 1),
    size = 20)

