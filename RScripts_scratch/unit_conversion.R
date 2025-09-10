
# in this r script I convert ppmv to uatm and umol/L
library(here)
library(dplyr)

#need airpress kpa
all_stn <- read.csv(here::here("Whitmore_etal_Biogeochemistry/allstation_data_2.csv"))
all_stn$DateTime_1 <- as.POSIXct(all_stn$DateTime,format="%Y-%m-%d %H:%M",tz="UTC")
all_stn$DateTime <- ifelse(is.na(all_stn$DateTime_1)==TRUE,paste(all_stn$DateTime,"00:00:00",sep=" "),all_stn$DateTime )
all_stn$DateTime <- as.POSIXct(all_stn$DateTime,format="%Y-%m-%d %H:%M",tz="UTC")
all_stn$DateTime_1 <- NULL

BaroData <- read.csv(here::here("data_cleaned/Baro_data_all.csv"), skip=0, header = TRUE)
BaroData <- BaroData[,2:4]
BaroData$DateTime_1 <- as.POSIXct(BaroData$DateTime,format="%Y-%m-%d %H:%M",tz="UTC")
BaroData$DateTime <- ifelse(is.na(BaroData$DateTime_1)==TRUE,paste(BaroData$DateTime,"00:00:00",sep=" "),BaroData$DateTime )
BaroData$DateTime <- as.POSIXct(BaroData$DateTime,format="%Y-%m-%d %H:%M",tz="UTC")
BaroData$DateTime_1 <- NULL

all_stn <- left_join(all_stn,BaroData,by="DateTime")


##convert units

#need airpress kpa

#calc pCO2
kH_STP_mol.l.atm = .035*1/0.986923
D_K = 2400 
T_STP_K = 298.15

#air
all_stn$pCO2_air_ppm <- 418.53 # 2022 average manoa
all_stn$air_pressure_atm <- all_stn$AirPres_kpa / 101.3
all_stn$water_pressure_atm <- all_stn$AirPres_kpa / 101.3 + 0.000967841
all_stn$pCO2_air_atm <-  all_stn$pCO2_air_ppm / 10^6  * all_stn$air_pressure_atm
all_stn$pCO2_air_uatm <-  all_stn$pCO2_air_ppm * all_stn$air_pressure_atm

#stn01
##CO2 in uatm
all_stn$pCO2_w_atm_01 <- all_stn$CO2_ppm_01 /10^6* all_stn$water_pressure_atm 
all_stn$pCO2_w_uatm_01 <- all_stn$CO2_ppm_01 * all_stn$water_pressure_atm 
#henry's constant adjust for temp
all_stn$KH_mol.l.atm <- kH_STP_mol.l.atm * exp(D_K*(1/(all_stn$WaterTemp_c_01 +273.15) - 1/T_STP_K))
all_stn$KH_mol.m3.atm <- all_stn$KH_mol.l.atm * 1000
##CO2 sat and umol/L
all_stn$CO2_sat_mol.L_01 <- all_stn$KH_mol.l.atm * all_stn$pCO2_air_atm
all_stn$CO2_mol.L_01 <- all_stn$KH_mol.l.atm*all_stn$pCO2_w_atm
all_stn$CO2_umol.L_01 <- all_stn$CO2_mol.L_01*10^6
all_stn$CO2_sat_precent_01 <- all_stn$CO2_mol.L_01/all_stn$CO2_sat_mol.L_01*100

#stn02
##CO2 in uatm
all_stn$pCO2_w_atm_02 <- all_stn$CO2_ppm_02 /10^6* all_stn$water_pressure_atm 
all_stn$pCO2_w_uatm_02 <- all_stn$CO2_ppm_02 * all_stn$water_pressure_atm 
#henry's constant adjust for temp
all_stn$KH_mol.l.atm_02 <- kH_STP_mol.l.atm * exp(D_K*(1/(all_stn$WaterTemp_c_02 +273.15) - 1/T_STP_K))
all_stn$KH_mol.m3.atm_02 <- all_stn$KH_mol.l.atm_02 * 1000
##CO2 sat and umol/L
all_stn$CO2_sat_mol.L_02 <- all_stn$KH_mol.l.atm_02 * all_stn$pCO2_air_atm
all_stn$CO2_mol.L_02 <- all_stn$KH_mol.l.atm_02*all_stn$pCO2_w_atm_02
all_stn$CO2_umol.L_02 <- all_stn$CO2_mol.L_02*10^6
all_stn$CO2_sat_precent_02 <- all_stn$CO2_mol.L_02/all_stn$CO2_sat_mol.L_02*100


#stn03
##CO2 in uatm
all_stn$pCO2_w_atm_03 <- all_stn$CO2_ppm_03 /10^6* all_stn$water_pressure_atm 
all_stn$pCO2_w_uatm_03 <- all_stn$CO2_ppm_03 * all_stn$water_pressure_atm 
#henry's constant adjust for temp
all_stn$KH_mol.l.atm_03 <- kH_STP_mol.l.atm * exp(D_K*(1/(all_stn$WaterTemp_c_03 +273.15) - 1/T_STP_K))
all_stn$KH_mol.m3.atm_03 <- all_stn$KH_mol.l.atm_03 * 1000
##CO2 sat and umol/L
all_stn$CO2_sat_mol.L_03 <- all_stn$KH_mol.l.atm_03 * all_stn$pCO2_air_atm
all_stn$CO2_mol.L_03 <- all_stn$KH_mol.l.atm_03*all_stn$pCO2_w_atm_03
all_stn$CO2_umol.L_03 <- all_stn$CO2_mol.L_03*10^6
all_stn$CO2_sat_precent_03 <- all_stn$CO2_mol.L_03/all_stn$CO2_sat_mol.L_03*100



#stn04
##CO2 in uatm
all_stn$pCO2_w_atm_04 <- all_stn$CO2_ppm_04 /10^6* all_stn$water_pressure_atm 
all_stn$pCO2_w_uatm_04 <- all_stn$CO2_ppm_04 * all_stn$water_pressure_atm 
#henry's constant adjust for temp
all_stn$KH_mol.l.atm_04 <- kH_STP_mol.l.atm * exp(D_K*(1/(all_stn$WaterTemp_c_04 +273.15) - 1/T_STP_K))
all_stn$KH_mol.m3.atm_04 <- all_stn$KH_mol.l.atm_04 * 1000
##CO2 sat and umol/L
all_stn$CO2_sat_mol.L_04 <- all_stn$KH_mol.l.atm_04 * all_stn$pCO2_air_atm
all_stn$CO2_mol.L_04 <- all_stn$KH_mol.l.atm_04*all_stn$pCO2_w_atm_04
all_stn$CO2_umol.L_04 <- all_stn$CO2_mol.L_04*10^6
all_stn$CO2_sat_precent_04 <- all_stn$CO2_mol.L_04/all_stn$CO2_sat_mol.L_04*100



all_stn_2 <- all_stn %>%dplyr::select(DateTime,month_number,month,AirPres_kpa,AirTemp_c,pCO2_air_ppm,pCO2_air_uatm,
                                     Q_m3s_01,WaterTemp_c_01,CO2_ppm_01,CO2_umol.L_01,pCO2_w_uatm_01,CO2_sat_precent_01,
                                     Q_m3s_02,WaterTemp_c_02, CO2_ppm_02,CO2_umol.L_02,pCO2_w_uatm_02,CO2_sat_precent_02,
                                     Q_m3s_03,WaterTemp_c_03,CO2_ppm_03,CO2_umol.L_03,pCO2_w_uatm_03,CO2_sat_precent_03,
                                     Q_m3s_04,WaterTemp_c_04,CO2_ppm_04,CO2_umol.L_04,pCO2_w_uatm_04,CO2_sat_precent_04)%>%rename(
                                       pCO2_uatm_01=pCO2_w_uatm_01, pCO2_uatm_02=pCO2_w_uatm_02,
                                       pCO2_uatm_03=pCO2_w_uatm_03, pCO2_uatm_04=pCO2_w_uatm_04
                                     )

#write.csv(all_stn_2,here::here("Whitmore_etal_Biogeochemistry/allstation_data_3.csv"))

