
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)


#rating curves with KW 

# Serial: 72020436	Station 1
# Serial: 72020421	Station 3



rating_curve <- read.csv(here::here("Analysis/Discharge/Discharge_KWCalc.csv"), 
                         header=T, na.strings=c("","NA"))
rating_curve$Level_m_compensated <-  as.numeric( as.character (rating_curve$Level_m_compensated))
rating_curve$lvl_m_stn1 <-  as.numeric( as.character (rating_curve$lvl_m_stn1))
rating_curve$lvl_m_stn3 <-  as.numeric( as.character (rating_curve$lvl_m_stn3))
rating_curve$Date <- as.POSIXct(rating_curve$Date, format='%m/%d/%Y')
rating_curve$Station <- as.factor(as.numeric(as.factor(rating_curve$Station)))

rating_curve_NO5 <- rating_curve %>%
  filter(Station != 2) 

plot(rating_curve$Station, rating_curve$Q_m3.s_Hach)


#all stations

mdl1_all <- lm(rating_curve$Q_m3.s_Hach ~ rating_curve$lvl_m_stn3)
mdl2_all <- lm(rating_curve$Q_m3.s_Hach ~ rating_curve$lvl_m_stn3 + I(rating_curve$lvl_m_stn3^2))
mdl3_all <- lm(rating_curve$Q_m3.s_Hach ~ rating_curve$lvl_m_stn3 + I(rating_curve$lvl_m_stn3^2) + I(rating_curve$lvl_m_stn3^3))
mdl4_all <- lm(rating_curve$Q_m3.s_Hach ~ I(rating_curve$lvl_m_stn3^2))

set.seed(20)
q <- seq(from=0, to=.5, by=0.005)
y_mdl1 <- mdl1_all$coefficients[1] + mdl1_all$coefficients[2]*q
y_mdl2 <- mdl2_all$coefficients[1] + mdl2_all$coefficients[2]*q + mdl2_all$coefficients[3]*q^2
y_mdl3 <- mdl3_all$coefficients[1] + mdl3_all$coefficients[2]*q + mdl3_all$coefficients[3]*q^2 + mdl3_all$coefficients[4]*q^3
y_mdl4 <- mdl4_all$coefficients[1] + mdl4_all$coefficients[2]*q^2

summary(mdl1_all)
summary(mdl2_all)
summary(mdl3_all)
summary(mdl4_all)


plot(rating_curve$Q_m3.s_Hach ~ rating_curve$lvl_m_stn3, col=rating_curve$Station,
     main="All Stations Discharge Rating Curve 03-26-2020",
     xlab="Level-logger stn 3 (meters)", ylab="Discharge (cubic meters per second)")

#legend(x = 'bottomright', 
#       legend = rating_curve$Station,
#       col = rating_curve$Station, pch = par("pch"), bty = 'n', xjust = 1)


#################

scatterplot(Q_m3.s_Hach ~ lvl_m_stn3 | Station, data = rating_curve, 
            smoother = FALSE, grid = FALSE, frame = FALSE)



#lines(q,y_mdl1,col='firebrick1',lwd=1)
lines(q,y_mdl2,col='blue',lwd=1)
#lines(q,y_mdl3,col='yellow',lwd=1)
#lines(q,y_mdl4,col='green',lwd=1)
mtext("y=  0.01887  - 0.25497*x + 0.97299*x^2 ; adjR-squared .9076", side=3)



#station 1
stn1 <- rating_curve %>%
  filter(Station == "1") #%>%
#  filter(Date != "2019-07-24") %>%
#  filter(Date != "2019-08-13")
mdl1_1 <- lm(stn1$Q_m3.s_Hach ~ stn1$lvl_m_stn3)
mdl2_1 <- lm(stn1$Q_m3.s_Hach ~ stn1$lvl_m_stn3 + I(stn1$lvl_m_stn3^2))
mdl3_1 <- lm(stn1$Q_m3.s_Hach ~ stn1$lvl_m_stn3 + I(stn1$lvl_m_stn3^2) + I(stn1$lvl_m_stn3^3))
mdl4_1 <- lm(stn1$Q_m3.s_Hach ~ I(stn1$lvl_m_stn3^2))

set.seed(20)
q <- seq(from=0, to=.5, by=0.005)
y_mdl1 <- mdl1_1$coefficients[1] + mdl1_1$coefficients[2]*q
y_mdl2 <- mdl2_1$coefficients[1] + mdl2_1$coefficients[2]*q + mdl2_1$coefficients[3]*q^2
y_mdl3 <- mdl3_1$coefficients[1] + mdl3_1$coefficients[2]*q + mdl3_1$coefficients[3]*q^2 + mdl3_1$coefficients[4]*q^3
y_mdl4 <- mdl4_1$coefficients[1] + mdl4_1$coefficients[2]*q^2

summary(mdl1_1)
summary(mdl2_1)
summary(mdl3_1)
summary(mdl4_1)

plot(stn1$Q_m3.s_Hach ~ stn1$lvl_m_stn3,
     main="Stn 1 Discharge Rating Curve 03-26-2020",
     xlab="Level-logger stn 3 (meters)", ylab="Discharge (cubic meters per second)")
#lines(q,y_mdl1,col='firebrick1',lwd=1)
lines(q,y_mdl2,col='blue',lwd=1)
#lines(q,y_mdl3,col='yellow',lwd=1)
#lines(q,y_mdl4,col='green',lwd=1)
mtext("y=  0.05323  - 0.61431*x + 1.89259*x^2 ; adjR-squared .9909", side=3)

#station 2
stn2 <- rating_curve %>%
  filter(Station == "2") #%>%
#  filter(Date != "2019-07-24") %>%
#  filter(Date != "2019-08-13")
mdl1_2 <- lm(stn2$Q_m3.s_Hach ~ stn2$lvl_m_stn3)
mdl2_2 <- lm(stn2$Q_m3.s_Hach ~ stn2$lvl_m_stn3 + I(stn2$lvl_m_stn3^2))
mdl3_2 <- lm(stn2$Q_m3.s_Hach ~ stn2$lvl_m_stn3 + I(stn2$lvl_m_stn3^2) + I(stn2$lvl_m_stn3^3))
mdl4_2 <- lm(stn2$Q_m3.s_Hach ~ I(stn2$lvl_m_stn3^2))

set.seed(20)
q <- seq(from=0, to=.5, by=0.005)
y_mdl1 <- mdl1_2$coefficients[1] + mdl1_2$coefficients[2]*q
y_mdl2 <- mdl2_2$coefficients[1] + mdl2_2$coefficients[2]*q + mdl2_2$coefficients[3]*q^2
y_mdl3 <- mdl3_2$coefficients[1] + mdl3_2$coefficients[2]*q + mdl3_2$coefficients[3]*q^2 + mdl3_2$coefficients[4]*q^3
y_mdl4 <- mdl4_2$coefficients[1] + mdl4_2$coefficients[2]*q^2


plot(stn2$Q_m3.s_Hach ~ stn2$Level_m_compensated)
#lines(q,y_mdl1,col='firebrick1',lwd=1)
lines(q,y_mdl2,col='blue',lwd=1)
#lines(q,y_mdl3,col='yellow',lwd=1)
#lines(q,y_mdl4,col='green',lwd=1)
mtext("y=  0.8823  - 10.3227*x + 32.2560*x^2 ; adjR-squared .9898", side=3)


#station 3
stn3 <- rating_curve %>%
  filter(Station == "3")

mdl1_3 <- lm(stn3$Q_m3.s_Hach ~ stn3$lvl_m_stn3)
mdl2_3 <- lm(stn3$Q_m3.s_Hach ~ stn3$lvl_m_stn3 + I(stn3$lvl_m_stn3^2))
mdl3_3 <- lm(stn3$Q_m3.s_Hach ~ stn3$lvl_m_stn3 + I(stn3$lvl_m_stn3^2) + I(stn3$lvl_m_stn3^3))
mdl4_3 <- lm(stn3$Q_m3.s_Hach ~ I(stn3$lvl_m_stn3^2))

summary(mdl1_3)
summary(mdl2_3)
summary(mdl3_3)
summary(mdl4_3)

set.seed(20)
q <- seq(from=0, to=.5, by=0.005)
#y_mdl1 <- mdl1_3$coefficients[1] + mdl1_3$coefficients[2]*q
y_mdl2 <- mdl2_3$coefficients[1] + mdl2_3$coefficients[2]*q + mdl2_3$coefficients[3]*q^2
#y_mdl3 <- mdl3_3$coefficients[1] + mdl3_3$coefficients[2]*q + mdl3_3$coefficients[3]*q^2 + mdl3_3$coefficients[4]*q^3
#y_mdl4 <- mdl4_3$coefficients[1] + mdl4_3$coefficients[2]*q^2


plot(stn3$Q_m3.s_Hach ~ stn3$Level_m_compensated,
     main="Stn 3 Discharge Rating Curve 03-26-2020",
     xlab="Level-logger stn 3 (meters)", ylab="Discharge (cubic meters per second)")
#lines(q,y_mdl1,col='firebrick1',lwd=1)
lines(q,y_mdl2,col='blue',lwd=1)
#lines(q,y_mdl3,col='yellow',lwd=1)
#lines(q,y_mdl4,col='green',lwd=1)
mtext("y=  0.01959  - 0.25846*x + 0.97362*x^2 ; adjR-squared 0.8756", side=3)

#station 4
stn4 <- rating_curve %>%
  filter(Station == "4")

mdl1_4 <- lm(stn4$Q_m3.s_Hach ~ stn4$Level_m_compensated)
mdl2_4 <- lm(stn4$Q_m3.s_Hach ~ stn4$Level_m_compensated + I(stn4$Level_m_compensated^2))
mdl3_4 <- lm(stn4$Q_m3.s_Hach ~ stn4$Level_m_compensated + I(stn4$Level_m_compensated^2) + I(stn4$Level_m_compensated^3))
mdl4_4 <- lm(stn4$Q_m3.s_Hach ~ I(stn4$Level_m_compensated^2))

summary(mdl1_4)
summary(mdl2_4)
summary(mdl3_4)
summary(mdl4_4)


set.seed(20)
q <- seq(from=0, to=.5, by=0.005)
y_mdl1 <- mdl1_4$coefficients[1] + mdl1_4$coefficients[2]*q
y_mdl2 <- mdl2_4$coefficients[1] + mdl2_4$coefficients[2]*q + mdl2_4$coefficients[3]*q^2
y_mdl3 <- mdl3_4$coefficients[1] + mdl3_4$coefficients[2]*q + mdl3_4$coefficients[3]*q^2 + mdl3_4$coefficients[4]*q^3
y_mdl4 <- mdl4_4$coefficients[1] + mdl4_4$coefficients[2]*q^2



plot(stn4$Q_m3.s_Hach ~ stn4$Level_m_compensated,
     main="Stn 4 Discharge Rating Curve 03-26-2020",
     xlab="Level-logger (meters)", ylab="Discharge (cubic meters per second)")
#lines(q,y_mdl1,col='firebrick1',lwd=1)
lines(q,y_mdl2,col='blue',lwd=1)
#lines(q,y_mdl3,col='yellow',lwd=1)
#lines(q,y_mdl4,col='green',lwd=1)
mtext("y=  0.001564  - 0.081946*x + 0.563126*x^2 ; adjR-squared 0.9358", side=3)

