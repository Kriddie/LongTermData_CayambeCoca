
#bind dataframes


predictions_2019 <- read.csv(here::here("StreamMetabolizer/Predictions/MetabPredictions_stn02_2019_feb08df.csv"))
predictions_2021 <- read.csv(here::here("StreamMetabolizer/Predictions/MetabPredictions_stn02_2021_feb08df.csv"))
predictions_2022 <- read.csv(here::here("StreamMetabolizer/Predictions/MetabPredictions_stn02_2022_feb08df.csv"))

data_inspection_2019 <- read.csv(here::here("StreamMetabolizer/Predictions/data_inspection_stn02_feb08_2019.csv"))
data_inspection_2021 <- read.csv(here::here("StreamMetabolizer/Predictions/data_inspection_stn02_feb08_2021.csv"))
data_inspection_2022 <- read.csv(here::here("StreamMetabolizer/Predictions/data_inspection_stn02_feb08_2022.csv"))

predictions_stn02 <- rbind(predictions_2019,predictions_2021,predictions_2022)
predictions_stn02$date <- as.Date(predictions_stn02$date)

data_inspection_stn02 <- rbind(data_inspection_2019,data_inspection_2021,data_inspection_2022)
data_inspection_stn02$date <- as.Date(data_inspection_stn02$date)

#####
#compare stn01 data
predictions_2019_stn01 <- read.csv(here::here("StreamMetabolizer/Predictions/MetabPredictions_stn01_2019_feb12df.csv"))%>%
  rename(GPP_2019=GPP,ER_2019=ER)
predictions_2019_stn01$date <- as.Date(predictions_2019_stn01$date)
predictions_all_stn01 <- read.csv(here::here("StreamMetabolizer/Predictions/MetabPredictions_stn01_all_feb12df.csv"))%>%
  rename(GPP_all=GPP,ER_all=ER)
predictions_all_stn01$date <- as.Date(predictions_all_stn01$date)

predictions_stn01_compare <- left_join(predictions_2019_stn01,predictions_all_stn01,by="date")
ggplot(predictions_stn01_compare,aes(x=GPP_2019,y=GPP_all)) + geom_point()
ggplot(predictions_stn01_compare,aes(x=ER_2019,y=ER_all)) + geom_point()

data_inspection_2019_stn01 <- read.csv(here::here("StreamMetabolizer/Predictions/data_inspection_stn01_feb12_2019.csv"))
data_inspection_all_stn01 <- read.csv(here::here("StreamMetabolizer/Predictions/data_inspection_stn01_feb12_all.csv"))


predictions_stn02 <- rbind(predictions_2019,predictions_2021,predictions_2022)
predictions_stn02$date <- as.Date(predictions_stn02$date)

data_inspection_stn02 <- rbind(data_inspection_2019,data_inspection_2021,data_inspection_2022)
data_inspection_stn02$date <- as.Date(data_inspection_stn02$date)


ggplot(predictions_stn02,aes(x=date,y=GPP)) + geom_point()
ggplot(predictions_stn02,aes(x=date,y=ER)) + geom_point()
ggplot(predictions_stn02,aes(x=date,y=ER)) + geom_point()


ggplot(data_inspection_stn02,aes(x=date,y=K600.daily)) + geom_point()
ggplot(data_inspection_stn02,aes(x=K600.daily,y=ER.daily)) + geom_point()
ggplot(data_inspection_stn02,aes(x=discharge,y=ER.daily)) + geom_point()
ggplot(data_inspection_stn02,aes(x=discharge,y=K600.daily)) + geom_point()
ggplot(data_inspection_stn02,aes(x=date,y=GPP.daily)) + geom_point()
ggplot(data_inspection_stn02,aes(x=date,y=GPP.daily+ER.daily)) + geom_point()

