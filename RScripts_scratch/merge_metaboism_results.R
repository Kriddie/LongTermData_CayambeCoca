
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


ggplot(predictions_stn02,aes(x=date,y=GPP)) + geom_point()
ggplot(predictions_stn02,aes(x=date,y=ER)) + geom_point()
ggplot(predictions_stn02,aes(x=date,y=ER)) + geom_point()


ggplot(data_inspection_stn02,aes(x=date,y=K600.daily)) + geom_point()
ggplot(data_inspection_stn02,aes(x=K600.daily,y=ER.daily)) + geom_point()
ggplot(data_inspection_stn02,aes(x=discharge,y=ER.daily)) + geom_point()
ggplot(data_inspection_stn02,aes(x=discharge,y=K600.daily)) + geom_point()
ggplot(data_inspection_stn02,aes(x=date,y=GPP.daily)) + geom_point()
ggplot(data_inspection_stn02,aes(x=date,y=GPP.daily+ER.daily)) + geom_point()

