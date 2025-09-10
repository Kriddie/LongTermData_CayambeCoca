#Figure 7
library(here)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(lubridate)

#read in data
stn01_DO_df <- read.csv(here::here("Whitmore_etal_Biogeochemistry/DO_data_stn01.csv"))
stn01_DO_df$Date <- as.Date(stn01_DO_df$Date)
stn01_DO_df <- stn01_DO_df%>%drop_na(CO2_departure)%>%drop_na(DO_departure)

departure_metrics <- read.csv(here::here("Whitmore_etal_Biogeochemistry/departure_metrics_stn01.csv"))
departure_metrics$date <- as.Date(departure_metrics$date)

df_stn1_metab <- read.csv(here::here("Whitmore_etal_Biogeochemistry/metabolim_predictions_stn01.csv"))
df_stn1_metab$date <- as.Date(df_stn1_metab$date)

departure_metrics <- left_join(departure_metrics,df_stn1_metab%>%select(date,GPP_mean,ER_mean,GPP_mean_CO2),by="date")

plot_df <- left_join(departure_metrics ,stn01_DO_df,by=c("date"="Date"))
plot_df$month <- format(plot_df$date,"%m")
plot_df <- plot_df%>%drop_na(month)

plot_df <- plot_df %>%drop_na(month)%>%arrange(desc(month))

### Figure 8a ##
hex <- hue_pal()(7)
p_8a <-ggplot(plot_df) +
  geom_smooth(aes(x = CO2_departure, y = DO_departure,color=as.factor(date)), formula = y ~ x,method='lm')+
  geom_point(aes(x = meanCO2dep, y = meanO2dep), color="black", shape=1) +
  theme_bw(base_size = 14)+
  theme(axis.line=element_blank(), panel.background=element_blank(), panel.border=element_blank(), panel.grid.major=element_blank()) +
  scale_color_manual(labels = NULL,
     values = c("2019-07-13" = hex[5],"2019-07-14" = hex[5],"2019-07-15" = hex[5],"2019-07-23" = hex[5],"2019-07-24" = hex[5],"2019-07-25" = hex[5],"2019-07-27" = hex[5],"2019-07-28" = hex[5],"2019-07-30" = hex[5],"2019-07-31" = hex[5],
                "2019-08-02" = hex[6],"2019-08-03" = hex[6],"2019-08-04" = hex[6],"2019-08-06" = hex[6],"2019-08-07" = hex[6],"2019-08-09" = hex[6],"2019-08-10" = hex[6],"2019-08-11" = hex[6],"2019-08-12" = hex[6],"2019-08-13" = hex[6],"2021-06-12" = hex[6],
                "2021-06-13" = hex[4],"2021-06-14" = hex[4],"2021-06-15" = hex[4],"2021-06-16" = hex[4],"2021-06-17" = hex[4],"2021-06-18" = hex[4],"2021-06-26" = hex[4],"2021-06-27" = hex[4],"2021-06-28" = hex[4],"2021-06-29" = hex[4],"2021-06-30" = hex[4],
                "2021-07-01" = hex[5],"2021-07-02" = hex[5],"2021-07-03" = hex[5],"2021-07-04" = hex[5],"2021-07-05" = hex[5],"2021-07-06" = hex[5],"2021-07-07" = hex[5],"2021-07-08" = hex[5],"2021-07-09" = hex[5],"2021-07-10" = hex[5],"2021-07-11" = hex[5],"2021-07-12" = hex[5],"2021-07-29" = hex[5],"2021-07-30" = hex[5],"2021-07-31" = hex[5],
                "2021-08-01" = hex[6],"2021-08-02" = hex[6],"2021-08-03" = hex[6],"2021-08-05" = hex[6],"2021-08-06" = hex[6],"2021-08-07" = hex[6],"2021-08-08" = hex[6],"2021-08-09" = hex[6],"2021-08-10" = hex[6],"2021-08-11" = hex[6],"2021-08-12" = hex[6],"2021-08-13" = hex[6],"2021-08-14" = hex[6],"2021-08-15" = hex[6],"2021-08-16" = hex[6],"2021-08-17" = hex[6],"2021-08-18" = hex[6],"2021-08-19" = hex[6],"2021-08-20" = hex[6],"2021-08-23" = hex[6],"2021-08-24" = hex[6],"2021-08-25" = hex[6],"2021-08-27" = hex[6],"2021-08-28" = hex[6],"2021-08-29" = hex[6],"2021-08-30" = hex[6],
                "2021-09-02" = hex[7], "2021-09-03" = hex[7],"2021-09-04" = hex[7],"2021-09-05" = hex[7],"2021-09-06" = hex[7],"2021-09-07" = hex[7],"2021-09-08" = hex[7],"2021-09-09" = hex[7],"2021-09-10" = hex[7],"2021-09-11" = hex[7],"2021-09-12" = hex[7],"2021-09-13" = hex[7],"2021-09-14" = hex[7],"2021-09-15" = hex[7],"2021-09-16" = hex[7],"2021-09-17" = hex[7],"2021-09-18" = hex[7],"2021-09-19" = hex[7],"2021-09-20" = hex[7],"2021-09-23" = hex[7],"2021-09-24" = hex[7],"2021-09-25" = hex[7],"2021-09-26" = hex[7],"2021-09-27" = hex[7],
                "2022-03-17" = hex[1],"2022-03-27" = hex[1],
                "2022-04-13" = hex[2],"2022-04-14" = hex[2],"2022-04-15" = hex[2],"2022-04-16" = hex[2],"2022-04-17" = hex[2],"2022-04-19" = hex[2],"2022-04-20" = hex[2],"2022-04-21" = hex[2],"2022-04-22" = hex[2],"2022-04-23" = hex[2],"2022-04-24" = hex[2],"2022-04-25" = hex[2],
                "2022-05-06" = hex[3],"2022-05-07" = hex[3],"2022-05-15" = hex[3],"2022-05-16" = hex[3],"2022-05-17" = hex[3],"2022-05-18" = hex[3],"2022-05-19" = hex[3],"2022-05-20" = hex[3],"2022-05-26" = hex[3],"2022-05-27" = hex[3],"2022-05-28" = hex[3],"2022-05-29" = hex[3],"2022-05-30" = hex[3]
                     )) +
  xlab(expression(CO[2] ~'departure ('~mu*'mol' ~ L^-1~')')) +
  ylab(expression(O[2] ~'departure ('~mu*'mol' ~ L^-1~')'))+
  geom_abline(intercept = 0, slope = -1,color="blue",linetype="dashed",linewidth=1) + 
  annotate('text', x = 60, y = 40, label = expression('RQ = 1.9'~frac(CO[2], O[2])), size = 4, angle='330',color="black") +
  annotate('text', x = 40, y = -75,label = '1:-1', size = 4, angle='300',color="blue") +
  xlim(-10,700) + ylim(-300,10) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  coord_fixed()

plot_df$month_code <- format(as.Date(plot_df$date), "%B")
plot_df$month_code <- reorder(plot_df$month_code, as.numeric( plot_df$month))

#legend code
p_leg <-ggplot(plot_df) +
  geom_line(aes(x = CO2_departure, y = DO_departure,color=month_code),linewidth=2) +
  theme_bw(base_size = 14) +
  # theme(legend.background=element_blank())# +
  labs(color = "month")

leg <- get_legend(p_leg)
p_full <- ggarrange(p_8a+rremove("legend"),leg,widths = c(3,1))


### Figure 8b
departure_metrics$month_code <- format(as.Date(departure_metrics$date), "%B")
departure_metrics$month_code <- reorder(departure_metrics$month_code, as.numeric( departure_metrics$month))

p_8b <- ggplot(departure_metrics,aes(x=DO_mgL_ave,y=offset*sqrt(2))) + 
  geom_point() + 
  geom_smooth(method = lm, formula = y ~ x,color="blue") +
  ylab("Offset") + xlab(expression(paste("DO (mg"~L^-1~")"))) +
  theme_bw(base_size = 14) +
  theme(legend.position = "top") +
  annotate("text", label = paste0("atop('p-value < 0.001','adj. R-sq = 0.72')"),
           x = 5, y = 580, size = 3, parse = TRUE)

### Figure 8c
p_8c <- ggplot(departure_metrics%>%drop_na(GPP_mean_CO2,stretch), aes(x=-GPP_mean_CO2,y=stretch)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x,color="blue") +
  xlab(expression(paste('Average GPP (g C' ,~m^-2~y^-1,")"))) + ylab("Stretch") +
  ylim(0,500) +
  theme_bw(base_size = 14) +
  annotate("text", label = paste0("atop('p-value < 0.001','adj. R-sq = 0.33')"),
           x = -1.5, y = 450, size = 3, parse = TRUE)


### Figure 8 all
p_full1 <- ggarrange(p_8b,p_8c,widths = c(1,1),labels=c("b","c"))

p_full2 <- ggarrange(p_full,p_full1,heights = c(1.5,1),nrow = 2,
                     labels=c("a",""))

