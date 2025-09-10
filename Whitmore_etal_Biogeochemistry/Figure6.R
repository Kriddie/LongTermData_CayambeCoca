#Figure 6
library(here)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
##################################

#read in data

#read in station 1 metabolism results
df_stn1_metab <- read.csv(here::here("Whitmore_etal_Biogeochemistry/metabolim_predictions_stn01.csv"))
df_stn1_metab$date <- as.Date(df_stn1_metab$date,format="%Y-%m-%d")
df_stn1_metab <- df_stn1_metab%>%select(date,GPP_mean_CO2,GPP_97.5pct_CO2,ER_mean_CO2,ER_97.5pct_CO2)
df_stn1_metab$Station <- "Stn01"

#read in station 5 metabolism results
df_stn5_metab <- read.csv(here::here("Whitmore_etal_Biogeochemistry/metabolim_predictions_stn05.csv"))
df_stn5_metab$date <- as.Date(df_stn5_metab$date,format="%Y-%m-%d")
df_stn5_metab <- df_stn5_metab%>%select(date,GPP_mean_CO2,GPP_97.5pct_CO2,ER_mean_CO2,ER_97.5pct_CO2)
df_stn5_metab$Station <- "Stn05"

#Make data frame with dates and merge
dates <- data.frame(
  seq(as.Date('2019-07-13'), as.Date('2022-04-03'), by = 'days'))
colnames(dates) <- "date"

df_stn1_metab <- full_join(df_stn1_metab,dates,by="date")
df_stn5_metab <- full_join(df_stn5_metab,dates,by="date")

df_metab <- rbind(df_stn5_metab,df_stn1_metab)


#Figure 7a
p_1.2 <-ggplot(df_stn1_metab%>%
                 filter(date < as.Date("2020-01-10")))  +
  geom_ribbon(aes(x=date,ymin=-GPP_mean_CO2-GPP_97.5pct_CO2, ymax=-GPP_mean_CO2+GPP_97.5pct_CO2),fill = "darkgreen",alpha=.3) +
  geom_line(aes(x=date,y=-GPP_mean_CO2),color="darkgreen") +
  geom_point(aes(x=date,y=-GPP_mean_CO2),color="darkgreen") +
  geom_ribbon(aes(x=date,ymin=-ER_mean_CO2-ER_97.5pct_CO2, ymax=-ER_mean_CO2+ER_97.5pct_CO2),fill = "darkorange",alpha=.3) +
  geom_line(aes(x=date,y=-ER_mean_CO2),color="darkorange") +
  geom_point(aes(x=date,y=-ER_mean_CO2),color="darkorange") +
  ylim(-5,15) +
  geom_hline(yintercept = 0) +
  ggtitle("Station 1: 2019 - 2020") +
  theme_classic(base_size = 14)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  theme(plot.title = element_text(margin = margin(t = 10, b = -10),hjust = 0.5,size = 12))

#Figure 7b
p_1.1 <-ggplot(df_stn1_metab %>%
                 filter(date > as.Date("2021-07-01")&date < as.Date("2021-10-01")))  +
  geom_ribbon(aes(x=date,ymin=-GPP_mean_CO2-GPP_97.5pct_CO2, ymax=-GPP_mean_CO2+GPP_97.5pct_CO2),fill = "darkgreen",alpha=.3) +
  geom_line(aes(x=date,y=-GPP_mean_CO2),color="darkgreen") +
  geom_point(aes(x=date,y=-GPP_mean_CO2),color="darkgreen") +
  geom_ribbon(aes(x=date,ymin=-ER_mean_CO2-ER_97.5pct_CO2, ymax=-ER_mean_CO2+ER_97.5pct_CO2),fill = "darkorange",alpha=.3) +
  geom_line(aes(x=date,y=-ER_mean_CO2),color="darkorange") +
  geom_point(aes(x=date,y=-ER_mean_CO2),color="darkorange") +
  ylim(-5,15) +
  geom_hline(yintercept = 0) +
  ggtitle("Station 1: 2022") +
  theme_classic(base_size = 14) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  theme(plot.title = element_text(margin = margin(t = 10, b = -10),hjust = 0.5,size = 12))




p_2 <- ggplot(df_stn5_metab%>%
                filter(date > as.Date("2021-06-15")& date < as.Date("2021-10-20")))  +
  geom_ribbon(aes(x=date,ymin=-GPP_mean_CO2-GPP_97.5pct_CO2, ymax=GPP_mean_CO2+GPP_97.5pct_CO2),fill = "darkgreen",alpha=.3) +
  ylim(-5,15) +
  geom_line(aes(x=date,y=-GPP_mean_CO2),color="darkgreen") +
  geom_point(aes(x=date,y=-GPP_mean_CO2),color="darkgreen") +
  geom_ribbon(aes(x=date,ymin=-ER_mean_CO2-ER_97.5pct_CO2, ymax=-ER_mean_CO2+ER_97.5pct_CO2),fill = "darkorange",alpha=.3) +
  geom_line(aes(x=date,y=-ER_mean_CO2),color="darkorange") +
  geom_point(aes(x=date,y=-ER_mean_CO2),color="darkorange") +
  geom_hline(yintercept = 0) +
  ggtitle("Station 5: 2022") +
  theme_classic(base_size = 14)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  theme(plot.title = element_text(margin = margin(t = 10, b = -10),hjust = 0.5,size = 12))

#legend plot
df <- data.frame(x = 1:10, y = 1:10)
leg_plot <- ggplot(df, aes(x = x, y = y)) +
  geom_blank() + annotate("segment", x = 0, xend = 4, y = 5, yend = 5, colour = "darkgreen") +
  annotate("rect", xmin = 0, xmax = 4, ymin = 5-.5, ymax = 5+.5, fill="darkgreen", alpha = .3) +
  
  annotate("segment", x = 0, xend = 4, y = 8, yend = 8, colour = "darkorange") +
  annotate("rect", xmin = 0, xmax =4, ymin = 8-.5, ymax = 8+.5, fill="darkorange", alpha = .3) + 
  
  annotate("text", x = c(8,8), y = c(8,5), label = c(expression(paste('ER (gC' ,~m^-2~y^-1,")")), expression(paste('GPP (gC' ,~m^-2~y^-1,")"))),size = 3) +
  theme_void()

metab_full <- plot_grid(p_1.2,p_1.1,p_2,labels =c("a","b","c"),nrow=3,ncol=1)
legend <- plot_grid(leg_plot,NULL,NULL,NULL,nrow=2,ncol=2)

##figure 7 c

#DOC
DOC_2021 <- read.csv(here::here("Whitmore_etal_Biogeochemistry/DOC_df.csv"))%>%
  select(Date,Station,DOC,TDN)%>%
  filter(Station=="Stn01"|Station=="Stn05")%>%
  rename(date=Date)
DOC_2021$date <- as.Date(DOC_2021$date,format="%m/%d/%y")
DOC_2021 <- DOC_2021%>%filter(date<"2022-01-01")

df_full <- full_join(df_metab,DOC_2021,by=c("date","Station"))


p1 <- ggplot(df_full%>%drop_na(Station) ,aes(x=Station,y=DOC,fill=Station)) +geom_boxplot()+   ylab(expression(paste('DOC'))) + xlab("") +
  scale_x_discrete(labels= c("stn 1","stn 5")) +
  theme_bw(base_size = 12)

p2 <- ggplot(df_full%>%drop_na(Station) ,aes(x=Station,y=TDN,fill=Station)) +geom_boxplot()+   ylab(expression(paste('TDN'))) + xlab("") +
  scale_x_discrete(labels= c("stn 1","stn 5")) +
  theme_bw(base_size = 12)

p3 <- ggplot(df_full%>%drop_na(Station) ,aes(x=Station,y=-GPP_mean_CO2,fill=Station)) +geom_boxplot()+  ylab(expression(paste('GPP'))) + xlab("") +
  scale_x_discrete(labels= c("stn 1","stn 5")) +
  theme_bw(base_size = 12)

p4 <- ggplot(df_full%>%drop_na(Station) ,aes(x=Station,y=-ER_mean_CO2,fill=Station)) +geom_boxplot() +
  ylab(expression(paste('ER'))) + xlab("") +
  scale_x_discrete(labels= c("stn 1","stn 5")) +
  theme_bw(base_size = 12)


p_full <- plot_grid(p3 + rremove("legend"),
                    p4 + rremove("legend"),
                    p1 + rremove("legend"),
                    p2 + rremove("legend"),
                    ncol=2,nrow=2)

full_p1 <- plot_grid(NULL,legend,p_full,labels=c("","","d"),rel_heights = c(.5,2,4),nrow=3)

#Figure 7 with legend
full_p2 <- plot_grid(metab_full,full_p1,rel_widths = c(1,1.5), ncol=2)

