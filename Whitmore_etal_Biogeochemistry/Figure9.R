#figure 9
library(here)
library(dplyr)
library(ggplot2)
library(ggpubr)

CH4_df_summary <- read.csv(here::here("Whitmore_etal_Biogeochemistry/methane_df.csv"))

DOC_df <- read.csv(here::here("Whitmore_etal_Biogeochemistry/DOC_df.csv"))
DOC_df$Date <- as.Date(DOC_df$Date, format="%m/%d/%y")

DOC_df$Date_plot <- as.character(format(DOC_df$Date,format="%Y %B %d"))
#without colmillo data
DOC_spatial <- ggplot(DOC_df %>%filter(SampleType=="Station")%>%filter(WS=="Gavilan")%>%filter(Date<"2022-01-01"))+
  geom_point(aes(x=Distance_from_peatland, y=DOC, fill=Date_plot), shape=21,size=3) + 
  geom_line(aes(x=Distance_from_peatland, y=DOC,color=Date_plot)) + 
  xlab("Distance from peatland outlet") +
  ylab(expression(paste('DOC (mg ' , l^-1,")"))) +
  labs(fill="Date",color="Date") +
  theme_bw(base_size = 14) +
  theme(legend.position = "top",
        legend.title = element_blank())  +
  guides(color=guide_legend(nrow=3,byrow=TRUE),
         fill=guide_legend(nrow=3,byrow=TRUE)) 

##longitudinal

CH4_df_summary$Date_format <- as.Date(CH4_df_summary$DateTime)
CH4_df_summary$Date_plot <- as.character(format(CH4_df_summary$Date_format,format="%Y %B %d"))
CH4_df_summary$Date_plot <- reorder(CH4_df_summary$Date_plot, CH4_df_summary$Date_format)


ch4_spatial <- ggplot(CH4_df_summary %>%filter(Site=="Outlet")) +
  geom_point(aes(x=as.numeric(gsub('m','',Site2)),y=pCH4_ppm, fill=Date_plot), size=3,shape=21) +
  geom_line(aes(x=as.numeric(gsub('m','',Site2)), y=pCH4_ppm,color=Date_plot)) +
  #  scale_y_continuous(transform = "log",breaks=c(2000,1000,500,200)) +
  #  scale_fill_manual(labels = c("June-06","July-01","July-06"),values=c("#e41a1c","#377eb8","#4daf4a")) +
  ylab(expression(italic(p)*CH[4] ~'(ppm)')) + xlab("Distance from outlet (m)")  + labs(fill="Date collected",color="Date collected") +
  theme_bw(base_size = 14)+
  theme(legend.position = "top",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=3,byrow=TRUE),
         fill=guide_legend(nrow=3,byrow=TRUE))

spatial_full_plot <- ggarrange(DOC_spatial,ch4_spatial#,common.legend = TRUE
)

