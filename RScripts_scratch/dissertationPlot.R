library(ggbreak)
library(patchwork) 
df <- read.csv(here::here("Dissertation_graph.csv"))

#rectangle
rect_df <- data.frame(Year=2025, 
                      Pg.C.yr.1=4)
print(super_sleepers)

p2007 <- ggplot(df,aes(x=as.integer(Year),y=Pg.C.yr.1)) + geom_point(color="grey90") +
  theme_classic(base_size = 18) + scale_x_continuous(breaks=seq(2007, 2026, 2),limits = c(2007,2017)) +
  scale_y_continuous(limits=c(0,5),breaks=c(0,1,2,3,4))+
  ylab(expression("Pg C"~yr^-1)) + xlab("Year")


p2025 <- ggplot(rect_df) +geom_point(aes(x=Year,y=Pg.C.yr.1)) +
  geom_rect(aes(xmin = 2024.5, xmax = 2025.5, ymin = 0, ymax = 5),fill="grey80") +
  scale_x_continuous(breaks=2025) +
  theme_classic(base_size = 18) + xlab("") + 
  theme(axis.line.y=element_blank(),
    axis.text.y=element_blank(),axis.ticks.y=element_blank(),
    axis.title.y=element_blank())

all <- plot_grid(p2007,p2025,nrow=1,ncol=2,
                 rel_widths = c(11,1),align="h"
                 )

