#modeling depth

library(here)
library(dplyr)
library(lubridate)
library(ggplot2)
#depth is a very important parameter in stream metabolizer, but it
#is not easy to estimate.
# we want to estimate "z hat" wich is the mean water depth across the length and width of the reach (m)

#I want to know - what is the length of the reach?
    # metabolism reach length = -ln(1-0.8)*v/K[02]
    # where v is the cross-section averaged river velocity in m d−1
#calculate v
  #dividing daily average discharge by the estimated cross-sectional channel area for that day: 
    #v = Q/A[fm]
    #where Q = discharge
    #where A[Fm]
#calculate Afm - is the field measured channel cross-sectional area
    # A[fm] = m*GH+b
    # where GH is gauge height and
    # where m and b for this equation are model coefficients determined from 
        #a linear regression of the field measured cross-sectional channel area against 
          #measured gage height for the days of the field measurements.



##### Step 1
#calculate A[fm] for station 4

#read in WL station data
df <- read.csv(here::here("data_cleaned/WL_04_cleaned.csv"))
#build rating curve between area and GH
#GH = WL_m

