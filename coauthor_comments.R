#notes on high v low flow

#October to February (low)
low_flow_df <- allstation_data_3%>%filter(month_number<=2|month_number>=10)

median(low_flow_df$Q_m3s_02,na.rm = TRUE)
mean(low_flow_df$Q_m3s_02,na.rm = TRUE)

#June to August (high)
high_flow_df <- allstation_data_3%>%filter(month_number>2&month_number<10)

median(high_flow_df$Q_m3s_02,na.rm = TRUE)
mean(high_flow_df$Q_m3s_02,na.rm = TRUE)

