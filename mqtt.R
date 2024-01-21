library(dplyr)
library(lubridate)
library(scales)
library(feather)
library(ggplot2)
library(data.table)
library(IAPWS95)
#dump data


#df <- feather::read_feather(path = "~/Dokumenty/R/mqtt/mqtt_mpm.feather")
df <- feather::read_feather(path = "~/Dokumenty/R/mqtt/mqtt_mpm.feather")
#df[-c(35357,35377,35376,35378),] -> df

library(tidyr)
df %>% unique() -> df
df$Date <- as.POSIXct(df$Date)
# df$Temperature <- ifelse(df$Temperature == -127, NA, df$Temperature)


df <- df %>% 
  group_by(DeviceRowID) %>% 
  mutate(grouped_id = row_number())



spread(df, key = DeviceRowID, value = Temperature) %>% 
select(-grouped_id)-> df

df <- df[,-c(17)]


colnames(df) <- c("chill", "rh", "pressure", "dew", "setpoint", 
                  "date", "temp_home" , "temp", "temp_rel", "temp_piec",  "temp_co",
                  "bufor_top", "bufor_mid1", "bufor_mid2", "bufor_bottom", "temp2")
df$date <- as.POSIXct(df$date)

df <- df %>% filter(date > "2022-10-14 16:50") %>% filter(date > (Sys.Date()-3))
df$temp=ifelse(df$date < "2022-11-05 13:00" | df$date > "2024-01-06 00:00",df$temp, df$temp2)
# 
# df$WABT <- (df$bufor_top + df$bufor_bottom)/2
# #dt$WABT <- (1*dt$bufor_mid1 )
# 
# df$delta_WABT <- df$WABT-df$temp_co
# #df$delta_WABT <- df$WABT-35
# df$Q_buf <- 600 * 4 * df$delta_WABT / 3412
# 
# 

df %>% group_by(date = floor_date(date, unit="5 mins")) %>%
  summarize(temp=mean(temp,na.rm=T),
            temp_rel=mean(temp_rel,na.rm=T) ,
            temp_home=mean(temp_home,na.rm=T) ,
            temp_piec=mean(temp_piec,na.rm=T) ,
            temp_co=mean(temp_co,na.rm=T) ,
            bufor_top=mean(bufor_top,na.rm=T) ,
            bufor_mid1=mean(bufor_mid1,na.rm=T) ,
            bufor_mid2=mean(bufor_mid2,na.rm=T) ,
            bufor_bottom=mean(bufor_bottom,na.rm=T)) -> dt

##usuwanie / maskowanie outlierów
##domyslny threshold 3.5
# idx <- sapply(dt, class)=="numeric"
# dt[, idx] <- lapply(dt[, idx],FUN = function(x){iglewicz_hoaglin(x,threshold = 5,
#                                                                  return_scores = F)})

#dt$WABT <- (dt$bufor_top + dt$bufor_bottom)/2

dt$WABT <- (((dt$bufor_top + dt$bufor_mid1)/2) + 
  ((dt$bufor_mid1 + dt$bufor_mid2)/2) + 
  ((dt$bufor_mid2 + dt$bufor_bottom)/2) ) / 3

dt$WABT <- ifelse(dt$WABT < 10 , NA, dt$WABT)
dt %>% filter(WABT > 10 ) -> dt



#dt$WABT <- (1*dt$bufor_mid1 )

dt$delta_WABT <- dt$WABT-dt$temp_co
#df$delta_WABT <- df$WABT-35
#dt$Q_buf <- 600 * 4 * dt$delta_WABT / 3412
#
# water enthalpy 
dt$enth <- NA
for (i in c(1:nrow(dt))){
  dt$enth[i] <- hTp(273.15+dt$WABT[i], 0.1)
}

# water enthalpy * 600 kg to kWh (.277/1000)
# minus 24.38 (energia zbiornika w temp 35 deg. C)
# ###IAPWS95::hTp(273.15+35,.1) * 600 *.277/1000
# dt$Q_buf <- (600 * dt$enth * 0.277 / 1000) - 24.38

# water enthalpy * 600 kg to kWh (.277/1000)
# minus 17,43 (energia zbiornika w temp 25 deg. C)
#IAPWS95::hTp(273.15+25,.1) * 600 *.277/1000

dt$Q_buf <- (600 * dt$enth * 0.277 / 1000)  - 17.43



# ##45 W na m2 
# Q <- 100*45
# 
# dt$temp_cwu_delta <- c(NA,diff(dt$temp_cwu))
# dt$Qcwu <- 100 * 4 * dt$temp_cwu_delta / 3412
# # plot(dt$Qcwu)

dt$deltaT = dt$temp_home - dt$temp
dt$temp_co_delta <- c(NA,diff(dt$temp_co))
dt$temp_home_delta <- c(NA,diff(dt$temp_home))
dt$Q_buf_delta <- c(NA,diff(dt$Q_buf))

dt$WABT <- loess(dt$WABT ~ as.numeric(dt$date), span=0.05)$fitted
dt$Q_buf_delta <- c(NA,loess(dt$Q_buf_delta ~ as.numeric(dt$date), span=0.05)$fitted)

# 
p3<-dt %>%  #filter(Q_buf_delta <0) %>%
  ggplot(., aes(x=date)) +
    # geom_point(aes(y=deltaT/6.06/12, color=deltaT/6.06)) +
   geom_point(aes(y=deltaT/4.4,color=temp)) + 
 # geom_point(aes(y=abs(Q_buf_delta), color=Q_buf_delta)) +
  scale_color_viridis_c() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
plotly::ggplotly(p3)



# 
# ##check
##sprawdzene AOV czy delta bufora = deltaT/5.6
##
##
# library(tidyr)
# data_long <- dt %>% filter(Q_buf_delta <0) %>%
#   mutate(q=deltaT/3.3/12, Q_buf_delta = abs(Q_buf_delta)) %>%
#   select(Q_buf_delta,q) %>%
#   gather(condition, measurement, Q_buf_delta:q, factor_key=TRUE)
# 
# aov <- aov(measurement ~ condition, data = data_long)
# 
# summary(aov)
# TukeyHSD(aov)

krzywe<- dt %>% filter(temp_co > 0 & temp > -50 ) %>% 
  ggplot(., aes(x=temp,y=temp_co, color=deltaT/4.4)) + 
  geom_point() + scale_color_viridis_c() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
#plotly::ggplotly(krzywe)
#

plot_temp <-  
  dt %>% filter(temp > -50 & temp_home > -50 & temp_co > 20) %>%
  select(date,temp,temp_co,temp_home, temp_piec) %>% 
  reshape2::melt(id.vars=c("date"))  %>% 
  ggplot(., aes(x=date)) + 
  geom_point(aes(y=value, color=variable), size=1)+
  scale_color_viridis_d() +
  scale_x_datetime(breaks = "4 hours", date_labels = "%F %H:%m") + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1),legend.position = "top") +
  facet_grid(variable~., scales = "free_y")
#plotly::ggplotly(plot_temp)



plot_buffer <-  
  dt %>% filter(bufor_mid2 > 20 & bufor_top > 20 & bufor_mid1 > 20 &
                                bufor_bottom > 20) %>% 
  filter(date > (Sys.Date()-1)) %>%
  select(date,bufor_top,bufor_mid1,bufor_mid2,bufor_bottom, WABT) %>% 
 reshape2::melt(id.vars=c("date"))  %>% 
  ggplot(., aes(x=date)) + 
  geom_point(aes(y=value, color=variable), size=2)+
  scale_color_viridis_d() +
  scale_x_datetime(breaks = "4 hours", date_labels = "%F %H:%m") + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1),legend.position = "top")

 #plotly::ggplotly(plot_buffer, dynamicTicks = TRUE) 
 # %>%
 # rangeslider() %>%
 #   layout(legend = list(orientation = 'h'))



qp <- dt %>% filter(Q_buf > 0 & temp_co > 20) %>%  select(date,Q_buf,deltaT,temp_co, Q_buf_delta) %>% 
  ggplot(., aes(x=date)) +
  #geom_point(aes(y=Q_buf/(deltaT/6.06),color=temp_co), size=2) +
  geom_point(aes(y=Q_buf,color=Q_buf_delta*12), size=2) +
  scale_color_viridis_c() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  # labs(x="data",y="pozostaly czas grzania z bufora [h]")
  labs(x="data",y="Energia bufora [kWh]")
#plotly::ggplotly(qp)
#   
# df[,c("date","bufor_top","bufor_mid1","bufor_mid2","bufor_bottom")] -> buf
# # 
# buf %>% reshape2::melt(id.vars=c("date")) -> buf
# buf$variable <- ifelse(buf$variable == "bufor_top", 100,
#                        ifelse(buf$variable == "bufor_mid1", 66,
#                               ifelse(buf$variable == "bufor_mid2", 33, 0)))
# 
# 
# buf <- na.omit(buf)
# 
# buf$lDate <- with(buf, ymd_hms(paste(date)))
# buf$day  <- day(buf$lDate)
# buf$hour  <- hour(buf$lDate)
# 
# profile <-ggplot(buf,aes((date),factor(variable)))+
#   geom_bar(aes(color=value)) + 
#   scale_colour_gradient(trans = "log")
# profile
# 
# p <- buf %>% 
#   ggplot(.,aes( y = as.factor(variable, x=) date, fill=value)) + 
#   geom_tile() +
#   scale_fill_viridis_c() 




# 
# 
# 
# 
# 
# 
#   ggplot(., aes(x=date)) + 
#   geom_point(aes(y=value, color=variable), size=2)+
#   scale_color_viridis_d() +
#   scale_x_datetime(breaks = "4 hours", date_labels = "%F %H:%m") + 
#   theme(axis.text.x = element_text(angle = 70, hjust = 1),legend.position = "top")

