library(dplyr)
library(lubridate)
library(scales)
library(feather)
library(ggplot2)
library(data.table)
#dump data

#df <- feather::read_feather(path = "~/Dokumenty/R/mqtt/mqtt_mpm.feather")
df <- feather::read_feather(path = "~/Dokumenty/R/mqtt/chrust_plus/mqtt_mpm.feather")
#df[-c(35357,35377,35376,35378),] -> df

library(tidyr)
df %>% unique() -> df
df$Date <- as.POSIXct(df$Date)


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

df <- df %>% filter(date > "2022-10-14 16:50") %>% filter(date > (Sys.Date()-2))
df$temp=ifelse(df$date < "2022-11-05 13:00", df$temp, df$temp2)

df$WABT <- (df$bufor_top + df$bufor_bottom)/2
#dt$WABT <- (1*dt$bufor_mid1 )

df$delta_WABT <- df$WABT-df$temp_co
#df$delta_WABT <- df$WABT-35
df$Q_buf <- 600 * 4 * df$delta_WABT / 3412

df %>% group_by(date = floor_date(date, unit="5 mins")) %>%
  summarize(temp=mean(temp,na.rm=T),
            temp_rel=mean(temp_rel,na.rm=T) ,
            temp_home=mean(temp_home,na.rm=T) ,
            temp_piec=mean(temp_piec,na.rm=T) ,
            temp_co=mean(temp_co,na.rm=T) ,
            bufor_top=mean(bufor_top,na.rm=T) ,
            bufor_mid1=mean(bufor_mid1,na.rm=T) ,
            bufor_mid2=mean(bufor_mid2,na.rm=T) ,
            bufor_bottom=mean(bufor_bottom,na.rm=T),
            Q_buf=mean(Q_buf,na.rm=T)) -> dt



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

# 
# p3<-dt %>%  filter(Q_buf_delta <0) %>%
#   ggplot(., aes(x=date)) +
#     geom_point(aes(y=deltaT/7.6/12, color=deltaT/7.6)) +
#   geom_point(aes(y=abs(Q_buf_delta), color=Q_buf_delta)) +
#   scale_color_viridis_c() +
#   theme(axis.text.x = element_text(angle = 70, hjust = 1))
# plotly::ggplotly(p3)
# 
# ##check
# ##sprawdzene AOV czy delta bufora = deltaT/5.6
# ##
# ##
# library(tidyr)
# data_long <- dt %>% filter(Q_buf_delta <0) %>%
#   mutate(q=deltaT/7.6/12, Q_buf_delta = abs(Q_buf_delta)) %>%
#   select(Q_buf_delta,q) %>%
#   gather(condition, measurement, Q_buf_delta:q, factor_key=TRUE)
# 
# aov <- aov(measurement ~ condition, data = data_long)
# 
# summary(aov)
# TukeyHSD(aov)

krzywe<- dt %>% filter(temp_co > 0 & temp > -50 ) %>% 
  ggplot(., aes(x=temp,y=temp_co, color=deltaT/7.6)) + 
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
plotly::ggplotly(plot_temp)



plot_buffer <-  
  dt %>% filter(bufor_mid2 > 20 & bufor_top > 20 & bufor_mid1 > 20 &
                                bufor_bottom > 20) %>% 
  filter(date > (Sys.Date()-1)) %>%
  select(date,bufor_top,bufor_mid1,bufor_mid2,bufor_bottom) %>% 
 reshape2::melt(id.vars=c("date"))  %>% 
  ggplot(., aes(x=date)) + 
  geom_point(aes(y=value, color=variable), size=2)+
  scale_color_viridis_d() +
  scale_x_datetime(breaks = "4 hours", date_labels = "%F %H:%m") + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1),legend.position = "top")
# ggplotly(plot_buffer, dynamicTicks = TRUE) %>%
#   rangeslider() %>%
#   layout(legend = list(orientation = 'h'))



qp <- dt %>% filter(Q_buf > 0 & temp_co > 20) %>%  select(date,Q_buf,deltaT,temp_co) %>% 
  ggplot(., aes(x=date)) +
  geom_point(aes(y=Q_buf/(deltaT/7.6),color=temp_co), size=2) +
  geom_point(aes(y=Q_buf,color=deltaT/7.6), size=2) +
  scale_color_viridis_c() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  labs(x="data",y="pozostaly czas grzania z bufora [h]")
#plotly::ggplotly(qp)





dt %>% group_by(date = floor_date(date, unit="60 mins")) %>%
  summarize(temp=mean(temp,na.rm=T),
            temp_rel=mean(temp_rel,na.rm=T) ,
            temp_home=mean(temp_home,na.rm=T) ,
            temp_piec=mean(temp_piec,na.rm=T) ,
            temp_co=mean(temp_co,na.rm=T) ,
            bufor_top=mean(bufor_top,na.rm=T) ,
            bufor_mid1=mean(bufor_mid1,na.rm=T) ,
            bufor_mid2=mean(bufor_mid2,na.rm=T) ,
            bufor_bottom=mean(bufor_bottom,na.rm=T),
            Q_buf=mean(bufor_bottom,na.rm=T)) -> dth

dth$WABT <- (dth$bufor_top + dth$bufor_bottom)/2
#dt$WABT <- (1*dt$bufor_mid1 )

dth$delta_WABT <- dth$WABT-dth$temp_co
dth$Q_buf <- 600 * 4 * dth$delta_WABT / 3412
dth$Q_buf_delta <- c(NA,diff(dth$Q_buf))

dth$tryb <- ifelse(dth$Q_buf_delta>0,"load","discharge")
within(dth, cycle_id <- rleid(dth$tryb) ) -> dth

dth %>% group_by(cycle_id) %>% 
  mutate(WABT = case_when(tryb == "load" ~ max(WABT),
                          tryb == "discharge" ~ min(WABT))) -> dth

p <- dth %>% filter(tryb != "NA") %>% 
  ggplot(.,aes( y = tryb, x= date, fill=Q_buf)) + 
  geom_tile() +
  scale_fill_viridis_c() 
##plotly::ggplotly(p)
