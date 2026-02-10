
  # Hydrology
  # Jan 2026 KPeck
  
  # Using station 08EE012 Simpson Creek as surrogate for TBC
  # showing comparison
  
library(tidyverse)
library(tidyhydat)
library(readxl)


# check what is available from station:

hy_stations(station_number = "08EE012")

simp.flows.real.raw <- realtime_dd(station_number = "08EE012")
range(simp.flows.real.raw$Date)
# usually a very short time period

simp.flows.hist.raw <- hy_daily_flows(station_number = "08EE012",
                                      start_date= "2024-01-01")
range(simp.flows.hist.raw$Date)

simp.lvl.hist.raw <- hy_daily_levels(station_number = "08EE012",
                                     start_date= "2024-01-01")
range(simp.lvl.hist.raw$Date)

# if neither of these have the date range we are seeking...
# go to website and download csv of discharge unit values as csv: 
# https://wateroffice.ec.gc.ca/report/real_time_e.html?stn=08EE012 
# simp.flows.custom <- read_csv("08EE012_QR_20260119T1840.csv",skip = 10,
#                                 col_types = c("c","?","?","?","?"),
#                                 col_names = c("Date","Parameter","Value","Approval","Qualifier")) %>% 
#   mutate(Date = ymd(substr(Date,1,10)), Parameter = "Flow") 
# 
# simp.flows.custom.daily <- simp.flows.custom %>% 
#   group_by(Date, Parameter) %>% 
#   summarize(ave.flow = mean(Value, na.rm=T)) %>% #ave daily flow 
#   select(Date,Parameter, Value=ave.flow) %>% 
#   mutate(Year = year(Date), fyear = as.factor(Year),
#          julian = yday(Date),STATION_NUMBER = "08EE012")

# get smolt daily values


 # smolt24 <- read_excel("toboggan_smolt_dataentry_FINAL 2024.xlsx", 
 #                       sheet="individualfish") %>% 
 #   filter(species != "co-f") %>% 
 #   # mutate(fate = ifelse(!is.na(sacrifice),"sacrifice",
 #   #                      ifelse(!is.na(mort),"mort","live"))) %>% 
 #   # mutate(tag.status = ifelse(!is.na(a_adclip) & fate %in% "live","A",
 #   #                            ifelse(!is.na(r_adclip),"R",NA))) %>% 
 #   # mutate(isoweek = isoweek(date), yday = yday(date),
 #   #        week.yday = as.numeric(paste0(isoweek,".",yday))) %>%  #this week starts on a monday, which matches our mark switch day
 #   group_by(date) %>% 
 #   summarize(total.fish = sum(count, na.rm=T))

smolt24 <- read_excel("toboggan_smolt_dataentry_FINAL 2024.xlsx",
                      sheet="individualfish") %>%
  filter(species != "co-f") %>%
  # mutate(fate = ifelse(!is.na(sacrifice),"sacrifice",
  #                      ifelse(!is.na(mort),"mort","live"))) %>%
  # mutate(tag.status = ifelse(!is.na(a_adclip) & fate %in% "live","A",
  #                            ifelse(!is.na(r_adclip),"R",NA))) %>%
  # mutate(isoweek = isoweek(date), yday = yday(date),
  #        week.yday = as.numeric(paste0(isoweek,".",yday))) %>%  #this week starts on a monday, which matches our mark switch day
  group_by(date) %>%
  summarize(total.fish = sum(count, na.rm = TRUE)) %>% # 5 lines added by AK
  ungroup() %>%
  mutate(
    date = as.Date(date)
  )


plot.daily.co.smolt <- ggplot()+
  geom_bar(data = smolt24,aes(x=date, y=total.fish), 
           stat = "identity", fill="gray50")+
  geom_line(data = simp.flows.hist.raw, aes(x=Date, y= Value*1000), col="blue")+
  labs(x="Date",y="Total Daily Fish")+
  scale_x_date(limits = c(ymd("2024-05-01"),ymd("2024-06-30")), 
               date_breaks = "1 week", date_labels = "%b-%d")+
  scale_y_continuous(sec.axis = sec_axis(~ . /1000, name = "Daily Discharge (cms)")) +
  theme_bw()+
  theme(axis.text.x = element_text(hjust=1, angle=45),
        axis.title.y = element_text(color = "gray50"),
        axis.title.y.right = element_text(color = "blue"),
        axis.text.y.right = element_text(color = "blue"))
plot.daily.co.smolt

 # ggsave(plot=plot.daily.co.smolt, filename = "plot.daily.discharge.smolt.24.png",
 #        width=6, height=4)

#compare env readings at traps to env canada station:

env24 <- read_excel("toboggan_smolt_dataentry_FINAL 2024.xlsx", 
                    sheet="effort") %>% 
  mutate(date = as_date(date), 
         date_time = ymd_hms(paste0(date,"-",
                                    substr(arrival_time_24h,12,19))))

env24daily <- env24 %>% 
  group_by(date, site) %>% 
  summarize(daily.level = mean(water_level_m, na.rm=T),
            ave.temp.water = mean(water_temp_c, na.rm=T))

ggplot(env24daily[env24daily$site%in%"fence",])+
  geom_point(aes(x=date,y=daily.level+.4, col=site))+
  geom_line(data=simp.lvl.hist.raw, aes(x=Date, y=Value))+
  scale_x_date(limits = c(ymd("2024-05-01"),ymd("2024-06-30")), 
               date_breaks = "1 week", date_labels = "%b-%d")+
  theme_bw()

# plot.water.temp24 <- ggplot(env24)+
#   geom_line(aes(x=date_time,y=water_temp_c, col=site, linetype=type))+
#   geom_point(aes(x=date_time,y=water_temp_c, col=site, shape=type))+
#   scale_x_date(limits = c(ymd("2024-05-01"),ymd("2024-06-30")), 
#                date_breaks = "1 week", date_labels = "%b-%d")+
#   theme_bw()+
#   theme(axis.text.x = element_text(hjust=1, angle=45))+
#   labs(x="Date", y=expression("Water Temperature ("^o*"C)"), linetype="",
#        shape="", col="Site")
# plot.water.temp24

plot.water.temp24 <- ggplot(env24)+
  geom_line(aes(x=date_time,y=water_temp_c, col=site, linetype=type))+
  geom_point(aes(x=date_time,y=water_temp_c, col=site, shape=type))+
  scale_x_datetime(                   ##3 lines changed by AK
    limits = as.POSIXct(c("2024-05-01", "2024-06-30")),
    date_breaks = "1 week", date_labels = "%b-%d")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust=1, angle=45))+
  labs(x="Date", y=expression("Water Temperature ("^o*"C)"), linetype="",
       shape="", col="Site")
plot.water.temp24

 # ggsave(plot = plot.water.temp24, filename = "plot.water.temp24.png",
 #        width = 6,height=4)

summary(lm(ave.temp.water~date+site, data=env24daily))

