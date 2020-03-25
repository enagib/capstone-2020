options(scipen = 999)

library(readr)
library(dplyr)
library(lubridate)
library(janitor)
library(knitr)
library(ggplot2)

flights_2015 <- read_csv("/Users/Eman Nagib/Documents/capstone-local/flights_2015_w_holidays.csv")
flights_2016 <- read_csv("/Users/Eman Nagib/Documents/capstone-local/flights_2016_w_holidays.csv")
flights_2017 <- read_csv("/Users/Eman Nagib/Documents/capstone-local/flights_2017_w_holidays.csv")
flights_2018 <- read_csv("/Users/Eman Nagib/Documents/capstone-local/flights_2018_w_holidays.csv")
flights_2019 <- read_csv("/Users/Eman Nagib/Documents/capstone-local/flights_2019_w_holidays.csv")


hol_2015 <- flights_2015 %>% mutate(id = row_number()) %>% select(id, ARR_DELAY, DEP_DELAY, "presidents_day":"winter_holiday")
hol_2016 <- flights_2016 %>% mutate(id = row_number()) %>% select(id, ARR_DELAY, DEP_DELAY, "presidents_day":"winter_holiday")
hol_2017 <- flights_2017 %>% mutate(id = row_number()) %>% select(id, ARR_DELAY, DEP_DELAY, "presidents_day":"winter_holiday")
hol_2018 <- flights_2018 %>% mutate(id = row_number()) %>% select(id, ARR_DELAY, DEP_DELAY, "presidents_day":"winter_holiday")
hol_2019 <- flights_2019 %>% mutate(id = row_number()) %>% select(id, ARR_DELAY, DEP_DELAY, "presidents_day":"winter_holiday")



########2015
  presidents_day_del_2015 <- hol_2015 %>% filter(presidents_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  presidents_day_arr_del_2015 <- presidents_day_del_2015$tot_arr_delay
  presidents_day_dep_del_2015 <- presidents_day_del_2015$tot_dep_delay 
  
  easter_del_2015 <- hol_2015 %>% filter(easter == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  easter_arr_del_2015 <- easter_del_2015$tot_arr_delay
  easter_dep_del_2015 <- easter_del_2015$tot_dep_delay 
  
  memorial_day_del_2015 <- hol_2015 %>% filter(memorial_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  memorial_day_arr_del_2015 <- memorial_day_del_2015$tot_arr_delay
  memorial_dep_del_2015 <- memorial_day_del_2015$tot_dep_delay 
  
  independence_day_del_2015 <- hol_2015 %>% filter(independence_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  independence_day_arr_del_2015 <- independence_day_del_2015$tot_arr_delay
  independence_day_dep_del_2015 <- independence_day_del_2015$tot_dep_delay 
  
  labor_day_del_2015 <- hol_2015 %>% filter(labor_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  labor_day_arr_del_2015 <- labor_day_del_2015$tot_arr_delay
  labor_day_dep_del_2015<- labor_day_del_2015$tot_dep_delay 
  
  thanksgiving_del_2015 <- hol_2015 %>% filter(thanksgiving == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  thanksgiving_arr_del_2015 <- thanksgiving_del_2015$tot_arr_delay
  thanksgiving_dep_del_2015 <- thanksgiving_del_2015$tot_dep_delay 
  
  winter_holiday_del_2015 <- hol_2015 %>% filter(winter_holiday == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  winter_holiday_arr_del_2015 <- winter_holiday_del_2015$tot_arr_delay
  winter_holiday_dep_del_2015 <- winter_holiday_del_2015$tot_dep_delay 

  
  
########2016
  presidents_day_del_2016 <- hol_2016 %>% filter(presidents_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  presidents_day_arr_del_2016 <- presidents_day_del_2016$tot_arr_delay
  presidents_day_dep_del_2016 <- presidents_day_del_2016$tot_dep_delay 
  
  easter_del_2016 <- hol_2016 %>% filter(easter == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  easter_arr_del_2016 <- easter_del_2016$tot_arr_delay
  easter_dep_del_2016 <- easter_del_2016$tot_dep_delay 
  
  memorial_day_del_2016 <- hol_2016 %>% filter(memorial_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  memorial_day_arr_del_2016 <- memorial_day_del_2016$tot_arr_delay
  memorial_day_dep_del_2016 <- memorial_day_del_2016$tot_dep_delay 
  
  independence_day_del_2016 <- hol_2016 %>% filter(independence_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  independence_day_arr_del_2016 <- independence_day_del_2016$tot_arr_delay
  independence_day_dep_del_2016 <- independence_day_del_2016$tot_dep_delay
  
  labor_day_del_2016 <- hol_2016 %>% filter(labor_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  labor_day_arr_del_2016 <- labor_day_del_2016$tot_arr_delay
  labor_day_dep_del_2016<- labor_day_del_2016$tot_dep_delay 
  
  thanksgiving_del_2016 <- hol_2016 %>% filter(thanksgiving == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  thanksgiving_arr_del_2016 <- thanksgiving_del_2016$tot_arr_delay
  thanksgiving_dep_del_2016 <- thanksgiving_del_2016$tot_dep_delay
  
  winter_holiday_del_2016 <- hol_2016 %>% filter(winter_holiday == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  winter_holiday_arr_del_2016 <- winter_holiday_del_2016$tot_arr_delay
  winter_holiday_dep_del_2016 <- winter_holiday_del_2016$tot_dep_delay 
  
  
########2017
  presidents_day_del_2017 <- hol_2017 %>% filter(presidents_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  presidents_day_arr_del_2017 <- presidents_day_del_2017$tot_arr_delay
  presidents_day_dep_del_2017 <- presidents_day_del_2017$tot_dep_delay 
  
  easter_del_2017 <- hol_2017 %>% filter(easter == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  easter_arr_del_2017 <- easter_del_2017$tot_arr_delay
  easter_dep_del_2017 <- easter_del_2017$tot_dep_delay 
  
  memorial_day_del_2017 <- hol_2017 %>% filter(memorial_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  memorial_day_arr_del_2017 <- memorial_day_del_2017$tot_arr_delay
  memorial_day_dep_del_2017 <- memorial_day_del_2017$tot_dep_delay 
  
  independence_day_del_2017 <- hol_2017 %>% filter(independence_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  independence_day_arr_del_2017 <- independence_day_del_2017$tot_arr_delay
  independence_day_dep_del_2017 <- independence_day_del_2017$tot_dep_delay
  
  labor_day_del_2017 <- hol_2017 %>% filter(labor_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  labor_day_arr_del_2017 <- labor_day_del_2017$tot_arr_delay
  labor_day_dep_del_2017 <- labor_day_del_2017$tot_dep_delay 
  
  thanksgiving_del_2017 <- hol_2017 %>% filter(thanksgiving == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  thanksgiving_arr_del_2017 <- thanksgiving_del_2017$tot_arr_delay
  thanksgiving_dep_del_2017 <- thanksgiving_del_2017$tot_dep_delay
  
  winter_holiday_del_2017 <- hol_2017 %>% filter(winter_holiday == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  winter_holiday_arr_del_2017 <- winter_holiday_del_2017$tot_arr_delay
  winter_holiday_dep_del_2017 <- winter_holiday_del_2017$tot_dep_delay 

  
########2018
  presidents_day_del_2018 <- hol_2018 %>% filter(presidents_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  presidents_day_arr_del_2018 <- presidents_day_del_2018$tot_arr_delay
  presidents_day_dep_del_2018 <- presidents_day_del_2018$tot_dep_delay
  
  easter_del_2018 <- hol_2018 %>% filter(easter == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  easter_arr_del_2018 <- easter_del_2018$tot_arr_delay
  easter_dep_del_2018 <- easter_del_2018$tot_dep_delay 
  
  memorial_day_del_2018 <- hol_2018 %>% filter(memorial_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  memorial_day_arr_del_2018 <- memorial_day_del_2018$tot_arr_delay
  memorial_day_dep_del_2018 <- memorial_day_del_2018$tot_dep_delay 
  
  independence_day_del_2018 <- hol_2018 %>% filter(independence_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  independence_day_arr_del_2018 <- independence_day_del_2018$tot_arr_delay
  independence_day_dep_del_2018 <- independence_day_del_2018$tot_dep_delay
  
  labor_day_del_2018 <- hol_2018 %>% filter(labor_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  labor_day_arr_del_2018 <- labor_day_del_2018$tot_arr_delay
  labor_day_dep_del_2018 <- labor_day_del_2018$tot_dep_delay
  
  thanksgiving_del_2018 <- hol_2018 %>% filter(thanksgiving == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  thanksgiving_arr_del_2018 <- thanksgiving_del_2018$tot_arr_delay
  thanksgiving_dep_del_2018 <- thanksgiving_del_2018$tot_dep_delay
  
  winter_holiday_del_2018 <- hol_2018 %>% filter(winter_holiday == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  winter_holiday_arr_del_2018 <- winter_holiday_del_2018$tot_arr_delay    
  winter_holiday_dep_del_2018 <- winter_holiday_del_2018$tot_dep_delay 
  
  
########2019
  presidents_day_del_2019 <- hol_2019 %>% filter(presidents_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  presidents_day_arr_del_2019 <- presidents_day_del_2019$tot_arr_delay
  presidents_day_dep_del_2019 <- presidents_day_del_2019$tot_dep_delay
  
  easter_del_2019 <- hol_2019 %>% filter(easter == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  easter_arr_del_2019 <- easter_del_2019$tot_arr_delay
  easter_dep_del_2019 <- easter_del_2019$tot_dep_delay 
  
  memorial_day_del_2019 <- hol_2019 %>% filter(memorial_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  memorial_day_arr_del_2019 <- memorial_day_del_2019$tot_arr_delay
  memorial_day_dep_del_2019 <- memorial_day_del_2019$tot_dep_delay 
  
  independence_day_del_2019 <- hol_2019 %>% filter(independence_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  independence_day_arr_del_2019 <- independence_day_del_2019$tot_arr_delay
  independence_day_dep_del_2019 <- independence_day_del_2019$tot_dep_delay
  
  labor_day_del_2019 <- hol_2019 %>% filter(labor_day == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  labor_day_arr_del_2019 <- labor_day_del_2019$tot_arr_delay
  labor_day_dep_del_2019 <- labor_day_del_2019$tot_dep_delay
  
  thanksgiving_del_2019 <- hol_2019 %>% filter(thanksgiving == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  thanksgiving_arr_del_2019 <- thanksgiving_del_2019$tot_arr_delay
  thanksgiving_dep_del_2019 <- thanksgiving_del_2019$tot_dep_delay
  
  winter_holiday_del_2019 <- hol_2019 %>% filter(winter_holiday == 1) %>% summarise(tot_arr_delay = sum(ARR_DELAY, na.rm = T), tot_dep_delay = sum(DEP_DELAY, na.rm = T))
  winter_holiday_arr_del_2019 <- winter_holiday_del_2019$tot_arr_delay     
  winter_holiday_dep_del_2019 <- winter_holiday_del_2019$tot_dep_delay 
  












################
df_2015 <- data.frame("holiday" = c("presidents_day", "easter", "memorial_day", "independence_day", "labor_day", "thanksgiving", "winter_holiday"),
                      "Total" =  c( 87404, 232386, 154168, 156388, 118698, 177471, 255956),
                      "Total_arrival_delays_mins" = c(presidents_day_arr_del_2015, easter_arr_del_2015, memorial_day_arr_del_2015, independence_day_arr_del_2015, labor_day_arr_del_2015, thanksgiving_arr_del_2015, winter_holiday_arr_del_2015 ),
                      "Total_departure_delays_mins" = c(presidents_day_dep_del_2015, easter_dep_del_2015, memorial_day_dep_del_2015, independence_day_dep_del_2015, labor_day_dep_del_2015, thanksgiving_dep_del_2015, winter_holiday_dep_del_2015 ),
                      "Year" = c(2015))
df_2015$Delays_per_flight <- df_2015$Total_delays_mins/df_2015$Total
  p1 <- ggplot(df_2015, aes(reorder(holiday, Total), Total, fill = holiday)) +
  geom_col() +
  xlab("Holiday") +
  ylab("Total flights delayed") +
  ggtitle("2015 Holiday Flight Delays")+
  coord_flip() +
  theme(legend.position = "none")+
  scale_y_continuous(breaks= c(seq(0, 350000, 25000)))
  

################
df_2016 <- data.frame("holiday" = c("presidents_day", "easter", "memorial_day", "independence_day", "labor_day", "thanksgiving", "winter_holiday"),
                      "Total" =  c(92177, 242462, 160495, 160927, 121644, 181512, 248683),
                      "Total_arrival_delays_mins" = c(presidents_day_arr_del_2016, easter_arr_del_2016, memorial_day_arr_del_2016, independence_day_arr_del_2016, labor_day_arr_del_2016, thanksgiving_arr_del_2016, winter_holiday_arr_del_2016),
                      "Total_departure_delays_mins" = c(presidents_day_dep_del_2016, easter_dep_del_2016, memorial_day_dep_del_2016, independence_day_dep_del_2016, labor_day_dep_del_2016, thanksgiving_dep_del_2016, winter_holiday_dep_del_2016),
                      "Year" = c(2016))
p2 <- ggplot(df_2016, aes(reorder(holiday, Total), Total, fill = holiday)) +
  geom_col() +
  xlab("Holiday") +
  ylab("Total flights delayed") +
  ggtitle("2016 Holiday Flight Delays")+
  coord_flip()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks= c(seq(0, 350000, 25000)))

################
df_2017 <- data.frame("holiday" = c("presidents_day", "easter", "memorial_day", "independence_day", "labor_day", "thanksgiving", "winter_holiday"),
                      "Total" =  c( 88548, 235059, 156310, 157011, 121184, 178139, 271763),
                      "Total_arrival_delays_mins" = c(presidents_day_arr_del_2017, easter_arr_del_2017, memorial_day_arr_del_2017, independence_day_arr_del_2017, labor_day_arr_del_2017, thanksgiving_arr_del_2017, winter_holiday_arr_del_2017),
                      "Total_departure_delays_mins" = c(presidents_day_dep_del_2017, easter_dep_del_2017, memorial_day_dep_del_2017, independence_day_dep_del_2017, labor_day_dep_del_2017, thanksgiving_dep_del_2017, winter_holiday_dep_del_2017),
                      "Year" = c(2017))
p3 <- ggplot(df_2017, aes(reorder(holiday, Total), Total, fill = holiday)) +
  geom_col() +
  xlab("Holiday") +
  ylab("Total flights delayed") +
  ggtitle("2017 Holiday Flight Delays")+
  coord_flip()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks= c(seq(0, 350000, 25000)))


##############
df_2018 <- data.frame("holiday" = c("presidents_day", "easter", "memorial_day", "independence_day", "labor_day", "thanksgiving", "winter_holiday"),
                      "Total" =  c(112679, 297565, 197837, 195079, 153923, 230576, 347001),
                      "Total_arrival_delays_mins" = c(presidents_day_arr_del_2018, easter_arr_del_2018, memorial_day_arr_del_2018, independence_day_arr_del_2018, labor_day_arr_del_2018, thanksgiving_arr_del_2018, winter_holiday_arr_del_2018),
                      "Total_departure_delays_mins" = c(presidents_day_dep_del_2018, easter_dep_del_2018, memorial_day_dep_del_2018, independence_day_dep_del_2018, labor_day_dep_del_2018, thanksgiving_dep_del_2018, winter_holiday_dep_del_2018),
                      "Year" = c(2018))
p4 <- ggplot(df_2018, aes(reorder(holiday, Total), Total, fill = holiday)) +
  geom_col() +
  xlab("Holiday") +
  ylab("Total flights delayed") +
  ggtitle("2018 Holiday Flight Delays")+
  coord_flip()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks= c(seq(0, 350000, 25000)))


##############
df_2019 <- data.frame("holiday" = c("presidents_day", "easter", "memorial_day", "independence_day", "labor_day", "thanksgiving", "winter_holiday"),
                      "Total" =  c( 114671, 309648, 203819, 198121, 158436, 173110, 0),
                      "Total_arrival_delays_mins" = c(presidents_day_arr_del_2019, easter_arr_del_2019, memorial_day_arr_del_2019, independence_day_arr_del_2019, labor_day_arr_del_2019, thanksgiving_arr_del_2019, winter_holiday_arr_del_2019),
                      "Total_departure_delays_mins" = c(presidents_day_dep_del_2019, easter_dep_del_2019, memorial_day_dep_del_2019, independence_day_dep_del_2019, labor_day_dep_del_2019, thanksgiving_dep_del_2019, winter_holiday_dep_del_2019),
                      "Year" = c(2019))
p5 <- ggplot(df_2019, aes(reorder(holiday, Total), Total, fill = holiday)) +
  geom_col() +
  xlab("Holiday") +
  ylab("Total flights delayed") +
  ggtitle("2019 Holiday Flight Delays")+
  coord_flip()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks= c(seq(0, 350000, 25000)))


gridExtra::grid.arrange(p1, p2, p3, p4, p5, nrow = 5)


ggplot(df_2018, aes(reorder(holiday, Total_delays_mins), Total_delays_mins, fill = holiday)) +
  geom_col() +
  xlab("Holiday") +
  ylab("Total flights delayed") +
  ggtitle("2019 Holiday Flight Delays")+
  coord_flip()+
  theme(legend.position = "none")
  scale_y_continuous(breaks= c(seq(0, 350000, 25000)))
  
  
final_df <- do.call("rbind", list(df_2015, df_2016, df_2017, df_2018, df_2019))


ggplot(final_df, aes(Year, Total_departure_delays_mins))+
  geom_col()+
  facet_wrap(~ holiday, ncol=1)


##Holiday Flight Delays (2015-2019)
ggplot(final_df, aes(reorder(holiday, Total_departure_delays_mins),Total_departure_delays_mins, fill = factor(Year)))+
  geom_col(position = "dodge")+
  coord_flip() +
  xlab("Holiday") +
  ylab("Total no. of Flights Delayed (mins)")+
  ggtitle("Holiday Flight Delays (2015-2019)")

final_df$avg_flight_arrival_delay <- final_df$Total_arrival_delays_mins/final_df$Total
final_df$avg_flight_departure_delay <- final_df$Total_departure_delays_mins/final_df$Total


## Holiday Flight Delays - Avg. Delay Per Flight
ggplot(final_df, aes(reorder(holiday, avg_flight_departure_delay),avg_flight_departure_delay, fill = factor(Year)))+
  geom_col(position = "dodge")+
  coord_flip() +
  xlab("Holiday") +
  ylab("Avg. Flight Departure Delay (mins)")+
  ggtitle("Holiday Flight Delays - Avg. Delay Per Flight")

## Holiday Flight Delays - Avg. Arrival Delay Per Flight
ggplot(final_df, aes(reorder(holiday, avg_flight_arrival_delay),avg_flight_arrival_delay, fill = factor(Year)))+
  geom_col(position = "dodge")+
  coord_flip() +
  xlab("Holiday") +
  ylab("Avg. Flight Departure Delay (mins)")+
  ggtitle("Holiday Flight Delays - Avg. Arrival Delay Per Flight")
