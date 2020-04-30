library(readr)
library(dplyr)
library(robustHD)
options(scipen = 999)
#########################################################################
#Merging all flights data
#########################################################################
flights_2015 <- read_csv("flights_2015_w_holidays.csv")
flights_2016 <- read_csv("flights_2016_w_holidays.csv")
flights_2017 <- read_csv("flights_2017_w_holidays.csv")
flights_2018 <- read_csv("flights_2018_w_holidays.csv")
flights_2019 <- read_csv("flights_2019_w_holidays.csv")
flights_2019$X1 <- NULL
flights_2018$X1 <- NULL

flights <- do.call("rbind", list(flights_2015, flights_2016, flights_2017, flights_2018, flights_2019))
flights <- flights %>%  mutate(total_arr_delay = ARR_DELAY + TAXI_IN, speed = DISTANCE/ACTUAL_ELAPSED_TIME,
                               total_dep_delay = DEP_DELAY + TAXI_OUT)
flights$ARR_DELAY <- NULL
colnames(flights) <- tolower(colnames(flights))
flights <- flights %>% rename("day" = day_of_month, "date" = fl_date)


#write_csv(flights, "flights.csv")
flights <- na.omit(flights)
delete <- c("US", "OO", "EV", "MQ", "VX", "YV", "OH", "9E", "YX", "4", "6", "9")
data_top10 <- flights[!flights$op_unique_carrier %in% delete,]

## the 99th percentile
qrt_99_d <- quantile(data_top10$total_dep_delay, probs = 0.99) 
qrt_99_a <- quantile(data_top10$total_arr_delay, probs = 0.99) 
## the 1th percentile: 0 minute
qrt_01_d <- quantile(data_top10$total_dep_delay, probs = 0.01)
qrt_01_a <- quantile(data_top10$total_arr_delay, probs = 0.01)

#Departure
a <- data_top10 %>% filter(total_dep_delay <= qrt_99_d) %>% 
  group_by(op_unique_carrier, origin, dest) %>% 
  summarise(med_dep = median(total_dep_delay)) %>% ungroup() 
dd_unw2 <- inner_join(data_top10, a, by = c("op_unique_carrier", "origin", "dest"))
flights1 <- dd_unw2 %>% 
  mutate(total_dep_delay2 = ifelse(total_dep_delay >= qrt_99_d, med_dep, total_dep_delay)) %>% 
  mutate(total_dep_delay2 = ifelse(total_dep_delay2 <= qrt_01_d, 0, total_dep_delay2))

#Arrival
a <- flights1 %>% filter(total_arr_delay <= qrt_99_a) %>% 
  group_by(op_unique_carrier, origin, dest) %>% 
  summarise(med_arr = median(total_arr_delay)) %>% ungroup() 
dd_unw2 <- inner_join(flights1, a, by = c("op_unique_carrier", "origin", "dest"))
flights1 <- dd_unw2 %>% 
  mutate(total_arr_delay2 = ifelse(total_arr_delay >= qrt_99_a, med_arr, total_arr_delay)) %>% 
  mutate(total_arr_delay2 = ifelse(total_arr_delay2 <= qrt_01_a, qrt_01_a, total_arr_delay2))

# high_qrt <- quantile(data_top10$total_dep_delay, probs = c(0.99))
# low_qrt <- quantile(data_top10$total_dep_delay, probs = c(0.01))
# data_top10$total_dep_delay <- ifelse(data_top10$total_dep_delay > high_qrt, high_qrt, data_top10$total_dep_delay)
# data_top10$total_dep_delay <- ifelse(data_top10$total_dep_delay < low_qrt, low_qrt, data_top10$total_dep_delay)

set.seed(888)
flights1 <- flights1 %>% 
  group_by(year,month) %>% 
  sample_n(2000, replace = F) %>% 
  ungroup(year, month)


####################################################################################
#Merging flights and weather
####################################################################################
#flights <- read_csv("bigdata/flights.csv")
weather <- read_csv("station_weather_airport.csv")
weather <- weather %>% rename("day" = da, "month" = mo, "origin" = CALL_ID) 
weather$date <- gsub('\\s+', '', (paste(weather$month,"/",weather$day,"/", weather$year)))
weather <- weather %>% select(-c(count_temp,count_dewp,count_slp,count_stp,count_visib,count_wdsp))


####################################################################################
#Final data set
####################################################################################
data <- flights1 %>% inner_join(weather, by = c("origin","date"))
data %>% group_by(year.x) %>%  count()

colnames(data) <- tolower(colnames(data))
data$city <- tolower(data$city) 
data$location <- tolower(data$location) 
data$name <- tolower(data$name) 

data <- data %>% select(-c(year.y,month.y,day.y, day.x, origin_city_name,
                           flag_max, flag_min, flag_prcp, flights,
                           stn, wban, mxpsd, max, min, gust, sndp, state)) %>% 
  rename("wind_speed" = wdsp,
         "dew_point_temp" = dewp,
         "sea_level_pressure" = slp,
         "station_pressure" = stp,
         "visibilty" = visib,
         "year" = year.x,
         "month" = month.x,
         "orig_city_name" = city)


data$dest_city_name <- gsub(",.*","",data$dest_city_name)

#write_csv(data, "full_data.csv")


####################################################################################
##Machine Learning Table
####################################################################################
ml_data <- data %>% select(-c(year, quarter, date, tail_num, op_carrier_fl_num, origin_airport_id ,
                              origin_state_abr, origin_state_nm, dest_airport_id, dest_city_name,
                              dest_state_abr, crs_dep_time, dep_time, dep_delay, taxi_out, wheels_off,
                              wheels_on, taxi_in, crs_arr_time, arr_time, crs_elapsed_time, actual_elapsed_time,
                              air_time, name, orig_city_name, location, cancelled, diverted, dest_state_nm, dep_delay, 
                              total_dep_delay, total_arr_delay, med_dep, med_arr)) %>% 
  rename("total_dep_delay" = total_dep_delay2,
         "total_arr_delay" = total_arr_delay2)


##Dummifying columns
ml_data_dummy <- fastDummies::dummy_cols(ml_data, select_columns = c("month", "day_of_week",
                                                                     "op_unique_carrier",
                                                                     "origin", "dest"))
ml_data_dummy <- ml_data_dummy %>% 
  rename("Allegiant" = op_unique_carrier_G4,
         "JetBlue" = op_unique_carrier_B6, 
         "Frontier" = op_unique_carrier_F9 ,
         "American" = op_unique_carrier_AA, 
         "Hawaiian" = op_unique_carrier_HA,
         "Southwest" = op_unique_carrier_WN , 
         "Alaska" = op_unique_carrier_AS, 
         "United" = op_unique_carrier_UA , 
         "Spirit" = op_unique_carrier_NK,
         "Delta" = op_unique_carrier_DL)

ml_data_dummy <-  ml_data_dummy %>% select(-c(month, day_of_week, op_unique_carrier, origin, dest))

dep_small_data <- ml_data_dummy %>% select(-c(total_arr_delay, speed))
arr_small_data <- ml_data_dummy %>% select(-c(distance))


write_csv(dep_small_data, "dep_small_data.csv")
write.csv(arr_small_data, "arr_small_data.csv")



