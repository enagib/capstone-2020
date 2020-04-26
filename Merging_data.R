library(readr)
library(dplyr)

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
colnames(flights) <- tolower(colnames(flights))
flights <- flights %>% rename("day" = day_of_month, "date" = fl_date)

#write_csv(flights, "flights.csv")
flights <- na.omit(flights)

set.seed(888)
fights1 <- flights %>% 
  group_by(year) %>% 
  sample_frac(.20) %>% 
  ungroup(year)


####################################################################################
#Merging flights and weather
####################################################################################
#flights <- read_csv("bigdata/flights.csv")
weather <- read_csv("station_weather_airport.csv")
colnames(fights1) <- tolower(colnames(fights1))
weather <- weather %>% rename("day" = da, "month" = mo, "origin" = CALL_ID) 
weather$date <- gsub('\\s+', '', (paste(weather$month,"/",weather$day,"/", weather$year)))
weather <- weather %>% select(-c(count_temp,count_dewp,count_slp,count_stp,count_visib,count_wdsp))

data1 <- flights %>% left_join(weather, by = c("origin","date"))

####################################################################################
#Final data set
####################################################################################
write_csv(data, "bigdata/flights_data.csv")
data <- read_csv("bigdata/flights_data.csv")

<<<<<<< HEAD
data <- fights1 %>% inner_join(weather, by = c("origin","date"))
#data %>% group_by(year.x) %>%  count()

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

write_csv(data, "full_data.csv")


####################################################################################
##Machine Learning Table
####################################################################################
data$total_dep_delay <- data$dep_delay + data$taxi_out

ml_data <- data %>% select(-c(year, quarter, date, tail_num, op_carrier_fl_num, origin_airport_id ,
                              origin_state_abr, origin_state_nm, dest_airport_id, dest_city_name,
                              dest_state_abr, crs_dep_time, dep_time, dep_delay, taxi_out, wheels_off,
                              wheels_on, taxi_in, crs_arr_time, arr_time, crs_elapsed_time, actual_elapsed_time,
                              air_time, name, orig_city_name, location, cancelled, diverted, dest_state_nm))


##Dummifying columns
ml_data_dummy <- fastDummies::dummy_cols(ml_data, select_columns = c(month, day_of_week,
                                                                     op_unique_carrier,
                                                                     origin, dest))
ml_data_dummy <- ml_data_dummy %>% select(-c(op_unique_carrier_US, op_unique_carrier_OO, 
                                             op_unique_carrier_EV, op_unique_carrier_MQ, 
                                             op_unique_carrier_VX, op_unique_carrier_YV, 
                                             op_unique_carrier_OH, op_unique_carrier_9E,
                                             op_unique_carrier_YX, op_unique_carrier_4,
                                             op_unique_carrier_6, op_unique_carrier_9)) %>% 
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


ml_data <- write.csv(ml_data_dummy, "ml_data.csv" )
