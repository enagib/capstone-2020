library(readr)
library(dplyr)

#########################################################################
#Merging all flights data
#########################################################################
flights_2015 <- read_csv("bigdata/flights_2015_w_holidays.csv")
flights_2016 <- read_csv("bigdata/flights_2016_w_holidays.csv")
flights_2017 <- read_csv("bigdata/flights_2017_w_holidays.csv")
flights_2018 <- read_csv("bigdata/flights_2018_w_holidays.csv")
flights_2019 <- read_csv("bigdata/flights_2019_w_holidays.csv")
flights_2019$X1 <- NULL
flights_2018$X1 <- NULL

flights <- do.call("rbind", list(flights_2015, flights_2016, flights_2017, flights_2018, flights_2019))
colnames(flights) <- tolower(colnames(flights))
flights <- flights %>% rename("day" = day_of_month, "date" = fl_date)

write_csv(flights, "flights.csv")


####################################################################################
#Merging flights and weather
####################################################################################
flights <- read_csv("bigdata/flights.csv")
weather <- read_csv("bigdata/station_weather_airport.csv")
colnames(flights) <- tolower(colnames(flights))
weather <- weather %>% rename("day" = da, "month" = mo, "origin" = CALL_ID) 
weather$date <- gsub('\\s+', '', (paste(weather$month,"/",weather$day,"/", weather$year)))
weather <- weather %>% select(-c(count_temp,count_dewp,count_slp,count_stp,count_visib,count_wdsp))

data1 <- flights %>% left_join(weather, by = c("origin","date"))

####################################################################################
#Final data set
####################################################################################
write_csv(data, "bigdata/flights_data.csv")
data <- read_csv("bigdata/flights_data.csv")

