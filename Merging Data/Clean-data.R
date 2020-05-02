library(readr)
library(readxl)
library(tibble)
library(stringr)
library(ggplot2)
library(dplyr)

## Airfare-Table
airfare_df <- read_excel("Final Flights Data/Airfare(2015-2019).xlsx")
names(airfare_df) <- str_replace_all(tolower(names(airfare_df)), "`","")
names(airfare_df) <- str_replace_all(tolower(names(airfare_df)), " ","_")
names(airfare_df) <- gsub("[()]", "", names(airfare_df))
names(airfare_df) <- gsub("[_$]", "", names(airfare_df))
dim(airfare_df)
glimpse(airfare_df)


## On-time Performance Table
time_performance <- read_csv("Final Flights Data/Airline on-time perform(2015-2019).csv", guess_max = 1000)
names(time_performance) <- str_replace_all(tolower(names(time_performance)), "`","")
names(time_performance) <- str_replace_all(tolower(names(time_performance)), " ","_")
names(time_performance) <- str_replace_all(tolower(names(time_performance)), "-","_")
names(time_performance) <- str_replace_all(tolower(names(time_performance)), "[.]","")
names(time_performance) <- gsub("[()]", "", names(time_performance))
time_performance[time_performance == "N`A"] <- NA
time_performance[,3:21] <- lapply(time_performance[,3:21], as.double)
glimpse(time_performance)


## Delay Reasons Table
## Delay Reasons Table
delay_reasons <- read_csv("Final Flights Data/Delay Reasons by airline(2015-2019).csv", skip = 1)
delay_reasons$X11 <- NULL
names(delay_reasons) <- str_replace_all(tolower(names(delay_reasons)), "`","")
names(delay_reasons) <- str_replace_all(tolower(names(delay_reasons)), " ","_")
names(delay_reasons) <- str_replace_all(tolower(names(delay_reasons)), "-","")
names(delay_reasons) <- gsub("[()]", "", names(delay_reasons))
delay_reasons[delay_reasons == "N/A"] <- NA
delay_reasons[,3:10] <- lapply(delay_reasons[,3:10], as.double)
glimpse(delay_reasons)


## Load Factor Table
loadfct_df <- read_csv("Final Flights Data/Load Factor by Airline(2015-2019).csv")
names(loadfct_df) <- str_replace_all(tolower(names(loadfct_df)), "`","")
names(loadfct_df) <- str_replace_all(tolower(names(loadfct_df)), " ","_")
glimpse(loadfct_df)


## No. Flights Table
no_flights <- read_csv("Final Flights Data/Number of Flights(2015-2019).csv")
names(no_flights) <- str_replace_all(tolower(names(no_flights)), "`","")
names(no_flights) <- str_replace_all(tolower(names(no_flights)), " ","_")
no_flights <- na.omit(no_flights)
glimpse(no_flights)

## No. Passengers Table
no_passengers <- read_csv("Final Flights Data/Number of Passengers by Airline(2015-2019).csv")
names(no_passengers) <- str_replace_all(tolower(names(no_passengers)), "`","")
names(no_passengers) <- str_replace_all(tolower(names(no_passengers)), " ","_")
glimpse(no_passengers)

## Cancellation Fees
cancel_fees <- read_csv("Final Flights Data/Res Cxl Fees(2015-2019).csv")
names(cancel_fees) <- str_replace_all(tolower(names(cancel_fees)), "`","")
names(cancel_fees) <- str_replace_all(tolower(names(cancel_fees)), " ","_")
cancel_fees[cancel_fees == "N/A"] <- NA
cancel_fees[,3:15] <- lapply(cancel_fees[,3:15], as.double)
glimpse(cancel_fees)