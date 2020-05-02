### Delay Reasons 

library(readr)
library(tidyverse)

delay_reason <- read_csv('Final Flights Data/Delay Reasons by airline(2015-2019).csv', skip = 1)
View(delay_reason)

delay_reason %>% 
  group_by(Year) %>% 
  summarise()
  

on_time <- read_csv('Final Flights Data/Airline on-time perform(2015-2019).csv', guess_max = 1000)
View(on_time)
names(on_time) <- str_replace_all(tolower(names(on_time)), "`","")
names(on_time) <- str_replace_all(tolower(names(on_time)), " ","_")
names(on_time) <- str_replace_all(tolower(names(on_time)), "-","_")
names(on_time) <- str_replace_all(tolower(names(on_time)), "[.]","")
names(on_time) <- gsub("[()]", "", names(on_time))
on_time[on_time == "N`A"] <- NA
colSums(is.na(on_time))



colSums(is.na(on_time))
summary(on_time)
## year by year performance 
on_time %>% 
  group_by(year) %>% 
  summarise(carrier = mean(as.numeric(carrier_avg_mins), na.rm = TRUE),
            weather = mean(as.numeric(weatheravg_mins), na.rm = TRUE),
            aviation = mean(as.numeric(national_aviation_systemavg_mins), na.rm = TRUE),
            late_arrival = mean(as.numeric(late_aircraft_arrivalavg_mins), na.rm = TRUE),
            security = mean(as.numeric(securityavg_mins), na.rm = TRUE)) %>% 
  mutate(total = carrier + weather + aviation + late_arrival) %>% 
  mutate(carrier_p = carrier * 100 / total,
         weather_p = weather * 100 / total,
         aviation_p = aviation * 100 / total,
         late_arrival_p = late_arrival * 100 / total,
         security_p = security * 100 / total) %>% 
  ungroup() -> result

result


par(mar = c(5, 4, 4, 8))
plot(result$year, result$carrier_p, main="Delay Reasons" ,type="l", xlab="Year",
     ylab="Probability", col = "#a86b32", lwd = 3, ylim = c(0, 40))
lines(result$year, result$weather_p, col = '#326fa8', type = "l", lwd = 3)
lines(result$year, result$aviation_p, col = '#6732a8', type = "l", lwd = 3)
lines(result$year, result$late_arrival_p, col =  '#55DFD1', type = "l", lwd = 3)
text(result$year[5], result$weather_p[5], labels = round(result$weather_p[5],2), 
     adj = c(0, 1))
text(result$year[5], result$carrier_p[5], labels = round(result$carrier_p[5],2), 
     adj = c(0, 1))
text(result$year[5], result$aviation_p[5], labels = round(result$aviation_p[5],2), 
     adj = c(0, 1))
text(result$year[5], result$late_arrival_p[5], labels = round(result$late_arrival_p[5],2), 
     adj = c(0, 1))
text(result$year[5], result$security_p[5], labels = round(result$security_p[5],2), 
     adj = c(0, 1))
legend("topright", inset = c(-0.5, 0),
       legend = c("carrier", "weather", 'aviation', 'late arrival', 'security'),
       fill=c("#a86b32", '#326fa8', '#6732a8', '#55DFD1','#1F7D3F' ))

