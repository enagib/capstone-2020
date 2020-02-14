
Jan <- read.csv("2016 01.csv")
Feb <- read.csv("2016 02.csv")
Mar <- read.csv("2016 03.csv")
Apr <- read.csv("2016 04.csv")
May <- read.csv("2016 05.csv")
Jun <- read.csv("2016 06.csv")
Jul <- read.csv("2016 07.csv")
Aug <- read.csv("2016 08.csv")
Sep <- read.csv("2016 09.csv")
Oct <- read.csv("2016 10.csv")
Nov <- read.csv("2016 11.csv")
Dec <- read.csv("2016 12.csv")

flight_2016 <- rbind(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
#head(flight_2016,10)
#tail(flight_2016,10)

#View(flight_2016)


###############################################1. Most delay flight per month  

airline_delay <-  flight_2016 %>% filter(ARR_DELAY>0) %>% 
  group_by(MONTH,OP_CARRIER_FL_NUM) %>% summarize(avg_delay_time=mean(ARR_DELAY)) %>% top_n(3)

ggplot(airline_delay, aes(x=MONTH, y=avg_delay_time,label=OP_CARRIER_FL_NUM)) +
  geom_text()+ggtitle("Top3 Most Delayed flight per Month")


################################################2. Tail num that has the highest mileage of flight 
tail_mile_max <- flight_2016 %>% group_by(TAIL_NUM) %>% summarise(Total_Mileage=sum(DISTANCE))
tail_mile_max_top20 <- tail_mile_max %>% top_n(21) %>% filter(Total_Mileage<2500000)
#View(tail_mile_max_top20)

ggplot(tail_mile_max_top20, aes(x=TAIL_NUM, y=Total_Mileage)) + 
  geom_bar(stat = "identity")+coord_flip()+ggtitle("Tail Num with highest total mileage of flight")

##########################3. Most busy airports - Origin 
origin_num_flights_top20 <- flight_2016 %>% group_by(ORIGIN,ORIGIN_CITY_NAME) %>% 
  summarise(Num_flights=n()) %>%   arrange(desc(Num_flights))
origin_num_flights_top20 <- head(origin_num_flights_top20,20)
View(origin_num_flights_top20)

ggplot(origin_num_flights_top20, aes(x=ORIGIN, y=Num_flights,label=ORIGIN_CITY_NAME)) + 
  geom_bar(stat = "identity")+coord_flip()+geom_text(hjust=-0.03)+ggtitle("Most Busy Airport 2016")


#4. In which month, it has the highest cancellation rate 
month_cancel <- flight_2016 %>% group_by(MONTH,CANCELLED) %>% summarise(Num_flights=n())

month_cancelled <- month_cancel %>% group_by(CANCELLED)
month_cancelled$CANCELLED <- as.factor(month_cancelled$CANCELLED)
#View(month_cancelled)

total_flights <- month_cancelled %>% group_by(MONTH) %>% summarise(total_flights=sum(Num_flights))
total_cancelled <- month_cancelled %>% filter(CANCELLED==1) %>% group_by(MONTH) 
#View(total_cancelled)


total_num_flights <- total_flights$total_flights
total_num_cancelled <- total_cancelled$Num_flights

month=c(1,2,3,4,5,6,7,8,9,10,11,12)
sum_noncancelled_flights=total_num_flights
sum_cancelled=total_num_cancelled

flight_cancellation_rate =data.frame(month,sum_noncancelled_flights,sum_cancelled)
flight_cancellation_rate

cancellation_rate <- flight_cancellation_rate %>% group_by(month) %>% 
  summarise(cancellation_rate=sum_cancelled/(sum_cancelled+sum_noncancelled_flights))

p <- ggplot(cancellation_rate, aes(x=month, y=cancellation_rate,label=cancellation_rate)) +
  geom_point(aes(size=5))+ggtitle("Flight cancellation rate of each month in 2016")+geom_line()

p + theme(axis.text.x = element_text(face="bold", color="#993333", 
                                     size=14),
          axis.text.y = element_text(face="bold", color="#993333", 
                                     size=10, angle=30))+scale_x_continuous(breaks = seq(1, 12, by = 1))

#5. Most Delayed Airport per Month
library(ggrepel)
delay <- flight_2016 %>% filter(ARR_DELAY>0)
avg_delay_month <- delay %>% group_by(MONTH,ORIGIN) %>% 
  summarise(avg_delay_time=mean(ARR_DELAY)) %>% top_n(3)

avg_delay_month 


k <- ggplot(avg_delay_month, aes(x=MONTH, y=avg_delay_time,label=ORIGIN))
k + scale_y_continuous(limits=c(50, 350))+
  ggtitle("Top3 Most Delayed Airport per Month")+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  geom_text_repel(angle=-45,colour = "Black", fontface = "bold",size=4)
