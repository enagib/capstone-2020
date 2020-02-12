library(readr)
library(readxl)
library(tibble)
library(stringr)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(ggrepel)
library("xlsx")
install.packages("xlsx")


  flightdelay <- read.csv("Final Flights Data/Airline on-time perform(2015-2019).csv")
View(flightdelay)

flightdelay = flightdelay %>% na.omit()
dim(flightdelay)
glimpse(flightdelay)

names(flightdelay)
#mycols<-c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#B15928")


flightdelay %>% group_by(Year) %>% 
  ggplot(aes(fill=factor(Carriers), x=factor(Year), y=Total.Number)) + 
  geom_bar(stat="identity") 
  #scale_fill_manual(name='',values=colorRampPalette(mycols)(length(unique(flightdelay$Carriers)))) + 


flightdelay %>% 
  ggplot(aes(fill=factor(Carriers), x=factor(Year), y=Percent.Flights.Cancelled)) + 
  geom_bar(stat="identity") 
  #scale_fill_manual(name='',values=colorRampPalette(mycols)(length(unique(factor(flightdelay$Carriers)))))  

unique(flightdelay$Carriers)
Y2019 <- flightdelay %>% filter(Year == "2019")
View(Y2019)
names(Y2019)
ggplot(Y2019, aes(x=Carriers, y=Average.Arrival.Delay..minutes.)) + 
  geom_bar(aes(fill=Carriers),stat='identity') +
  coord_flip() 
--------
airport <- read_xlsx("Final Flights Data/Airfare(2015-2019).xlsx")
#View(airport)
names(airport)
unique(airport$`Airport Code`)


airport %>% group_by(Year) %>% top_n(10) %>% 
  ggplot(aes(fill=(`Airport Code`), x=factor(Year), y='Average Fare ($)')) + 
  geom_bar(stat="identity") + theme_fivethirtyeight() 
summary(airport)
airport$`Airport Code` = as.factor(airport$`Airport Code`)
summary(airport)
glimpse(airport)
Avg <- airport %>% 
 group_by(`Airport Code`) %>% 
  summarise(Avgfair = mean(`Average Fare ($)`)) %>% top_n(10)
Avg %>% ggplot(aes(x=`Airport Code`,y=Avgfair)) +
  geom_bar(stat="identity") + coord_flip() +
  theme_fivethirtyeight() 

-----
airport %>% group_by(`Airport Code`, Year) %>% top_n(10) %>% 
  ggplot(aes(x=`Airport Code`,y=`Average Fare ($)`)) + 
  geom_bar(aes(fill=Year),stat='identity') + 
  coord_flip() + theme_fivethirtyeight() +
  scale_fill_gradientn(name='',colors=colorRampPalette(c("gray","#46ACC8"))(10)) + 
  theme(legend.position='none',plot.title = element_text(size =10))
  

------
