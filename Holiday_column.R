library(readr)
library(dplyr)
library(lubridate)

#### YEAR: 2015###########################
flights_2015 <- read_csv("/Users/Eman Nagib/Documents/capstone-local/flights_2015.csv")
glimpse(flights_2015)
flights_2015$X1 <- NULL
flights_2015$X37 <- NULL
flights_2015$FL_DATE <- format(as.Date(flights_2015$FL_DATE), "%m/%d/%Y")

##HOLIDAYS DATES
presidents_day <- format(seq(as.Date("2015/2/12"), as.Date("2015/2/17"), by="days"), format="%m/%d/%Y")
easter <- format(seq(as.Date("2015/3/29"), as.Date("2015/4/12"), by="days"), format="%m/%d/%Y")
memorial_day <- format(seq(as.Date("2015/5/18"), as.Date("2015/5/27"), by="days"), format="%m/%d/%Y")
independence_day <- format(seq(as.Date("2015/6/26"), as.Date("2015/7/05"), by="days"), format="%m/%d/%Y")
labor_day <- format(seq(as.Date("2015/9/02"), as.Date("2015/9/09"), by="days"), format="%m/%d/%Y")
thanksgiving <- format(seq(as.Date("2015/11/20"), as.Date("2015/12/01"), by="days"), format="%m/%d/%Y")
winter_holiday <- format(seq(as.Date("2015/12/16"), as.Date("2016/1/05"), by="days"), format="%m/%d/%Y")

##ADDIND DUMMY COLUMNS FOR HOLIDAYS
flights_2015$presidents_day <- ifelse(flights_2015$FL_DATE %in% presidents_day, 1, 0)
flights_2015$easter <- ifelse(flights_2015$FL_DATE %in% easter, 1, 0)
flights_2015$memorial_day <- ifelse(flights_2015$FL_DATE %in% memorial_day, 1, 0)
flights_2015$independence_day <- ifelse(flights_2015$FL_DATE %in% independence_day, 1, 0)
flights_2015$labor_day <- ifelse(flights_2015$FL_DATE %in% labor_day, 1, 0)
flights_2015$thanksgiving <- ifelse(flights_2015$FL_DATE %in% thanksgiving, 1, 0)
flights_2015$winter_holiday <- ifelse(flights_2015$FL_DATE %in% winter_holiday, 1, 0)

## CHECKING
unique(flights_2015$presidents_day)
with(flights_2015, table(presidents_day))
with(flights_2015, table(easter))
with(flights_2015, table(memorial_day))
with(flights_2015, table(independence_day))
with(flights_2015, table(labor_day))
with(flights_2015, table(thanksgiving))
with(flights_2015, table(winter_holiday))

write_csv(flights_2015, "/Users/Eman Nagib/Documents/capstone-local/flights_2015_w_holidays.csv")



#### YEAR: 2016###########################
flights_2016 <- read_csv("/Users/Eman Nagib/Documents/capstone-local/flights_2016.csv")
glimpse(flights_2016)
flights_2016$X1 <- NULL
flights_2016$FL_DATE <- format(as.Date(flights_2016$FL_DATE), "%m/%d/%Y")

##HOLIDAYS DATES
presidents_day <- format(seq(as.Date("2016/2/11"), as.Date("2016/2/16"), by="days"), format="%m/%d/%Y")
easter <- format(seq(as.Date("2016/3/20"), as.Date("2016/4/3"), by="days"), format="%m/%d/%Y")
memorial_day <- format(seq(as.Date("2016/5/23"), as.Date("2016/6/01"), by="days"), format="%m/%d/%Y")
independence_day <- format(seq(as.Date("2016/7/01"), as.Date("2016/7/10"), by="days"), format="%m/%d/%Y")
labor_day <- format(seq(as.Date("2016/8/31"), as.Date("2016/9/07"), by="days"), format="%m/%d/%Y")
thanksgiving <- format(seq(as.Date("2016/11/18"), as.Date("2016/11/29"), by="days"), format="%m/%d/%Y")
winter_holiday <- format(seq(as.Date("2016/12/15"), as.Date("2017/1/04"), by="days"), format="%m/%d/%Y")

##ADDIND DUMMY COLUMNS FOR HOLIDAYS
flights_2016$presidents_day <- ifelse(flights_2016$FL_DATE %in% presidents_day, 1, 0)
flights_2016$easter <- ifelse(flights_2016$FL_DATE %in% easter, 1, 0)
flights_2016$memorial_day <- ifelse(flights_2016$FL_DATE %in% memorial_day, 1, 0)
flights_2016$independence_day <- ifelse(flights_2016$FL_DATE %in% independence_day, 1, 0)
flights_2016$labor_day <- ifelse(flights_2016$FL_DATE %in% labor_day, 1, 0)
flights_2016$thanksgiving <- ifelse(flights_2016$FL_DATE %in% thanksgiving, 1, 0)
flights_2016$winter_holiday <- ifelse(flights_2016$FL_DATE %in% winter_holiday, 1, 0)

## CHECKING
with(flights_2016, table(presidents_day))
with(flights_2016, table(easter))
with(flights_2016, table(memorial_day))
with(flights_2016, table(independence_day))
with(flights_2016, table(labor_day))
with(flights_2016, table(thanksgiving))
with(flights_2016, table(winter_holiday))


write_csv(flights_2016, "/Users/Eman Nagib/Documents/capstone-local/flights_2016_w_holidays.csv")





#### YEAR: 2017###########################
flights_2017 <- read_csv("/Users/Eman Nagib/Documents/capstone-local/flights_2017.csv")
glimpse(flights_2017)
flights_2017$X1 <- NULL
flights_2017$X37 <- NULL

flights_2017$FL_DATE <- format(as.Date(flights_2017$FL_DATE), "%m/%d/%Y")

##HOLIDAYS DATES
presidents_day <- format(seq(as.Date("2017/2/16"), as.Date("2017/2/21"), by="days"), format="%m/%d/%Y")
easter <- format(seq(as.Date("2017/4/9"), as.Date("2017/4/23"), by="days"), format="%m/%d/%Y")
memorial_day <- format(seq(as.Date("2017/5/22"), as.Date("2017/5/31"), by="days"), format="%m/%d/%Y")
independence_day <- format(seq(as.Date("2017/7/01"), as.Date("2017/7/10"), by="days"), format="%m/%d/%Y")
labor_day <- format(seq(as.Date("2017/8/30"), as.Date("2017/9/06"), by="days"), format="%m/%d/%Y")
thanksgiving <- format(seq(as.Date("2017/11/17"), as.Date("2017/11/28"), by="days"), format="%m/%d/%Y")
winter_holiday <- format(seq(as.Date("2017/12/14"), as.Date("2018/1/03"), by="days"), format="%m/%d/%Y")

##ADDIND DUMMY COLUMNS FOR HOLIDAYS
flights_2017$presidents_day <- ifelse(flights_2017$FL_DATE %in% presidents_day, 1, 0)
flights_2017$easter <- ifelse(flights_2017$FL_DATE %in% easter, 1, 0)
flights_2017$memorial_day <- ifelse(flights_2017$FL_DATE %in% memorial_day, 1, 0)
flights_2017$independence_day <- ifelse(flights_2017$FL_DATE %in% independence_day, 1, 0)
flights_2017$labor_day <- ifelse(flights_2017$FL_DATE %in% labor_day, 1, 0)
flights_2017$thanksgiving <- ifelse(flights_2017$FL_DATE %in% thanksgiving, 1, 0)
flights_2017$winter_holiday <- ifelse(flights_2017$FL_DATE %in% winter_holiday, 1, 0)

## CHECKING
with(flights_2017, table(presidents_day))
with(flights_2017, table(easter))
with(flights_2017, table(memorial_day))
with(flights_2017, table(independence_day))
with(flights_2017, table(labor_day))
with(flights_2017, table(thanksgiving))
with(flights_2017, table(winter_holiday))


write_csv(flights_2017, "/Users/Eman Nagib/Documents/capstone-local/flights_2017_w_holidays.csv")





#### YEAR: 2018###########################
flights_2018 <- read_csv("/Users/Eman Nagib/Documents/capstone-local/flights_2018.csv")
glimpse(flights_2018)
flights_2015$X1 <- NULL

flights_2018$FL_DATE <- as.Date(flights_2018$FL_DATE ,format="%m/%d/%Y")
flights_2018$FL_DATE <- format(as.Date(flights_2018$FL_DATE), "%m/%d/%Y")

##HOLIDAYS DATES
presidents_day <- format(seq(as.Date("2018/2/15"), as.Date("2018/2/20"), by="days"), format="%m/%d/%Y")
easter <- format(seq(as.Date("2018/3/25"), as.Date("2018/4/08"), by="days"), format="%m/%d/%Y")
memorial_day <- format(seq(as.Date("2018/5/21"), as.Date("2018/5/30"), by="days"), format="%m/%d/%Y")
independence_day <- format(seq(as.Date("2018/6/29"), as.Date("2018/7/08"), by="days"), format="%m/%d/%Y")
labor_day <- format(seq(as.Date("2018/8/29"), as.Date("2018/9/05"), by="days"), format="%m/%d/%Y")
thanksgiving <- format(seq(as.Date("2018/11/16"), as.Date("2018/11/27"), by="days"), format="%m/%d/%Y")
winter_holiday <- format(seq(as.Date("2018/12/14"), as.Date("2019/1/03"), by="days"), format="%m/%d/%Y")

##ADDIND DUMMY COLUMNS FOR HOLIDAYS
flights_2018$presidents_day <- ifelse(flights_2018$FL_DATE %in% presidents_day, 1, 0)
flights_2018$easter <- ifelse(flights_2018$FL_DATE %in% easter, 1, 0)
flights_2018$memorial_day <- ifelse(flights_2018$FL_DATE %in% memorial_day, 1, 0)
flights_2018$independence_day <- ifelse(flights_2018$FL_DATE %in% independence_day, 1, 0)
flights_2018$labor_day <- ifelse(flights_2018$FL_DATE %in% labor_day, 1, 0)
flights_2018$thanksgiving <- ifelse(flights_2018$FL_DATE %in% thanksgiving, 1, 0)
flights_2018$winter_holiday <- ifelse(flights_2018$FL_DATE %in% winter_holiday, 1, 0)

## CHECKING
with(flights_2018, table(presidents_day))
with(flights_2018, table(easter))
with(flights_2018, table(memorial_day))
with(flights_2018, table(independence_day))
with(flights_2018, table(labor_day))
with(flights_2018, table(thanksgiving))
with(flights_2018, table(winter_holiday))

write_csv(flights_2018, "/Users/Eman Nagib/Documents/capstone-local/flights_2018_w_holidays.csv")







#### YEAR: 2019###########################
flights_2019 <- read_csv("/Users/Eman Nagib/Documents/capstone-local/flights_2019.csv")
glimpse(flights_2019)
flights_2018$X1 <- NULL

flights_2019$FL_DATE <- as.Date(flights_2019$FL_DATE ,format="%m/%d/%Y")
flights_2019$FL_DATE <- format(as.Date(flights_2019$FL_DATE), "%m/%d/%Y")

##HOLIDAYS DATES
presidents_day <- format(seq(as.Date("2019/2/14"), as.Date("2019/2/19"), by="days"), format="%m/%d/%Y")
easter <- format(seq(as.Date("2019/3/15"), as.Date("2019/3/29"), by="days"), format="%m/%d/%Y")
memorial_day <- format(seq(as.Date("2019/5/20"), as.Date("2019/5/29"), by="days"), format="%m/%d/%Y")
independence_day <- format(seq(as.Date("2019/6/28"), as.Date("2019/7/07"), by="days"), format="%m/%d/%Y")
labor_day <- format(seq(as.Date("2019/8/28"), as.Date("2019/9/04"), by="days"), format="%m/%d/%Y")
thanksgiving <- format(seq(as.Date("2019/11/22"), as.Date("2019/12/03"), by="days"), format="%m/%d/%Y")
winter_holiday <- format(seq(as.Date("2019/12/14"), as.Date("2020/1/03"), by="days"), format="%m/%d/%Y")

##ADDIND DUMMY COLUMNS FOR HOLIDAYS
flights_2019$presidents_day <- ifelse(flights_2019$FL_DATE %in% presidents_day, 1, 0)
flights_2019$easter <- ifelse(flights_2019$FL_DATE %in% easter, 1, 0)
flights_2019$memorial_day <- ifelse(flights_2019$FL_DATE %in% memorial_day, 1, 0)
flights_2019$independence_day <- ifelse(flights_2019$FL_DATE %in% independence_day, 1, 0)
flights_2019$labor_day <- ifelse(flights_2019$FL_DATE %in% labor_day, 1, 0)
flights_2019$thanksgiving <- ifelse(flights_2019$FL_DATE %in% thanksgiving, 1, 0)
flights_2019$winter_holiday <- ifelse(flights_2019$FL_DATE %in% winter_holiday, 1, 0)

## CHECKING
with(flights_2019, table(presidents_day))
with(flights_2019, table(easter))
with(flights_2019, table(memorial_day))
with(flights_2019, table(independence_day))
with(flights_2019, table(labor_day))
with(flights_2019, table(thanksgiving))
with(flights_2019, table(winter_holiday))

write_csv(flights_2019, "/Users/Eman Nagib/Documents/capstone-local/flights_2019_w_holidays.csv")
