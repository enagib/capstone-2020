library(dummies)
library(fastDummies)

flights_2015 <- read_csv("/Users/Eman Nagib/Documents/capstone-local/flights_2015.csv")
glimpse(flights_2015)


data <- flights_2015 %>% 
  select(YEAR, OP_UNIQUE_CARRIER, MONTH, DAY_OF_WEEK, DAY_OF_MONTH, 
         QUARTER, DEP_DELAY, ARR_DELAY, ORIGIN, DEST, 
         DISTANCE)

no_flightsdf <- flights_2015 %>% 
  group_by(YEAR, MONTH, DAY_OF_MONTH,ORIGIN, OP_UNIQUE_CARRIER) %>% 
  summarise(no_flights = n())


final_df <- left_join(data, no_flightsdf, 
                  by = c("YEAR", "MONTH", "DAY_OF_MONTH","ORIGIN","OP_UNIQUE_CARRIER"))
                  

df_dummy <- final_df %>% select(OP_UNIQUE_CARRIER, MONTH, DAY_OF_WEEK, DAY_OF_MONTH, 
                    QUARTER, ORIGIN, DEST) %>% dummy_cols()


View(df_dummy)
