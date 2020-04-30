library(readr)
library(dplyr)
library(randomForest)

##############################################################
## read in machine learning table
##############################################################
dep_delay_data <- read_csv("dep_data.csv")
arr_delay_data <- read_csv("arr_data.csv")
arr_delay_data$X1 <- NULL

# arr_delay_data <- arr_small_data
# dep_delay_data <- dep_small_data

# data <- ml_data_dummy
dep_delay_data <- dep_delay_data[, -grep("dest", colnames(dep_delay_data))]
dep_delay_data <- dep_delay_data[, -grep("orig", colnames(dep_delay_data))]
arr_delay_data <- arr_delay_data[, -grep("dest", colnames(arr_delay_data))]
arr_delay_data <- arr_delay_data[, -grep("orig", colnames(arr_delay_data))]


cols <- c("presidents_day", "easter","memorial_day",        
          "independence_day", "labor_day", "thanksgiving","winter_holiday",               
          "fog", "rain_drizzle", "snow_ice_pellets", "hail",                
          "thunder", "tornado_funnel_cloud","month_1", "month_10",            
          "month_11", "month_12", "month_2","month_3", "month_4",             
          "month_5", "month_6",  "month_7", "month_8","month_9",             
          "day_of_week_1", "day_of_week_2", "day_of_week_3","day_of_week_4", "day_of_week_5",       
          "day_of_week_6", "day_of_week_7", "American","Alaska","JetBlue",             
          "Delta", "Frontier", "Allegiant", "Hawaiian", "Spirit", "United","Southwest")

dep_delay_data[,cols] <- lapply(dep_delay_data[,cols] , factor)
arr_delay_data[,cols] <- lapply(arr_delay_data[,cols] , factor)

##############################################################
## Splitting data for test/train 
##############################################################
dep_train_t <- dep_train %>% select(-total_dep_delay)
y_hat_train_dep <- predict(fit_rf_dep, dep_train_t)
mse_train_dep<- mean((y_hat_train_dep - y_train_dep)^2)
train_dep <- round(0.8 * nrow(dep_delay_data))
test_dep <- nrow(dep_delay_data) - train_dep

# spliting data into train and test
set.seed(888)
train_index_dep <- sample(nrow(dep_delay_data), train_dep)

# get index for random rows
dep_test <- dep_delay_data[train_index_dep,]
dep_train <- dep_delay_data[-train_index_dep,]

## set y variable
y_train_dep <- dep_train$total_dep_delay
y_test_dep <- dep_test$total_dep_delay

##############################################################
##Random Forest for departure delay
##############################################################
# xnames_dep <- colnames(dep_delay_data)
# xnames_dep <- xnames_dep[!xnames_dep %in% c("total_dep_delay")]
# loopformula_dep <- "distance ~ Southwest"
# for (k in 1:50) {
#   loopformula_dep <- paste(loopformula_dep, "+",xnames_dep[k], sep = " ")}
# f_dep <- as.formula(loopformula_dep)

dep_test <- dep_test %>% select(-total_dep_delay)

fit_rf_dep <- randomForest(total_dep_delay~.,
                           data = dep_train,
                           ntree = 150,
                           do.trace = F)

# fit_rf_dep <- rfcv(dep_train, y_tr
dep_train_t <- dep_train %>% select(-total_dep_delay)
y_hat_train_dep <- predict(fit_rf_dep, dep_train_t)
mse_train_dep<- mean((y_hat_train_dep - y_train_dep)^2)


y_hat_test_dep <- predict(fit_rf_dep, dep_test)
mse_test_dep <- mean((y_hat_test_dep - y_test_dep)^2)

# Print MSES
mse_train_dep
mse_test_dep

#Examine variable importance
varImpPlot(fit_rf_dep)


##############################################################
## Splitting data for test/train 
##############################################################
train_arr <- round(0.8 * nrow(arr_delay_data))
test_arr <- nrow(arr_delay_data) - train_arr

# spliting data into train and test
train_index_arr <- sample(nrow(arr_delay_data), train_arr)

# get index for random rows
arr_test <- arr_delay_data[train_index_arr,]
arr_train <- arr_delay_data[-train_index_arr,]

## set y variable
y_train_arr <- arr_train$total_arr_delay
y_test_arr <- arr_test$total_arr_delay

##############################################################
##Random Forest for arrival delay
##############################################################
arr_test <- arr_test %>% select(-total_arr_delay)

fit_rf_arr <- randomForest(total_arr_delay~.,
                           data = arr_train,
                           ntree = 150,
                           do.trace = F)

arr_train_t <- arr_train %>% select(-total_arr_delay)
y_hat_train_arr <- predict(fit_rf_arr, arr_train_t)
mse_train_arr <- mean((y_hat_train_arr - y_train_arr)^2)

y_hat_test_arr <- predict(fit_rf_arr, arr_test)
mse_test_arr <- mean((y_hat_test_arr - y_test_arr)^2)

# Print MSES
mse_train_arr
mse_test_arr

#Examine variable importance
varImpPlot(fit_rf_arr)
