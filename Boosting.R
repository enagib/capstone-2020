library(gbm)

##############################################################
## read in machine learning table
##############################################################

#data <- read_csv("ml_data.csv")
data <- ml_data_dummy
data <- data[, -grep("dest", colnames(data))]
data <- data[, -grep("orig", colnames(data))]
data <- data %>% select(-c(month, day_of_week, op_unique_carrier))

cols <- c("presidents_day", "easter","memorial_day",        
          "independence_day", "labor_day", "thanksgiving","winter_holiday",               
          "fog", "rain_drizzle", "snow_ice_pellets", "hail",                
          "thunder", "tornado_funnel_cloud","month_1", "month_10",            
          "month_11", "month_12", "month_2","month_3", "month_4",             
          "month_5", "month_6",  "month_7", "month_8","month_9",             
          "day_of_week_1", "day_of_week_2", "day_of_week_3","day_of_week_4", "day_of_week_5",       
          "day_of_week_6", "day_of_week_7", "American","Alaska","JetBlue",             
          "Delta", "Frontier", "Allegiant", "Hawaiian", "Spirit", "United","Southwest")

data[,cols] <- lapply(data[,cols] , factor)

dep_delay_data <- data %>% select(-c(arr_delay))
arr_delay_data <- data 

##############################################################
## Splitting data for test/train 
##############################################################

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
##Boosting for departure delay
##############################################################

dep_train <- dep_train %>% select(-tornado_funnel_cloud)
dep_test <- dep_test %>%  select(-tornado_funnel_cloud, -total_dep_delay)

fit_btree_dep <- gbm(total_dep_delay~.,
                     data = dep_train,                
                     distribution = "gaussian",                 
                     n.trees = 375,                 
                     interaction.depth = 4,                 
                     shrinkage = 0.01)

dep_train_t <- dep_train %>% select(-total_dep_delay)
y_hat_train_boosting_dep <- predict(fit_btree_dep, dep_train_t, n.trees = 300)
mse_train_boosting_dep <- mean((y_hat_train_boosting_dep - y_train_dep)^2)

y_hat_test_boosting_dep <- predict(fit_btree_dep, dep_test, n.trees = 300)
mse_test_boosting_dep <- mean((y_hat_test_boosting_dep - y_test_dep)^2)

relative.influence(fit_btree_dep)

# Print MSEs
fit_btree_dep
mse_train_boosting_dep
mse_test_boosting_dep


#Generating a Prediction matrix for each Tree
n.trees = seq(from=1 ,to=500, by=10)
predmatrix <- predict(fit_btree_dep, dep_test, n.trees = n.trees)
#Calculating The Mean squared Test Error
#contains the Mean squared test error for each of the 100 trees averaged
train.error <- with(dep_test, apply( (predmatrix- y_test_dep)^2,2,mean))
#Plotting the test errors
par(xpd=FALSE)
plot(n.trees , train.error, pch=20, col="maroon", xlab="Number of Trees",     
     ylab="Test MSE", main = "Perfomance of Boosting on Test Set")


##############################################################
##Boosting for arrival delay
##############################################################
arr_train <- arr_train %>% select(-tornado_funnel_cloud)
arr_test <- arr_test %>%  select(-tornado_funnel_cloud, -total_dep_delay)

fit_btree_arr<- gbm(arr_delay~.,
                    data = arr_train,                
                    distribution = "gaussian",                 
                    n.trees = 350,                 
                    interaction.depth = 4,                 
                    shrinkage = 0.01)

arr_train_t <- arr_train %>% select(-arr_delay)
y_hat_train_boosting_arr <- predict(fit_btree_arr, arr_train_t, n.trees = 350)
mse_train_boosting_arr <- mean((y_hat_train_boosting_arr - y_train_arr)^2)

y_hat_test_boosting_arr <- predict(fit_btree_arr, arr_test, n.trees = 350)
mse_test_boosting_arr <- mean((y_hat_test_boosting_arr - y_test_arr)^2)

relative.influence(fit_btree_arr)

# Print MSEs
fit_btree_arr
mse_train_boosting_arr
mse_test_boosting_arr


#Generating a Prediction matrix for each Tree
n.trees = seq(from=1 ,to=400, by=10)
predmatrix <- predict(fit_btree_arr, arr_test, n.trees = n.trees)
#Calculating The Mean squared Test Error
#contains the Mean squared test error for each of the 100 trees averaged
train.error <- with(arr_test, apply( (predmatrix- y_test_arr)^2,2,mean))
#Plotting the test errors
par(xpd=FALSE)
plot(n.trees , train.error, pch=20, col="maroon", xlab="Number of Trees",     
     ylab="Test MSE", main = "Perfomance of Boosting on Test Set")
