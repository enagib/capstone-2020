library(readr)
library(dplyr)
library(randomForest)

##############################################################
## read in machine learning table
##############################################################

data <- read_csv("ml_data.csv")
data$X1 <- NULL

dep_delay_data <- data %>% select(-arr_delay)
arr_delay_data <- data %>% select(-total_dep_delay)

##############################################################
## Splitting data for test/train 
##############################################################

train_dep <- round(0.8 * nrow(dep_delay_data))
test_dep <- nrow(dep_delay_data) - train_dep

train_arr <- round(0.8 * nrow(arr_delay_data))
test_arr <- nrow(arr_delay_data) - train_arr

# spliting data into train and test
set.seed(888)
train_index_dep <- sample(nrow(dep_delay_data), train_dep) 
train_index_arr <- sample(nrow(arr_delay_data), train_arr) 

# get index for random rows
dep_test <- dep_delay_data[train_index_dep,]
dep_train <- dep_delay_data[-train_index_dep,]

arr_test <- arr_delay_data[train_index_arr,]
arr_train <- arr_delay_data[-train_index_arr,]

## set y variable
y_train_dep <- arr_test$arr_delay
y_test_dep <- arr_train$arr_delay

y_train_dep <- dep_test$total_dep_delay
y_test_dep <- dep_train$total_dep_delay

##############################################################
##Random Forest for departure delay
##############################################################
xnames_dep <- colnames(dep_delay_data)
xnames_dep <- xnames_dep[!xnames_dep %in% c("total_dep_delay")]
loopformula_dep <- "month ~ dest_BFM"
for (xname in xnames_dep) {  
  loopformula_dep <- paste(loopformula_dep, "+", xnames_dep, sep = "")}

f_dep <- as.formula(loopformula_dep)

fit_rf_dep <- randomForest(f_dep,
                       dep_train,
                       ntree = 10,
                       do.trace = F)

y_hat_train_dep <- predict(fit_rf_dep, dep_train)
mse_train_dep<- mean((y_hat_train_dep - y_train_dep)^2)

y_hat_test_dep <- predict(fit_rf_dep, dep_test)
mse_test_dep <- mean((y_hat_test_dep - y_test_Dep)^2)

# Print MSEstree_mse_train_random
mse_train_dep
mse_test_dep

#Examine variable importance
varImpPlot(fit_rf_def)


##############################################################
##Random Forest for arrival delay
##############################################################
xnames_arr <- colnames(arr_delay_data)
xnames_arr <- xnames_arr[!xnames_arr %in% c("arr_delay")]
loopformula_arr <- "month ~ dest_BFM"
for (xname in xnames_arr) {  
loopformula_arr <- paste(loopformula_arr, "+", xnames_arr, sep = "")}

f_arr <- as.formula(loopformula_arr)

fit_rf_arr <- randomForest(f_arr,
                           arr_train,
                           ntree = 10,
                           do.trace = F)

y_hat_train_arr <- predict(fit_rf_arr, arr_train)
mse_train_arr <- mean((y_hat_train_arr - y_train_arr)^2)

y_hat_test_arr <- predict(fit_rf_arr, arr_test)
mse_test_arr <- mean((y_hat_test_arr - y_test_arr)^2)

# Print MSEstree_mse_train_random
mse_train_arr
mse_test_arr

#Examine variable importance
varImpPlot(fit_rf_arr)