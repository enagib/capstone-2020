library(readr)
library(tidyverse)

big_arr_data <- read_csv('bigdata2/arr_big_data.csv')
big_dep_data <- read_csv('bigdata2/dep_big_data.csv')

big_arr_data$X1 <- NULL

### seperate train and test
set.seed(8888)
train_big <- sample(1:nrow(big_arr_data), 0.8*nrow(big_arr_data))
dd_train_arr <- big_arr_data[train_big, ]
dd_test_arr <- big_arr_data[-train_big, ]


f1 <- as.formula(total_arr_delay ~ .)
fit_linear_arr <- lm(f1, data = dd_train_arr)
summary(fit_linear_arr)
yhat_train_l_arr <- predict(fit_linear_arr, dd_train_arr)
rmse_train_l_arr <- (mean((dd_train_arr$total_arr_delay - yhat_train_l_arr) ^ 2))^0.5
yhat_test_l_arr <- predict(fit_linear_arr, dd_test_arr)
rmse_test_l_arr <- (mean((dd_test_arr$total_arr_delay - yhat_test_l_arr) ^ 2))^0.5


### departure delay 
dd_train_dep <- big_dep_data[train_big, ]
dd_test_dep <- big_dep_data[-train_big, ]


f_dep <- as.formula(total_dep_delay ~ .)
fit_linear_dep <- lm(f_dep, data = dd_train_dep)
summary(fit_linear_dep)
yhat_train_l_dep <- predict(fit_linear_dep, dd_train_dep)
rmse_train_l_dep <- (mean((dd_train_dep$total_dep_delay - yhat_train_l_dep) ^ 2))^0.5
yhat_test_l_dep <- predict(fit_linear_dep, dd_test_dep)
rmse_test_l_dep <- (mean((dd_test_dep$total_dep_delay - yhat_test_l_dep) ^ 2))^0.5


### small data 
## dep delay forward selection 
small_data_arr <- read_csv('bigdata2/arr_small_data.csv')
small_data_arr$X1 <- NULL
# load("bigdata2/forward_selection_dep.RData")
train_small <- sample(1:nrow(small_data_arr), 0.8*nrow(small_data_arr))
train_arr <- small_data_arr[train_small, ]
test_arr <- small_data_arr[-train_small, ]



### forward selection
xnames <- colnames(train_arr)
xnames <- xnames[!xnames %in% 'total_arr_delay']
fit_fw_arr <- lm(total_arr_delay ~ 1, data = train_arr)
yhat_train_arr <- predict(fit_fw_arr, train_arr)
yhat_test_arr <- predict(fit_fw_arr, test_arr)
rmse_train_arr <- (mean((train_arr$total_arr_delay - yhat_train_arr) ^ 2))^0.5
rmse_test_arr <- (mean((test_arr$total_arr_delay - yhat_test_arr) ^ 2)) ^ 0.5
xname <- "intercept"


log_fw_arr <-tibble(xname = xname,
                model = paste0(deparse(fit_fw_arr$call), collapse = ""),
                rmse_train = rmse_train_arr,
                rmse_test = rmse_test_arr,
                rsquare = unlist(summary(fit_fw_arr)[8]))
log_fw_arr

while (length(xnames) > 0) {
  # initialize
  best_rmse_train <- NA
  best_rmse_test <- NA
  best_fit_fw <- NA
  best_xname <- NA
  
  ## select the nest best predictor 
  for (xname in xnames) {
    fit_fw_tmp <- update(fit_fw_arr, as.formula(paste0(". ~ . + ", xname)))
    yhat_train_tmp <- predict(fit_fw_tmp, train_arr)
    rmse_train_tmp <- (mean((train_arr$total_arr_delay - yhat_train_tmp) ^ 2))^0.5
    # compute MSE test
    yhat_test_tmp <- predict(fit_fw_tmp, test_arr)
    rmse_test_tmp <- (mean((test_arr$total_arr_delay - yhat_test_tmp) ^ 2))^0.5
    # if this is the first predictor to be examined,
    # or if this predictors yields a lower MSE that the current
    # best, then store this predictor as the current best predictor
    if (is.na(best_rmse_test) | rmse_test_tmp < best_rmse_test) {
      best_xname <- xname
      best_fit_fw <- fit_fw_tmp
      best_rmse_train <- rmse_train_tmp
      best_rmse_test <- rmse_test_tmp
      adj_rsquare <- unlist(summary(best_fit_fw)[attributes(summary(best_fit_fw))$name == "adj.r.squared"])
    }
  }
  log_fw_arr <- log_fw_arr %>% add_row(xname = best_xname,
                                       model = paste0(deparse(best_fit_fw$call), collapse = ""),
                                       rmse_train = best_rmse_train,
                                       rmse_test = best_rmse_test,
                                       rsquare = adj_rsquare)
  print(log_fw_arr)
  # adopt the best model for the next iteration
  fit_fw_arr <- best_fit_fw
  # remove the current best predictor from the list of predictors
  xnames <- xnames[xnames!=best_xname]
}


## save the result 
save(log_fw_arr, file = "bigdata2/forward_selection_arr.RData")

log_fw_arr$index <- seq(1, nrow(log_fw_arr))
plot(log_fw_arr$index, log_fw_arr$rmse_test, main="Forward Selection" ,type="l", xlab="Number of Variables",
     ylab="RMSE", col = "black", lwd = 2)
lines(log_fw_arr$index, log_fw_arr$rmse_train, col = "blue", type = "l", lwd = 2)
legend("topright",
       c("Test", "Train"),
       fill=c("black", "blue"))
text(log_fw_arr$index[1:20], y = log_fw_arr$rmse_test[1:20], labels = 
       log_fw_arr$xname[1:20], adj = c(1, 1), cex = 0.6)
