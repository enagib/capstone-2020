library(readr)
library(tidyverse)

## big data 
dd_unw <- read_csv('bigdata/unwinsor_big_data.csv')

range(dd_unw$total_dep_delay)
qrt_99 <- quantile(dd_unw$total_dep_delay, probs = 0.99) 

dd_unw %>% 
  filter(total_dep_delay >= qrt_99) %>% 
  group_by(op_unique_carrier, origin, dest) %>% 
  summarise(count = n())

dd_unw %>% filter(op_unique_carrier == 'AA', 
                  origin == "ABQ",
                  dest == 'DFW') %>% select(total_dep_delay) %>% summary

a <- dd_unw %>% filter(total_dep_delay <= qrt_99) %>% 
  group_by(op_unique_carrier, origin, dest) %>% 
  summarise(average = mean(total_dep_delay)) %>% ungroup() 

dd_unw2 <- inner_join(dd_unw, a, by = c("op_unique_carrier", "origin", "dest"))

ml_dd <- dd_unw2 %>% mutate(total_dep_delay2 = ifelse(total_dep_delay >= qrt_99, average, total_dep_delay))
ml_dd$total_dep_delay2 %>% range

ml_data3 <- ml_dd %>% select(-c(arr_delay, month, day_of_week, total_dep_delay, average, op_unique_carrier))
train <- sample(1:nrow(ml_data3), 0.8*nrow(ml_data3))
dd_train <- ml_data3[train, ]
dd_test <- ml_data3[-train, ]

f1 <- as.formula(total_dep_delay2 ~ .)
fit_linear <- lm(f1, data = dd_train)
summary(fit_linear)
yhat_train_l <- predict(fit_linear, dd_train)
rmse_train_l <- (mean((dd_train$total_dep_delay2 - yhat_train_l) ^ 2))^0.5


#### do the forward selection with the small data 
small_data_u <- read_csv('bigdata/unwinsor_data.csv')


#### data cleaning
range(small_data_u$total_dep_delay)
qrt_99 <- quantile(small_data_u$total_dep_delay, probs = 0.99) 

small_data_u %>% 
  filter(total_dep_delay >= qrt_99) %>% 
  group_by(op_unique_carrier, origin, dest) %>% 
  summarise(count = n())

small_data_u %>% filter(op_unique_carrier == 'AA', 
                  origin == "ABQ",
                  dest == 'DFW') %>% select(total_dep_delay) %>% summary

a2 <- small_data_u %>% filter(total_dep_delay <= qrt_99) %>% 
  group_by(op_unique_carrier, origin, dest) %>% 
  summarise(average = mean(total_dep_delay)) %>% ungroup() 

dd_small_2 <- inner_join(small_data_u, a2, by = c("op_unique_carrier", "origin", "dest"))

ml_small <- dd_small_2 %>% mutate(total_dep_delay2 = ifelse(tot\al_dep_delay >= qrt_99, average, total_dep_delay))
ml_small$total_dep_delay2 %>% range
hist(ml_small$total_dep_delay2, breaks = 50)

ml_data3 <- ml_small %>% select(-c(origin, dest, arr_delay, month, day_of_week, total_dep_delay, average, op_unique_carrier))
train <- sample(1:nrow(ml_data3), 0.8*nrow(ml_data3))
dd_train <- ml_data3[train, ]
dd_test <- ml_data3[-train, ]

f1 <- as.formula(total_dep_delay2 ~ .)
fit_linear <- lm(f1, data = dd_train)
summary(fit_linear)
yhat_train_l <- predict(fit_linear, dd_train)
rmse_train_l <- (mean((dd_train$total_dep_delay2 - yhat_train_l) ^ 2))^0.5


### forward selection
xnames <- colnames(dd_train)
xnames <- xnames[!xnames %in% 'total_dep_delay2']
fit_fw <- lm(total_dep_delay2 ~ 1, data = dd_train)
yhat_train <- predict(fit_fw, dd_train)
yhat_test <- predict(fit_fw, dd_test)
rmse_train <- (mean((dd_train$total_dep_delay2 - yhat_train) ^ 2))^0.5
rmse_test <- (mean((dd_test$total_dep_delay2 - yhat_test) ^ 2)) ^ 0.5
xname <- "intercept"


log_fw <-tibble(xname = xname,
                model = paste0(deparse(fit_fw$call), collapse = ""),
                rmse_train = rmse_train,
                rmse_test = rmse_test,
                rsquare = unlist(summary(fit_fw)[8]))
log_fw

while (length(xnames) > 0) {
  # initialize
  best_rmse_train <- NA
  best_rmse_test <- NA
  best_fit_fw <- NA
  best_xname <- NA
  
  ## select the nest best predictor 
  for (xname in xnames) {
    fit_fw_tmp <- update(fit_fw, as.formula(paste0(". ~ . + ", xname)))
    yhat_train_tmp <- predict(fit_fw_tmp, dd_train)
    rmse_train_tmp <- (mean((dd_train$total_dep_delay2 - yhat_train_tmp) ^ 2))^0.5
    # compute MSE test
    yhat_test_tmp <- predict(fit_fw_tmp, dd_test)
    rmse_test_tmp <- (mean((dd_test$total_dep_delay2 - yhat_test_tmp) ^ 2))^0.5
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
  log_fw <- log_fw %>% add_row(xname = best_xname,
                               model = paste0(deparse(best_fit_fw$call), collapse = ""),
                               rmse_train = best_rmse_train,
                               rmse_test = best_rmse_test,
                               rsquare = adj_rsquare)
  print(log_fw)
  # adopt the best model for the next iteration
  fit_fw <- best_fit_fw
  # remove the current best predictor from the list of predictors
  xnames <- xnames[xnames!=best_xname]
}

## save the result 
save(log_fw, file = "bigdata/forward_selection.RData")

## result plot 
log_fw$index <- seq(1, nrow(log_fw), by = 1)
# ggplot(log_fw) +
#   geom_line(aes(index, rmse_test), color = 'black') +
#   geom_line(aes(index, rmse_train), color = 'blue')
plot(log_fw$index, log_fw$rmse_test, main="Forward Selection" ,type="l", xlab="Number of Variables",
     ylab="RMSE", col = "black", lwd = 2)
lines(log_fw$index, log_fw$rmse_train, col = "blue", type = "l", lwd = 2)
legend("topright",
       c("Test", "Train"),
       fill=c("black", "blue"))
text(log_fw$index[1:20], y = log_fw$rmse_test[1:20], labels = 
       log_fw$xname[1:20], adj = c(1, 1), cex = 0.6)

### Try Lasso
library(glmnet)
x = as.matrix(dd_train %>% select(-c(total_dep_delay2)))
y = dd_train$total_dep_delay2

fit_lasso <- cv.glmnet(x, y, family="gaussian", lambda = seq(0.1, 1, by = 0.1),
                       type.measure = 'mse')

results_coef <- coef(fit_lasso)
fit_lasso$lambda
fit_lasso$lambda.1se
## store results 
save(fit_lasso, results_coef, fit_lasso2, coef_results, file = "bigdata/fit_lasso.RData")

## best result lambda = 0.3
 
fit_lasso2 <- cv.glmnet(x, y, family="gaussian", alpha = 1)

yhat_train_lasso <- predict(fit_lasso2, x)
yhat_test_lasso <- predict(fit_fw, dd_test)
rmse_train_lasso <- (mean((dd_train$total_dep_delay2 - yhat_train) ^ 2))^0.5
rmse_test_lasso <- (mean((dd_test$total_dep_delay2 - yhat_test) ^ 2)) ^ 0.5

coef_results <- coef(fit_lasso2)
