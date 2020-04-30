
small_data_dep <- read_csv('bigdata2/dep_small_data.csv')
train_small <- sample(1:nrow(small_data_dep), 0.8*nrow(small_data_dep))
train_dep <- small_data_dep[train_small, ]
test_dep <- small_data_dep[-train_small, ]



### forward selection
xnames <- colnames(train_dep)
xnames <- xnames[!xnames %in% 'total_dep_delay']
fit_fw <- lm(total_dep_delay ~ 1, data = train_dep)
yhat_train <- predict(fit_fw, train_dep)
yhat_test <- predict(fit_fw, test_dep)
rmse_train <- (mean((train_dep$total_dep_delay - yhat_train) ^ 2))^0.5
rmse_test <- (mean((test_dep$total_dep_delay - yhat_test) ^ 2)) ^ 0.5
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
    yhat_train_tmp <- predict(fit_fw_tmp, train_dep)
    rmse_train_tmp <- (mean((train_dep$total_dep_delay - yhat_train_tmp) ^ 2))^0.5
    # compute MSE test
    yhat_test_tmp <- predict(fit_fw_tmp, test_dep)
    rmse_test_tmp <- (mean((test_dep$total_dep_delay - yhat_test_tmp) ^ 2))^0.5
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
save(train_small, file = 'bigdata2/train_small.RData')
save(log_fw, file = "bigdata2/forward_selection_dep.RData")

## result plot 
log_fw$index <- seq(1, nrow(log_fw), by = 1)
# ggplot(log_fw) +
#   geom_line(aes(index, rmse_test), color = 'black') +
#   geom_line(aes(index, rmse_train), color = 'blue')
plot(log_fw$index, log_fw$rmse_test, main="Forward Selection" ,type="l", xlab="Number of Variables",
     ylab="RMSE", col = "black", lwd = 2, ylim = c(25, 26.5))
lines(log_fw$index, log_fw$rmse_train, col = "blue", type = "l", lwd = 2)
legend("topright",
       c("Test", "Train"),
       fill=c("black", "blue"))
text(log_fw$index[1:20], y = log_fw$rmse_test[1:20], labels = 
       log_fw$xname[1:20], adj = c(1, 1), cex = 0.6)