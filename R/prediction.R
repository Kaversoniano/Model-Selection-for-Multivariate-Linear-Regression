
### continued from 'model selection.R' -----

### RMSE
RMSE <- function(pred, true) {
  n <- length(true)
  rmse <- sqrt(sum((pred - true)^2)/n)
  return(rmse)
}

### prediction & evaluation
pred1 <- predict(fit1, cdi_test[, -1])
RMSE(pred1, cdi_test$crimesper1000) # RMSE = 17.22893
pred1 <- predict(fit1, cdi_train[, -1])
RMSE(pred1, cdi_train$crimesper1000) # RMSE = 20.8649

pred2 <- predict(fit2, cdi_test[, -1])
RMSE(pred2, cdi_test$crimesper1000) # RMSE = 19.14319
pred2 <- predict(fit2, cdi_train[, -1])
RMSE(pred2, cdi_train$crimesper1000) # RMSE = 19.54766

pred3 <- predict(fit3, cdi_test[, -1])
RMSE(pred3, cdi_test$crimesper1000) # RMSE = 19.10019
pred3 <- predict(fit3, cdi_train[, -1])
RMSE(pred3, cdi_train$crimesper1000) # RMSE = 19.54265

pred4 <- predict(fit4, cdi_test[, -1])
RMSE(pred4, cdi_test$crimesper1000) # RMSE = 19.14319
pred4 <- predict(fit4, cdi_train[, -1])
RMSE(pred4, cdi_train$crimesper1000) # RMSE = 19.54766

pred5 <- predict(fit5, cdi_test[, -1])
RMSE(pred5, cdi_test$crimesper1000) # RMSE = 19.14319
pred5 <- predict(fit5, cdi_train[, -1])
RMSE(pred5, cdi_train$crimesper1000) # RMSE = 19.54766

pred6 <- predict(fit6, data.matrix(cdi_test[, -1]))
RMSE(pred6, cdi_test$crimesper1000) # RMSE = 19.057
pred6 <- predict(fit6, data.matrix(cdi_train[, -1]))
RMSE(pred6, cdi_train$crimesper1000) # RMSE = 19.52636

pred7 <- predict(fit7, cdi_test[, -1])
RMSE(pred7, cdi_test$crimesper1000) # RMSE = 20.023
pred7 <- predict(fit7, cdi_train[, -1])
RMSE(pred7, cdi_train$crimesper1000) # RMSE = 21.00896


