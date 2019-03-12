
### read-in data
cdi <- read.csv("F:/R/200B/Project 2/cdi.csv")
vars <- c("crimes", "area", "pop", "pop18", "pop65", "docs", "beds", "hsgrad",
          "bagrad", "poverty", "unemp", "pcincome", "totalinc","region")
cdi <- cdi[, vars] # variables of interest


### dataset partition
set.seed(1234)
trainID <- sample(1:440, size = round(0.75 * 440)) # n = 330
testID <- setdiff(1:440, trainID) # n = 110

cdi_train <- cdi[trainID,] # training set
cdi_test <- cdi[testID,] # test set


### summary statistics
# categorical - region
rbind(table(cdi_train$region),
      round(100*prop.table(table(cdi_train$region)), 2))
rbind(table(cdi_test$region),
      round(100*prop.table(table(cdi_test$region)), 2))

# numerical
SummaryStats <- function(data) {
  data <- data[, -14]
  M <- round(apply(data, 2, mean), 2)
  SD <- round(apply(data, 2, sd), 2)
  M.SD <- paste(M, "¡À", SD, sep = "")
  names(M.SD) <- names(data)
  return(M.SD)
}

SummaryStats(cdi_train)
SummaryStats(cdi_test)


### dummy 'region' variable - setting 'northeast' (region = 1) as reference
# dummy training set
attach(cdi_train)
cdi_train$region2 <- (region == 2)*1
cdi_train$region3 <- (region == 3)*1
cdi_train$region4 <- (region == 4)*1
cdi_train <- cdi_train[, -14]

cdi_train$crimes <- crimes/pop*1000
names(cdi_train)[1] <- "crimesper1000" # the number of serious crimes per 1000 persons

cdi_train <- cdi_train[, -3]
detach(cdi_train)

# dummy test set
attach(cdi_test)
cdi_test$region2 <- (region == 2)*1
cdi_test$region3 <- (region == 3)*1
cdi_test$region4 <- (region == 4)*1
cdi_test <- cdi_test[, -14]

cdi_test$crimes <- crimes/pop*1000
names(cdi_test)[1] <- "crimesper1000" # the number of serious crimes per 1000 persons

cdi_test <- cdi_test[, -3]
detach(cdi_test)


### write-out data
write.csv(cdi_train, "F:/R/200B/Project 2/cdi_train.csv", row.names = FALSE)
write.csv(cdi_test, "F:/R/200B/Project 2/cdi_test.csv", row.names = FALSE)


### standardization
for (k in 2:12) {
  mu <- mean(cdi_train[,k]) # hyperparameter: sample mean
  std <- sd(cdi_train[,k]) # hyperparameter: sample standard deviation
  cdi_train[,k] <- (cdi_train[,k] - mu) / std
  cdi_test[,k] <- (cdi_test[,k] - mu) / std
}


### write-out standardized data
write.csv(cdi_train, "F:/R/200B/Project 2/cdi_train0.csv", row.names = FALSE)
write.csv(cdi_test, "F:/R/200B/Project 2/cdi_test0.csv", row.names = FALSE)


