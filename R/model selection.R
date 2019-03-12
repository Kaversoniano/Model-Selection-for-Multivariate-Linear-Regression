
### read-in data
cdi_train <- read.csv("F:/R/200B/Project 2/cdi_train.csv")
cdi_test <- read.csv("F:/R/200B/Project 2/cdi_test.csv")

### null model and full model
null <- lm(crimesper1000 ~ 1, cdi_train)
full <- lm(crimesper1000 ~ ., cdi_train)

### component-plus-residual plot
library(car)
crPlots(full)

### 1 - personal judgement
fit1 <- lm(crimesper1000 ~ pop18 + beds + hsgrad + bagrad + poverty +
             unemp + pcincome + region2 + region3 + region4, cdi_train)
summary(fit1)
round(summary(fit1)$coefficients[, 1], 4)

### 2 - best subset selection
library(leaps)
library(bestglm)
fit2 <- bestglm(cdi_train[, c(2:15, 1)], IC = "AIC") # 9*: 4 votes, 7*: 2 votes
fit2$Subsets
fit2$Subsets[which(fit2$Subsets$AIC == min(fit2$Subsets$AIC)),]

fit2 <- lm(crimesper1000 ~ pop18 + docs + beds + poverty + pcincome + totalinc +
             region2 + region3 + region4, cdi_train)
summary(fit2)
round(summary(fit2)$coefficients[, 1], 4)

### 3 - forward selection
step(null, scope = list(upper = full, lower = null),
     direction = "forward", trace = TRUE)

fit3 <- lm(crimesper1000 ~ poverty + beds + region3 + bagrad + docs +
             region4 + pcincome + pop18 + region2 + totalinc, data = cdi_train)
summary(fit3)
round(summary(fit3)$coefficients[, 1], 4)

### 4 - backward selection
step(full, scope = list(upper = full, lower = null),
     direction = "backward", trace = TRUE)

fit4 <- lm(crimesper1000 ~ pop18 + docs + beds + poverty + pcincome +
             totalinc + region2 + region3 + region4, data = cdi_train)
summary(fit4)
round(summary(fit4)$coefficients[, 1], 4)

### 5 - stepwise selection
step(null, scope = list(upper = full, lower = null),
     direction = "both", trace = TRUE)

fit5 <- lm(crimesper1000 ~ poverty + beds + region3 + docs + 
             region4 + pcincome + pop18 + region2 + totalinc, data = cdi_train)
summary(fit5)
round(summary(fit5)$coefficients[, 1], 4)

### 6 - Lasso
library(glmnet)
set.seed(1234)
cv.lasso <- cv.glmnet(x = data.matrix(cdi_train[, -1]),
                      y = data.matrix(cdi_train$crimesper1000),
                      family = "gaussian", alpha = 1)
plot(cv.lasso)
cv.lasso$lambda.min # 0.01373692 - different every time of run, due to random sampling during cross-validation

fit6 <- glmnet(x = data.matrix(cdi_train[, -1]),
               y = data.matrix(cdi_train$crimesper1000),
               family = "gaussian", alpha = 1, lambda = 0.01373692)
round(coef(fit6), 4)

### 7 - bivariate p-value treshhold method
p.value <- rep(NA, 14)
vars <- names(cdi_train)[-1]
f <- paste("crimesper1000 ~ ", vars, sep = "")

for (k in 1:14) {
  fit0 <- lm(f[k], data = cdi_train)
  p.value[k] <- round(summary(fit0)$coefficient[2,4], 4)
}

vars[p.value < 0.1] # set 0.1 as threshold

fit7 <- lm(crimesper1000 ~ pop18 + docs + beds + hsgrad + poverty +
             totalinc + region2 + region3, data = cdi_train)
summary(fit7)
round(summary(fit7)$coefficients[, 1], 4)


