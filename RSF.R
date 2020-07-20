set.seed(12345678)
library(randomForestSRC)
library(survival)
library(Hmisc)

# training and testing sets 60/40 split
n <- nrow(IPD.Ohtsu)
train <- sample(1:n, round(n * 3/5))
train_IPD.Ohtsu <- IPD.Ohtsu[train, ]

test <- setdiff(1:n, train)
test_IPD.Ohtsu <- IPD.Ohtsu[test, ]

# create rfsrc object
fit <- rfsrc(Surv(time, event) ~ arm, data = train_IPD.Ohtsu)
fit

plot(fit)

pred <- predict.rfsrc(fit, test_IPD.Ohtsu)
pred

plot.survival(pred)

