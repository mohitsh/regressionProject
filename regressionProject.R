
data(mtcars)
library(plyr)
library(dplyr)
library(reshape2)


pairs(mtcars, panel=panel.smooth, main="mtcars data", col=3+(mtcars$am==1))
mtcars[,9] <- as.factor(mtcars[,9])

##### analysis based on "am" only
data(mtcars)
data <-  mtcars[,c("mpg","am")]
data[,"am"] <-  as.factor(data[,"am"])

attach(data)
summary(am)
summary(mpg)
summary(mpg[am == 1])
summary(mpg[am == 0])

boxplot(list(manual=mpg[am==1], automatic=mpg[am==0]) )
plot(am, mpg)
fit <- lm(mpg~am)
summary(fit)
par(mfrow=c(2,2))
plot(fit)

# plot(predict(fit), resid(fit))
# Because the data were coded as 0/1, the coefficient is directly
# interpreted as the manual (1) - auto (0) difference. 
# In other words, manual on average were 7.245
# points greater than auto, a significant difference.

##### analysis based on all the covariates

data(mtcars); par(mfrow=c(2,2))
fit1 <- lm(mpg~., data=mtcars)
sqrt(vif(fit1))
plot(fit1)

fit10 <- lm(mpg~ am, data=mtcars)
fit9 <- update(fit10, mpg~  cyl + am, data=mtcars)
fit8 <- update(fit10, mpg~  cyl + disp + am, data=mtcars)
fit7 <- update(fit10, mpg~  cyl + disp + hp + am, data=mtcars)
fit6 <- update(fit10, mpg~  cyl + disp + hp + drat + am, data=mtcars)
fit5 <- update(fit10, mpg~  cyl + disp + hp + drat + wt + am, data=mtcars)
fit4 <- update(fit10, mpg~  cyl + disp + hp + drat + wt + qsec + am , data=mtcars)
fit3 <- update(fit10, mpg~  cyl + disp + hp + drat + wt + qsec + vs+ am , data=mtcars)
fit2 <- update(fit10, mpg~  cyl + disp + hp + drat + wt + qsec + vs+ am + gear, data=mtcars)
fit1 <- update(fit10, mpg~ cyl + disp + hp + drat + wt + qsec + vs+ am + gear + carb, data=mtcars)





