library(car)
# linear simulation + outlier
x <- runif(100)
y <- 5*x + 7 + rnorm(100, sd = 0.3)
# outlier points
x <- c(x, 0.7, 0.8)
y <- c(y, 7, 6)
plot(x, y, main="Fitted model")
fit.linear <- lm(y~x)
summary(fit.linear)
abline(fit.linear$coefficients[1], fit.linear$coefficients[2], col="red")
plot(y, rstandard(fit.linear), ylab='rstandard', main="Studentized Residuals")
plot((y-fitted(fit.linear))^2, ylab='MSE', xlab="prediction", main="MSE")
influencePlot(fit.linear, main="Cook's distance & Studentized Residuals")
# cook distance
cutoff <- 4/((100-length(fit.linear$coefficients)-2))
plot(fit.linear, which=4, cook.levels=cutoff)
# cook distance to manually
iobs <- 102
fit.linear_minus_iobs <-  lm(y[-iobs]~x[-iobs])
sum(fitted(fit.linear)[-iobs] - fitted(fit.linear_minus_iobs))^2/(sum(fit.linear$residuals^2)*100/(100-1))

# non-linear simulation 
x <- runif(100, min= 0, max=10)
y <- 5*sin(x) + 4 + rnorm(100)
plot(x, y, main="Fitted model")
fit.nonlinear <- lm(y~x)
summary(fit.nonlinear)
abline(fit.nonlinear$coefficients[1], fit.nonlinear$coefficients[2], col="red")
plot(y, rstandard(fit.nonlinear), ylab='rstandard', main="Studentized Residuals")
plot((y-fitted(fit.nonlinear))^2, ylab='MSE', xlab="prediction", main="MSE")
influencePlot(fit.nonlinear, main="Cook's distance & Studentized Residuals")
# cook distance
cutoff <- 4/((100-length(fit.nonlinear$coefficients)-2))
plot(fit.nonlinear, which=4, cook.levels=cutoff)


# splines fitting models 
library(splines)
fit.splines <- lm(y~bs(x, df=10))
pred <- predict(fit.splines, newdata=as.data.frame(x), interval="c")
ids <- sort(x, index.return=TRUE)
plot(x, y)
lines(x[ids$ix], pred[ids$ix, 'fit'], col="red")

