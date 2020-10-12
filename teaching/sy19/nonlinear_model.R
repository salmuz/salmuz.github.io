########################################
# Generation de data sets 
########################################
set.seed(67)
N <- 150
x <- runif(N, min= 0, max=10)
# nonlinear data
y <- 4 + 5*sin(x) + rnorm(N) 
plot(x, y)
data.nolinear <- data.frame(y=y, x=x)
# linear data
y <- 4 + 5*x + rnorm(N, sd=3) 
data.linear <- data.frame(y=y, x=x)
plot(x, y)

########################################
# linear regression analysis
par(mar = c(4, 4, 1.2, 0.5))
par(mfrow = c(2, 2))
########################################
library(car)
fit.linear <- lm(y~., data=data.linear)
plot(data.linear$x, data.linear$y, main="Fitted model")
abline(fit.linear$coeff[1], fit.linear$coeff[2], col="red", lwd=2)
plot(data.linear$y, rstandard(fit.linear), ylab='rstandard', main="Studentized-Residu.")
plot((data.linear$y-fitted(fit.linear))^2,ylab='MSE', xlab="prediction", main="MSE")
influencePlot(fit.linear, main="Cook's distance & Studentized Residuals")

fit.nonlinear <- lm(y~., data=data.nolinear)
plot(data.nolinear$x, data.nolinear$y, main="Fitted model")
abline(fit.nonlinear$coeff[1], fit.nonlinear$coeff[2], col="red", lwd=2)
plot(data.nolinear$y, rstandard(fit.nonlinear), ylab='rstandard', main="Studentized-Residu.")
plot((data.nolinear$y-fitted(fit.nonlinear))^2,ylab='MSE', xlab="prediction", main="MSE")
influencePlot(fit.nonlinear, main="Cook's distance & Studentized Residuals")

########################################
# creation ortho-polynomial 
fit.nonlinear <- lm(y~ 1 + poly(x, 6, raw=T), data=data.nolinear)
ids <- sort(data.nolinear$x, index.return=TRUE)
plot(data.nolinear$x, data.nolinear$y, main="Fitted model")
lines(data.nolinear$x[ids$ix],fitted.values(fit.nonlinear)[ids$ix], 
      main="Fitted model", col="red", lwd=2)
plot(data.nolinear$y, rstandard(fit.nonlinear), ylab='rstandard', main="Studentized-Residu.")
plot((data.nolinear$y-fitted(fit.nonlinear))^2,ylab='MSE', xlab="prediction", main="MSE")
influencePlot(fit.nonlinear, main="Cook's distance & Studentized Residuals")

