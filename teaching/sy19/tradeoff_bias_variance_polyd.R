# charger le jeu de données 
library(MASS)
attach(Boston)
plot(lstat, medv)

# graine aléatoire 
set.seed(43)
# Regression polynomes d different degrees
n <- nrow(Boston)
kfold <- rep((1:10), (n/10)+1)[1:n]
maxdegree <- 12
cv.error <- matrix(NA, 10, maxdegree)
cv.error.train <- matrix(NA, 10, maxdegree)
Krep <- 20
cv.error.rep <- matrix(NA, Krep, maxdegree)
for(rep in 1:Krep){
  Boston <- Boston[sample(nrow(Boston)),]
  for(d in 1:maxdegree){
    for(k in 1:10){
      data.train <- Boston[kfold != k,]
      data.test <- Boston[kfold == k,]
      fit.poly <- glm(medv ~ poly(lstat, degree=d), 
                      data=data.train)
      ypred <- predict(fit.poly, data.test)
      cv.error[k, d] <-  mean((data.test$medv - ypred)^2)
      cv.error.train[k, d] <- mean((data.train$medv-
                                fit.poly$fitted.values)^2)
    }
    cv.error.rep[rep, d] <- mean(cv.error[, d])
  }
}

# to choose the error to analyse
error.analyse <- cv.error
# plot bias variance tradeoff
library(ggplot2)
err_data <- 1:maxdegree
err_data <- cbind(err_data, colMeans(error.analyse))
stderr <- function(x) sd(x)/sqrt(length(x))
err_data <- cbind(err_data, apply(error.analyse, 2, stderr))
ggplot(as.data.frame(err_data), aes(err_data))+
  geom_line(aes(y = V2)) +
  geom_ribbon(aes(ymin=V2-1.6*V3,ymax=V2+1.6*V3), 
              alpha=0.3) +
  ylab("MSE")+
  xlab("Degree d")

