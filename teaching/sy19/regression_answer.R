# Reading data
data <- read.table('data/TP4_a19_reg_app.txt')

# Exploration data
boxplot(data[,-101])
library(FactoMineR)
pca <- PCA(data[, -101])
plot(pca$eig[,3], type='l', ylab='cumulative percentage of variance', 
     xlab="components")
abline(h=90, col="red", lwd=2)
abline(h=95, col="blue", lwd=2)

# Regression simple 
library(car)
reg.fit <- lm(y~., data=data)
summary(reg.fit)
plot(data$y, rstandard(reg.fit), ylab='rstandard', main="Studentized Residuals",
     col=ifelse(abs(rstandard(reg.fit))> 2, 'red', 'black'))
abline(h = -2, col="red", lwd=2)
abline(h = 2, col="red", lwd=2)
plot((data$y - fitted(reg.fit))^2, ylab='MSE', main="MSE")

# (1) Outiers 
outlierTest(reg.fit) # Bonferonni p-value for most extreme obs

# (2) Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(data)-length(reg.fit$coefficients)-2))
plot(reg.fit, which=4, cook.levels=cutoff)

# (3) Influence Plot
influencePlot(reg.fit, main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")

# (4) Normality of Residuals
qqPlot(reg.fit, main="QQ Plot") # qq plot for studentized resid
# distribution of studentized residuals
library(MASS)
sresid <- studres(reg.fit)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit <- seq(min(sresid), max(sresid), length=40)
yfit <- dnorm(xfit)
lines(xfit, yfit)

# (5) Evaluate homoscedasticity
#ncvTest(reg.fit) # non-constant error variance test
#spreadLevelPlot(reg.fit) # plot studentized residuals vs. fitted values

# ACP valeur propres
barplot(pca$eig[, 2], main='PCA data set')
barplot(d$eig[, 2], main='PCA Expressions(TD09)')

# function plotting IC (sample)
plot.ic.error <- function(data, x.title="x", ylim.plot=NULL, x.pos=1){
  ic.error.bar <- function(x, lower, upper, length=0.1){ 
    arrows(x, upper, x, lower, angle=90, code=3, length=length, col='red')
  }
  stderr <- function(x) sd(x)/sqrt(length(x))
  # calculer les erreurs moyennes et l'erreur type (standard error)
  means.errs <- mean(data)
  std.errs <- stderr(data)
  .xlim <- c(x.pos-1, x.pos+1)
  if(is.null(ylim.plot)){
    .ylim <- range(means.errs + 1.6*std.errs, means.errs - 1.6*std.errs)
  }else{
    .ylim <- ylim.plot  
  }
  plot(x.pos, means.errs, type='b', ylim = .ylim, xlab=x.title, 
       ylab='CV. error', xlim=.xlim)
  ic.error.bar(x.pos, means.errs - 1.6*std.errs, means.errs + 1.6*std.errs)
}

# Regression Problem
set.seed(379) 
data <- read.table('data/TP4_a19_reg_app.txt')
is.orthogonal <- FALSE
degree <- 2
data <- data[sample(nrow(data)), ]

# creation de groupes pour la validation croisée 
n <-  nrow(data)
group <- rep((1:10), (n/10)+1)[1:n]

# cross-validation lasso
transformation <- function(X, degree=2, orthogonal.poly=TRUE){
  features.poly <- paste("+ poly(", names(X), ", degree=", degree,", 
                          raw=", !orthogonal.poly, ")", sep="", collapse = " ")
  features.log  <- paste("+ log(", names(X), ")", sep="", collapse = " ")
  #features.exp  <- paste("+ exp(", names(X), ")", sep="", collapse = " ")
  features.exp.log <- paste("+ I(exp(", names(X), ")*log(", names(X), "))", sep="", collapse = " ")
  #features <- paste(features.poly, features.log, features.exp, features.exp.log, sep="")
  #features <- paste(features.poly, features.exp, features.log, sep="")
  features <- paste(features.poly, features.exp.log, features.log, sep="")
  formule <- paste("~ -1 ", features)
  X.transform <- model.matrix(as.formula(formule), data = X)
  colnames(X.transform) <- paste("X", 1:ncol(X.transform), sep='')
  attr(X.transform, "assign") <- NULL
  return(X.transform)
}

library(glmnet)
err <- rep(0, 10)
lambdas <- rep(0, 10)
for(i in 1:10){
  index.cv <- which(group==i)
  validation <- data[index.cv,]
  training <- data[-index.cv,]
  # transformations training data
  X <- transformation(training[, -101], degree = degree, 
                      orthogonal.poly=is.orthogonal)
  # lasso model 
  model <- cv.glmnet(X, training$y, alpha = 1 , nfolds = 20)
  # transformation validation data
  X.new <- transformation(validation[, -101], degree = degree, 
                          orthogonal.poly=is.orthogonal)
  yhat <- predict(model, newx=X.new, s = "lambda.1se")
  err[i] <- mean((validation[,101] - yhat)^2)
  lambdas[i] <- model$lambda.1se
}
plot.ic.error(err, ylim.plot = c(min(err), max(err)), x.pos=1.5)
boxplot(err, add=TRUE)
lambda.best <- lambdas[which.min(err)]
X <- transformation(data[,-101], degree = degree, orthogonal.poly=is.orthogonal)
model.best <- glmnet(X, data[,101], alpha = 1, lambda = lambda.best)
plot(glmnet(X, data[,101], alpha = 1))

data.test <- read.table("data/TP4_a19_reg_tst.txt")
X.test <- transformation(data.test[,-101], degree = degree, 
                         orthogonal.poly=is.orthogonal)
yhat <- predict(model.best, newx = X.test)
err.real <- mean((data.test[, 101] - yhat)^2)
abline(h=err.real, col="red", lwd=2)
 
# rm(list = ls())


