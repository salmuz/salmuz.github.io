# plot interval errors 
plot.cv.error <- function(data, x.title="x"){
  ic.error.bar <- function(x, lower, upper, length=0.1){ 
    arrows(x, upper, x, lower, angle=90, code=3, length=length, col='red')
  }
  stderr <- function(x) sd(x)/sqrt(length(x))
  # calculer les erreurs moyennes et l'erreur type (standard error)
  means.errs <- colMeans(data)
  std.errs <- apply(data, 2, stderr)
  # plotting  
  x.values <- 1:ncol(data)
  plot(x.values, means.errs, 
       ylim = range(means.errs + 1.6*std.errs, means.errs - 1.6*std.errs),
       xlab=x.title, ylab='CV. error')
  ic.error.bar(x.values, means.errs - 1.6*std.errs, means.errs + 1.6*std.errs)
}

# reading data
data <- read.table('data/TPN1_a20_reg_app.txt')
# mix up observations randomly
data <- data[sample(nrow(data.fit)), ]

# creation de groupes pour la validation croisée 
n <-  nrow(data)
group.outer <- rep((1:10), (n/10)+1)[1:n]
idx.test.outer <- list()
idx.test.inner <- list()
rs.data.inner <- list()
for(i in 1:10){
  index.cv <- which(group.outer==i)
  idx.test.outer[[i]] <- index.cv
  n.inner <- n - length(index.cv)
  rs.data.inner[[i]] <- sample(n.inner)
  group.inner <- rep((1:10), (n.inner/10)+1)[1:n.inner]
  for(j in 1:10){
    index.cv <- which(group.inner==j)
    idx.test.inner[[j]] <- index.cv
  }
}


############################################################
# Nested and Non-nested Cross validation
############################################################
library(FNN)
Kmax <- 100
K.sequences <- seq(1, Kmax, by = 2)
err.knn.mse <-  rep(0, 10)
for(i in 1:10){
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,]
  data.validation <- data[index.outer.cv,]
  # re-sampling (inner cross validation)
  data.inner <- data.inner[rs.data.inner[[i]], ]
  knn.mse.kmin <- rep(0, length(K.sequences))
  for(k in 1:length(K.sequences)){
    for(j in 1:10){
      index.inner.cv <- idx.test.inner[[j]]
      data.train.x <- data.inner[-index.inner.cv, -101]
      data.train.y <- data.inner[-index.inner.cv, 101]
      data.test.x <- data.inner[index.inner.cv, -101]
      data.test.y <- data.inner[index.inner.cv, 101]
      model.reg <- knn.reg(train=data.train.x, 
                           test=data.test.x, 
                           y=data.train.y, k=k)
      # on peut calculer la moyenne knn.mse.kmin 
      # (par chaque k, mais ils sont proportionnelle)
      knn.mse.kmin[k] <- knn.mse.kmin[k] + mean((model.reg$pred-data.test.y)^2)
    }
  } 
  idx.kmin <- which(min(knn.mse.kmin) == knn.mse.kmin)
  best.kmin <- K.sequences[idx.kmin]
  
  # validation our model with best model 
  data.train.x <- data.inner[, -101]
  data.train.y <- data.inner[, 101]
  data.test.x <- data.validation[, -101]
  data.test.y <- data.validation[, 101]
  model.reg <- knn.reg(train=data.train.x, 
                       test=data.test.x, 
                       y=data.train.y, k=best.kmin)
  err.knn.mse[i] <- mean((model.reg$pred-data.test.y)^2)
}
plot(knn.mse.kmin/10, type='l', ylab='Cv(k) Error', xlab="k voisins")
boxplot(err.knn.mse)

# regression regularized 
n.alpha <- 25
err.reg.mse <-  rep(0, 10)
for(i in 1:10){
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,]
  data.validation <- data[index.outer.cv,]
  # re-sampling (inner cross validation)
  data.inner <- data.inner[rs.data.inner[[i]], ]
  alphas <- seq(0, 1, length.out =  n.alpha)
  err.lasso.mse <- rep(0, n.alpha)
  best.lambda.alpha <- rep(0, n.alpha)
  for(k in 1:n.alpha){
    alpha <- alphas[k]
    X.cv.train <- as.matrix(data.inner[, -101])
    y.cv.train <- data.inner[, 101]
    cv.model <- cv.glmnet(X.cv.train, y.cv.train, alpha=alpha, foldid = group.inner)
    err.lasso.mse[k] <- cv.model$cvm[which(cv.model$lambda == cv.model$lambda.min)]
    best.lambda.alpha[k] <- cv.model$lambda.min
  }
  idx.best <- which(err.lasso.mse == min(err.lasso.mse))
  best.lambda <- best.lambda.alpha[idx.best]
  best.alpha <- alphas[idx.best]
  # validation model 
  X.train <- as.matrix(data.inner[, -101])
  y.train <- data.inner[, 101]
  X.test <- as.matrix(data.validation[, -101])
  y.test <- data.validation[, 101]
  model.fit <- glmnet(X.train, y.train, lambda = best.lambda, alpha = best.alpha)
  err.reg.mse[i] <- mean((y.test - predict(model.fit, newx=X.test))^2)
}
boxplot(err.reg.mse)

# errors 
errors <- matrix(0, 10, 2)
errors[,1] <- err.knn.mse
errors[,2] <- err.reg.mse
plot.cv.error(errors, x.title = "Modèles")

# boxplot vs IC
par(mfrow=c(1,2))
plot.cv.error(as.matrix(errors[, 2]))
boxplot(err.reg.mse)
