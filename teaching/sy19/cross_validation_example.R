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

############################################################
# Nested and Non-nested Cross validation
############################################################
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
  idx.test.inner[[i]] <- list()
  for(j in 1:10){
    index.inner.cv <- which(group.inner==j)
    idx.test.inner[[i]][[j]] <- index.inner.cv
  }
}

################################################################
## Regression
################################################################
library(FNN)
# knn - linearregression
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
      index.inner.cv <- idx.test.inner[[i]][[j]]
      data.train.x <- data.inner[-index.inner.cv, -101]
      data.train.y <- data.inner[-index.inner.cv, 101]
      data.test.x <- data.inner[index.inner.cv, -101]
      data.test.y <- data.inner[index.inner.cv, 101]
      model.reg <- knn.reg(train=data.train.x,
      test=data.test.x,
      y=data.train.y, k=k)
      # on peut calculer la moyenne knn.mse.kmin
      # (par chaque k, mais ils sont proportionnelle)
      #1 - sum(diag(table(data.test.y, model.reg.pred)))/nrow(data.test.y)
      knn.mse.kmin[k] <- knn.mse.kmin[k] +
      mean((model.reg$pred-data.test.y)^2)
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

library(glmnet)
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
    cv.model <- cv.glmnet(X.cv.train, y.cv.train, alpha=alpha,
    #family = "multinomial",
    foldid = group.inner)
    err.lasso.mse[k] <- cv.model$cvm[which(cv.model$lambda ==
    cv.model$lambda.min)]
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
  model.fit <- glmnet(X.train, y.train, lambda = best.lambda,
  #family = "multinomial",
  alpha = best.alpha)
  # classification
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

################################################################
# forward/backward selection variable
err.reg.mse <-  rep(0, 10)
nb.variables.max <- 100
for(i in 1:10){
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,] # training
  data.validation <- data[index.outer.cv,] # validation
  # re-sampling (inner cross validation)
  data.inner <- data.inner[rs.data.inner[[i]], ]
  library(leaps) # library to selection of variables
  cv.errors <- matrix(NA, k, nb.variables.max)
  cv.errors.train <- matrix(NA, k, nb.variables.max)
  for(j in 1:10){ # cross validation interne
    index.inner.cv <- idx.test.inner[[i]][[j]]
    data.train <- data.inner[-index.inner.cv, ]
    data.test <- data.inner[index.inner.cv, ]
    best.models <- regsubsets(y~., data=data.train,
    nvmax=nb.variables.max,
    method="forward") # backward
    for(v in 1:nb.variables.max){
      pred <- predict(best.models, data.test, id=v)
      pred.train <- predict(best.models, data.train, id=v)
      cv.errors[j,v] <- mean((pred-data.test$y)^2)
    }
  }
  # critère (one standard rule, moyenne minimal)
  mse.min.regressors <- which.min(colMeans(cv.errors))
  best.model <- regsubsets(y~., data=data.inner, nvmax = 100,
  method = "forward")
  #coef.best.model <- coef(best.model, mse.min.regressors)
  pred <- predict(best.model, data.validation, id=mse.min.regressors)
  err.reg.mse[i] <- mean((pred - data.validation$y)^2)
}

################################################################
## Classification
################################################################

# reading data
data <- read.table('data/TPN1_a20_clas_app.txt')
# mix up observations randomly
data <- data[sample(nrow(data)), ]
data$y <- as.factor(data$y)

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
  idx.test.inner[[i]] <- list()
  for(j in 1:10){
    index.cv <- which(group.inner==j)
    idx.test.inner[[i]][[j]] <- index.cv
  }
}

err <-  matrix(0, 10, 2)
# regression logistic normal
library(glmnet)
for(i in 1:10){
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,]
  data.validation <- data[index.outer.cv,]
  # re-sampling (inner cross validation)
  data.inner <- data.inner[rs.data.inner[[i]], ]

  # inner cross-valition
  X <- as.matrix(data.inner[, -51])
  y <- as.factor(data.inner[, 51])
  model.log <- glmnet(X, y, family = "multinomial", alpha = 1, lambda = 0)

  # validation
  pred <- predict(model.log, newx=as.matrix(data.validation[, -51]),
  type="response")
  confusion <- table(data.validation[, 51], max.col(pred[,,]))
  err[i, 1] <- 1 - sum(diag(confusion))/nrow(data.validation)
}


# knn classification
library(FNN)
Kmax <- 15
K.sequences <- seq(1, Kmax, by = 2)
for(i in 1:10){
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,]
  data.validation <- data[index.outer.cv,]
  # re-sampling (inner cross validation)
  data.inner <- data.inner[rs.data.inner[[i]], ]
  # cross-validation interne
  knn.mse.kmin <- rep(0, length(K.sequences))
  for(k in 1:length(K.sequences)){
    for(j in 1:10){
      index.inner.cv <- idx.test.inner[[i]][[j]]
      data.train.x <- data.inner[-index.inner.cv, -51]
      data.train.y <- data.inner[-index.inner.cv, 51]
      data.test.x <- data.inner[index.inner.cv, -51]
      data.test.y <- data.inner[index.inner.cv, 51]
      model.reg <- knn(train=data.train.x, test=data.test.x,
      cl=data.train.y, k=k)
      errc <- 1 - sum(diag(table(data.test.y, model.reg)))/nrow(data.inner)
      knn.mse.kmin[k] <- knn.mse.kmin[k] + errc
    }
  }
  idx.kmin <- which(min(knn.mse.kmin) == knn.mse.kmin)
  best.kmin <- K.sequences[idx.kmin]

  # validation our model with best model
  data.train.x <- data.inner[, -51]
  data.train.y <- data.inner[, 51]
  data.test.x <- data.validation[, -51]
  data.test.y <- data.validation[, 51]
  model.reg <- knn(train=data.train.x, test=data.test.x,
  cl=data.train.y, k=best.kmin)
  n <- nrow(data.validation)
  err[i, 2] <- 1 - sum(diag(table(data.test.y, model.reg)))/n
}

