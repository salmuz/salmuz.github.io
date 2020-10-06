####################################################################################
# Case 0: non-scaling version
set.seed(36)
rm(list = ls())

# reading data
data <- read.table('data/TP4_a19_reg_app.txt')
idx.train <- sample(nrow(data), size=nrow(data)*0.9, replace=F)
validation <- data[-idx.train,]
training <- data[idx.train,]

# modeling with lm
model.fit <- lm(y~., data=training)
summary(model.fit)

# prediction
ypredi <- predict.lm(model.fit, newdata=validation)
mean((validation$y -ypredi)^2)

####################################################################################
# Case 1: scaling only the predictors X
set.seed(36)
rm(list = ls())

# reading data
data <- read.table('data/TP4_a19_reg_app.txt')
idx.train <- sample(nrow(data), size=nrow(data)*0.9, replace=F)
validation <- data[-idx.train,]
training <- data[idx.train,]

# modeling with lm
features  <- paste("+ scale(", names(training)[-101], ")", sep="", collapse = " ")
formule <- paste("y ~ 1 ", features) # -1 if the y is scaled, otherwise 1.
model.fit <- lm(as.formula(formule), data=training)
summary(model.fit)

# prediction
ypredi <- predict.lm(model.fit, newdata=validation)
mean((validation$y -ypredi)^2)

####################################################################################
# Case 2: scaling only the predictors X and the output y
set.seed(36)
rm(list = ls())
# reading data
data <- read.table('data/TP4_a19_reg_app.txt')
idx.train <- sample(nrow(data), size=nrow(data)*0.9, replace=F)
validation <- data[-idx.train,]
training <- data[idx.train,]

# modeling with lm
features  <- paste("+ scale(", names(training)[-101], ")", sep="", collapse = " ")
formule <- paste("scale(y) ~  -1 ", features)
model.fit <- lm(as.formula(formule), data=training)
y.mean <- mean(training[, 101])
y.sd <- sd(training[, 101])
summary(model.fit)

# prediction
ypredi <- predict.lm(model.fit, newdata=validation)
ypredi <- y.sd*ypredi + y.mean
mean((validation$y - ypredi)^2)

####################################################################################
# Case 3: Version with equations (centrée et reduite)
set.seed(36)
rm(list=ls())
data <- read.table('data/TP4_a19_reg_app.txt')
idx.train <- sample(nrow(data), size=nrow(data)*0.9, replace=F)
validation <- data[-idx.train,]
training <- data[idx.train,]

# scaled data
train.scaled  <- scale(training)
X <- as.matrix(train.scaled[, -101])
y <- train.scaled[, 101]
beta.scaled <- solve(t(X)%*%X)%*%t(X)%*%y
x.means <- as.vector(attr(train.scaled,"scaled:center"))[-101]
x.scale <- as.vector(attr(train.scaled,"scaled:scale"))[-101]
y.mean <- as.vector(attr(train.scaled,"scaled:center"))[101]
y.scale <- as.vector(attr(train.scaled,"scaled:scale"))[101]

# prediction
x.mean.train  <- matrix(rep(x.means, nrow(validation)), ncol=100, byrow = T)
x.title <- y.scale*(validation[, -101] - x.mean.train)
ypredi <- t(apply(x.title, 1, "/", x.scale)) %*% beta.scaled + y.mean
mean((validation$y -ypredi)^2)

# wrong
ypredi <- as.matrix(validation[, -101])%*%beta.scaled 
mean((validation$y -ypredi)^2)

####################################################################################
# Case 4: Version with equations (centrée et reduite: les prédicteur)
set.seed(36)
rm(list=ls())
data <- read.table('data/TP4_a19_reg_app.txt')
idx.train <- sample(nrow(data), size=nrow(data)*0.9, replace=F)
validation <- data[-idx.train,]
training <- data[idx.train,]

# scaled data
train.scaled  <- scale(training)
X <- cbind(1, as.matrix(train.scaled[, -101]))
y <- training[, 101]
beta.scaled <- solve(t(X)%*%X)%*%t(X)%*%y
x.means <- as.vector(attr(train.scaled,"scaled:center"))[-101]
x.scale <- as.vector(attr(train.scaled,"scaled:scale"))[-101]

# prediction
x.mean.train  <- matrix(rep(x.means, nrow(validation)), ncol=100, byrow = T)
x.tilde <- validation[, -101] - x.mean.train
ypredi <- cbind(1, t(apply(x.tilde, 1, "/", x.scale))) %*% beta.scaled
mean((validation$y -ypredi)^2)

####################################################################################
# Case 5:  Version with equations (centrée et reduite: les prédicteur et la réponse)
set.seed(36)
rm(list=ls())
data <- read.table('data/TP4_a19_reg_app.txt')
idx.train <- sample(nrow(data), size=nrow(data)*0.9, replace=F)
validation <- data[-idx.train,]
training <- data[idx.train,]

# scaled data
train.scaled  <- scale(training)
X <- as.matrix(train.scaled[, -101])
y <- training[, 101] - mean(training[, 101])
beta.scaled <- solve(t(X)%*%X)%*%t(X)%*%y
x.means <- as.vector(attr(train.scaled,"scaled:center"))[-101]
x.scale <- as.vector(attr(train.scaled,"scaled:scale"))[-101]
y.mean <- mean(training[, 101])

# prediction
x.mean.train  <- matrix(rep(x.means, nrow(validation)), ncol=100, byrow = T)
x.tilde <- validation[, -101] - x.mean.train
ypredi <- t(apply(x.tilde, 1, "/", x.scale)) %*% beta.scaled + y.mean
mean((validation$y -ypredi)^2)

####################################################################################






