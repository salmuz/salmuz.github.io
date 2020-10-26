# read data
data <- read.table("data/TPN1_a20_clas_app.txt")

# convertir to factor
data$y <- as.factor(data$y)
id.train <- sample(nrow(data), nrow(data)*0.6)

######################################################
# Logistic regression (one-vs-all)
######################################################
# partitions training and test data sets
data.train <- data[id.train,]
data.test <- data[-id.train,]

### One-vs-all
###### 1 vs. all
data.onevsall <- data.train
data.onevsall$y <- as.numeric(as.character(data.onevsall$y))
data.onevsall$y[data.onevsall$y %in% c(2, 3)] <- 0
data.onevsall$y <- as.factor(data.onevsall$y)
model.one <- glm(y~., data=data.onevsall, family = "binomial")
###### 2 vs. all
data.onevsall <- data.train
data.onevsall$y <- as.numeric(as.character(data.onevsall$y))
data.onevsall$y[data.onevsall$y %in% c(1, 3)] <- 0
data.onevsall$y[data.onevsall$y == 2] <- 1
data.onevsall$y <- as.factor(data.onevsall$y)
model.two <- glm(y~., data=data.onevsall, family = binomial())
###### 3 vs. all
data.onevsall <- data.train
data.onevsall$y <- as.numeric(as.character(data.onevsall$y))
data.onevsall$y[data.onevsall$y %in% c(1, 2)] <- 0
data.onevsall$y[data.onevsall$y == 3] <- 1
data.onevsall$y <- as.factor(data.onevsall$y)
model.three <- glm(y~., data=data.onevsall, family = binomial())

## prob. pred one-vs-alls
pred <- cbind(predict(model.one, newdata=data.test, type = "response"),
              predict(model.two, newdata=data.test, type = "response"),
              predict(model.three, newdata=data.test, type = "response"))
1-sum(diag(table(data.test$y, max.col(pred))))/nrow(data.test)
