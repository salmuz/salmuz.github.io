#charger les données.
data <- read.table('data/spambase.dat')
barplot(table(data$V58))
names(data)[58] <- 'Y'
data$Y <- as.factor(data$Y)

#graine alétoire.
set.seed(143) 
# with number 1473, we get a error qda 
# because of some preditors are linearly independent
# so matrix covariance-variance is not inversible.
n <- nrow(data)
train <- sample(n, n*2/3)
spam.train <- data[train, ]
spam.test <- data[-train, ]

# loading libraries 
library(MASS) # lda, qda
library(naivebayes) # naive_bayes
library(FNN) # knn(train, test, cl, k)
library(pROC) # proc
# glm -> logistic  = glm(..., family=binomial())

########################################################################
# model LDA
lda.model <- lda(Y~., data=spam.train)
lda.pred <- predict(lda.model, newdata=spam.test)
table(spam.test$Y, lda.pred$class) #matrice de confusion
1 - sum(diag(table(spam.test$Y, lda.pred$class)))/nrow(spam.test)
#0.1134289
plot(roc(spam.test$Y, as.vector(lda.pred$posterior[, 1])))

########################################################################
# model QDA
qda.model <- qda(Y~., data=spam.train)
qda.pred <- predict(qda.model, newdata=spam.test)
1 - sum(diag(table(spam.test$Y, qda.pred$class)))/nrow(spam.test)
#0.1662321
plot(roc(spam.test$Y, as.vector(qda.pred$posterior[, 1])), add=T, col='red')

########################################################################
# model Knn-classification
knn.model <- knn(train=spam.train[, -58],
                 test=spam.test[, -58],
                 cl=spam.train[,58],
                 k=15,
                 prob=TRUE)
matrix.conf.knn <- table(spam.test$Y, knn.model)
1 - sum(diag(matrix.conf.knn))/nrow(spam.test)
#0.2372881
knn.posterior <- matrix(0, nrow=nrow(spam.test), ncol=2)
knn.probByclass <- attr(knn.model, "prob")
for(i in 1:nrow(spam.test)){
  prob <- knn.probByclass[i]
  knn.posterior[i, 1] <- ifelse(knn.model[i] == "0", prob, 1-prob)
  knn.posterior[i, 2] <- 1-knn.posterior[i, 1]
}
plot(roc(spam.test$Y, knn.posterior[,1]), add=T, col='green')

########################################################################
# model naive bayes with gaussian ditribution
nb.model <- naive_bayes(Y~., data=spam.train)
nb.pred <- predict(nb.model, newdata = spam.test, type="class")
nb.prob <- predict(nb.model, newdata = spam.test[,-58], type="prob")
matrix.conf.nb <- table(spam.test$Y, nb.pred)
1 - sum(diag(matrix.conf.nb))/nrow(spam.test)
# 0.2998696
plot(roc(spam.test$Y, as.vector(nb.prob[, 1])), add=T, col='blue')

TPR <- matrix.conf.nb[2,2]/rowSums(matrix.conf.nb)[2]
FPR <- matrix.conf.nb[1,2]/rowSums(matrix.conf.nb)[1] 
points(1-FPR, TPR, col="red", pch = 19)

########################################################################
# model logistic regression
log.model <- glm(Y~., data=spam.train, family = binomial())
log.pred <- predict(log.model, newdata = spam.test)
1 - sum(diag(table(spam.test$Y, log.pred>0.5)))/nrow(spam.test)
# 0.08865711
plot(roc(spam.test$Y, log.pred), add=T, col='violet')

########################################################################
legend("bottomright", lty = 1, 
       legend=c("LDA", "QDA", "Knn", "NB", "LR"), 
       col=c("black", "red", "green", "blue", "violet"))




