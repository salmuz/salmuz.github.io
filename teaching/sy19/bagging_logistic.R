# create data set (author:TD06-SY19)
sample.donut <- function(n1, r1, n2, r2) {
    p <- 2

    R1 <- rnorm(n1, mean = r1)
    angle1 <- runif(n1, 0, 2 * pi)
    X1 <- data.frame(X1 = R1 * cos(angle1), X2 = R1 * sin(angle1))

    R2 <- rnorm(n2, mean = r2)
    angle2 <- runif(n2, 0, 2 * pi)
    X2 <- data.frame(X1 = R2 * cos(angle2), X2 = R2 * sin(angle2))

    cbind(rbind(X1, X2), y = factor(c(rep(1, n1), rep(2, n2))))
}
data <- sample.donut(300, 3, 300, 4)
plot(data$X1, data$X2, col=data$y)

library(glmnet)
# bootstraping + logistic model 
set.seed(43)
nb.classifiers <- 100 # number of classifiers (bagging)
# k-fold cross validation (k=10)
n <- nrow(data)
kfold <- rep((1:10), (n/10)+1)[1:n]
cv.error.avg.prob <- rep(0, 10)
cv.error.voting <- rep(0, 10)
for(k in 1:10){
    # oringal data in the k-fold
    data.orig.train <- data[kfold != k,]
    data.orig.test <- data[kfold == k,]
    classifiers <- list()
    for(c in 1:nb.classifiers){
        # creation new training data set
        idx.data.train <- sample(1:nrow(data.orig.train), replace = TRUE)
        data.train <- data.orig.train[idx.data.train, ]
        transform.poly <- model.matrix( ~ -1 + poly(X1, 2) + poly(X2, 2),
        data=data.train)
        classifiers[[c]] <- glmnet(as.matrix(transform.poly),
        data.train$y,
        family = "multinomial",
        alpha=0, lambda=0)
    }
    # Hit: It is better to calculate the probability/prediction inside above.
    # aggregation/average/voting strategies
    classes <- levels(data.orig.test$y)
    nb.classes <- length(classes)
    nb.test <- nrow(data.orig.test)
    prob.predictions <- array(NA, dim=c(nb.classifiers, nb.test, nb.classes))
    yhat.predictions <- array(NA, dim=c(nb.classifiers, nb.test))
    for(c in 1:nb.classifiers){
        transform.poly <- model.matrix( ~ -1 + poly(X1, 2) + poly(X2, 2),
        data=data.orig.test)
        prob.predictions[c,,] <- predict(classifiers[[c]],
        transform.poly,
        type="response")[,,1]
        yhat.predictions[c,] <- max.col(prob.predictions[c,,])
    }
    # mean probability obtained from the ensemble of classifiers
    avg.prob.bagging <- apply(prob.predictions, c(2, 3) , mean)
    # voting by class
    votting <- function(classifers.yhat){
        vot.by.class <- rep(0, nb.classes)
        # count the time of class 1 and 2, then in the column 1 and 2
        for(class in classes){
            vot.by.class[as.integer(class)] <-
            length(classifers.yhat[classifers.yhat == class])
        }
        return(vot.by.class)
    }
    vot.yhat.bagging <- t(apply(yhat.predictions, 2,
    function(classifers.yhat) votting(classifers.yhat)))

    # aggregation final prediction
    yhat.majority.vote <- max.col(vot.yhat.bagging)
    yhat.avg.probability <- max.col(avg.prob.bagging)
    cv.error.avg.prob[k] <-
    1- sum(diag(table(data.orig.test$y, yhat.avg.probability))/nb.test)
    cv.error.voting[k] <-
    1- sum(diag(table(data.orig.test$y, yhat.majority.vote))/nb.test)
}
boxplot(cv.error.voting, cv.error.avg.prob)
mean(cv.error.voting)
mean(cv.error.avg.prob)

