# https://michael.hahsler.net/SMU/EMIS7332/R/viz_classifier.html
# http://www.cmap.polytechnique.fr/~lepennec/R/Learning/Learning.html
# Soit ((X1, X2), Y ) la distribution jointe de données dans le plan ainsi que leur classe associée (Y ∈ {0, 1}). 
#     X1 |(Y =0)=R1cosθ1          X1 |(Y =1)=R2cosθ2 
#     X2 |(Y =0)=R1sinθ1          X2 |(Y =1)=R2sinθ2
# avec θ1,θ2 ∼ U(0,2π), R1 ∼ N(r1,1), R2 ∼ N(r2,1) et Pr(Y = 1) = Pr(Y = 0) = 1/2.
sample.donut <- function(n1, r1, n2, r2) {
  p <- 2
  R1 <- rnorm(n1, mean = r1)
  angle1 <- runif(n1, 0, 2 * pi)
  group1 <- data.frame(X1 = R1 * cos(angle1), 
                       X2 = R1 * sin(angle1))
  
  R2 <- rnorm(n2, mean = r2)
  angle2 <- runif(n2, 0, 2 * pi)
  group2 <- data.frame(X1 = R2 * cos(angle2), 
                       X2 = R2 * sin(angle2))
  
  cbind(rbind(group1, group2), 
        y = factor(c(rep(1, n1), rep(2, n2))))
}

decision.boundary <- function(X1.grid, X2.grid, 
                              prob.prediction, 
                              color="blue"){
  contour(x=X1.grid, y=X2.grid, 
          z=matrix(prob.prediction, 
                   nrow=length(X1.grid)), 
          levels=0.5, col=color, drawlabels=FALSE, 
          lwd=2, add = TRUE)
}

library(MASS)
library(glmnet)
# Grid new data 
X1.grid <- seq(-6, 6, length.out = 100)
X2.grid <- seq(-6, 6, length.out = 100)
grid.newdata <- expand.grid(X1=X1.grid, X2=X2.grid)
r1 <- 3
r2 <- 4

# Question (1): Compléter la fonction sample.donut du fichier src/dataset.R qui génère un jeu de données suivant le modèle précédent. Afficher le jeu de données obtenu pour r1 = 3 et r2 = 4. Quelle semble être la frontière de décision optimale ?
data <- sample.donut(100, r1, 100, r2)
plot(data$X1, data$X2, col=data$y)

# Question (2): Est-ce qu'une analyse discriminant linéaire convient pour discriminer les deux classes
data.train <- sample.donut(100, r1, 100, r2)
data.test <- sample.donut(10^4, r1, 10^4, r2)
plot(data.train$X1, data.train$X2, col=data.train$y)

# lda model
fit.lda <- lda(y~., data=data.train)
prob.pred <- predict(fit.lda, grid.newdata)$posterior[, 1]
decision.boundary(X1.grid, X2.grid, prob.pred)

# qda model
fit.qda <- qda(y~., data=data.train)
prob.pred <- predict(fit.qda, grid.newdata)$posterior[, 1]
decision.boundary(X1.grid, X2.grid, prob.pred, color="red")

# logistic 
fit.log <- cv.glmnet(as.matrix(data.train[, -3]), 
                     as.integer(data.train$y), 
                     family="binomial", 
                     type.measure="class")
prob.pred <- predict(fit.log,newx=as.matrix(grid.newdata), 
                     s="lambda.min", type="response")
decision.boundary(X1.grid, X2.grid, prob.pred, color="green")

# class prediction
class.pred <- predict(fit.lda, data.test)$class
acc.lda <- mean(class.pred == data.test$y)
class.pred <- predict(fit.qda, data.test)$class
acc.qda <- mean(class.pred == data.test$y)
class.pred <- predict(fit.log, 
                      newx=as.matrix(data.test[,-3]), 
                      s="lambda.min", type="class")
acc.log <- mean(class.pred == data.test$y)
# mean accuracy models
acc.base <- cbind(acc.lda, acc.qda, acc.log)
acc.base

# Question (3): Refaites une analyse discriminante linéaire en rajoutant des descripteurs qui vont aider à la discrimination.
fit.lda.poly <- lda(y ~ poly(X1, degree=2) + 
                        poly(X2, degree=2), 
                    data=data.train)
plot(data.train$X1, data.train$X2, col=data.train$y)
pred.lda.poly <- predict(fit.lda.poly, 
                         newdata = grid.newdata)
decision.boundary(X1.grid, X2.grid, 
                  pred.lda.poly$posterior[, 1])

# fit.glm.poly <- glm(y ~ poly(X1, degree=2) + 
#                       poly(X2, degree=2),
#                     data=data.train, 
#                     family = "binomial")
# fit.glm.ns3 <- glm(y ~ bs(X1, df=6, degree = 5) + 
#                      bs(X2, df=6, , degree = 5),
#                    data=data.train, 
#                    family = "binomial")

# class
class.lda.poly <- predict(fit.lda.poly, 
                          newdata = data.test)$class
acc.lda.poly <- mean(class.lda.poly == data.test$y)
acc.lda.poly

# Question (4): Utiliser un modèle linéaire généralisé avec des splines lissées. On utilisera la fonction gam de la bibliothèque mgcv.
library(mgcv)
fit.gam <- gam(y ~ s(X1) + s(X2), 
               data=data.train, 
               family = "binomial")
prob.pred  <- predict(fit.gam, 
                      newdata = grid.newdata, 
                      type="response")
decision.boundary(X1.grid, X2.grid, 
                  prob.pred, color = "yellow")
# accuracy gam model ### mean(factor(as.numeric(...prediction...> 0) + 1)==data.test$y)
prob.pred  <- predict(fit.gam, newdata = 
                        data.test, 
                      type="response")
class.pred <- ifelse(prob.pred > 0.5, 2, 1)
acc.lda.gam <- mean(class.pred == data.test$y)
acc.lda.gam

# Question (5): Visualiser la frontière de décision avec la fonction vis.gam
vis.gam(fit.gam, type="response", 
        plot.type="contour")
points(data.train$X1, data.train$X2, 
       col=data.train$y)


# Question (6): Retrouver le caractère quadratique des fonctions apprises sur chacun des prédicteurs.
plot(fit.gam, select=1)
plot(fit.gam, select=2)

# Question (7): Re-tester l’analyse discriminante linéaire et le modèle additif généralisé avec peu de données. Que se passe-t-il ?
data.test <- sample.donut(1000, r1, 1000, r2)
NSim <- 1000
simulation.err <- matrix(0, ncol = 2, nrow = NSim) 
for(i in 1:NSim){
  small.data.train <- sample.donut(100, r1, 100, r2)
  fit.lda <- lda(y~., data=small.data.train)
  pred.lda <- predict(fit.lda, newdata = data.test)
  fit.gam <- gam(y ~ s(X1) + s(X2), data=small.data.train, 
                 family = "binomial")
  pred.gam <- predict(fit.gam, newdata = data.test, 
                      type="response")
  class.pred <- ifelse(pred.gam > 0.5, 2, 1)
  simulation.err[i, 1] <- mean(pred.lda$class == data.test$y)
  simulation.err[i, 2] <- mean(class.pred == data.test$y)
}
boxplot(simulation.err)
