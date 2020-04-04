library(corrplot)
# differences con option raw
x <- rnorm(1000)
raw.poly <- poly(x, 6,raw=T)
orthogonal.poly = poly(x,6)
corrplot(cor(raw.poly), method="color", title="Non-Orthogonal-Poly", 
         mar = c(0, 0, 2, 0))
corrplot(cor(orthogonal.poly), method="color", title="Orthogonal-Poly",
         mar = c(0, 0, 2, 0))

# fitted value (two cases)
x <- rnorm(1000)
y <- 3 + 4*x + 5*x^2 + 3*x^3 + rnorm(1000)
raw.mod <- lm(y~poly(x,6,raw=T))
orthogonal.mod <- lm(y~poly(x,6))
plot(x, y, main="Fitted model with non-orthogonal poly.")
abline(raw.mod$coefficients[1], raw.mod$coefficients[2], col="red")
plot(x, y, main="Fitted model with orthogonal poly.")
abline(orthogonal.mod$coefficients[1], orthogonal.mod$coefficients[2], col="red")
sum(fitted(raw.mod)-fitted(orthogonal.mod))


# simulation biais + variance with d-degree polynome 
model <- function(x) return(3 + 4*x + 3*x^3)
xtrain <- rnorm(100)
ytrain <-  model(xtrain) + rnorm(100)
xtest <- rnorm(100)
ytest <- model(xtest) + rnorm(100)
degree <- 13
err <- matrix(0, ncol=degree, nrow=100)
for(d in 1:degree){
  model.degree.d <- lm(y~poly(x, d), data=data.frame(x=xtrain, y=ytrain))  
  err[, d] <- (ytest-predict(model.degree.d, data.frame(x=xtest)))^2
}

# bias-variance tradeoff
err_data <- 1:degree
err_data <- cbind(err_data, colMeans(err))
stderr <- function(x) sd(x)/sqrt(length(x))
err_data <- cbind(err_data, apply(err, 2, stderr))
library(ggplot2)
ggplot(as.data.frame(err_data), aes(err_data))+
  geom_line(aes(y = V2)) +
  geom_ribbon(aes(ymin=V2-1.6*V3,ymax=V2+1.6*V3), alpha=0.3) +
  ylab("MSE")+
  xlab("Degree d")


# analyse component principal (ACP)
library(FactoMineR)
pca.orho <- PCA(orthogonal.poly, graph = F)
pca.raw <- PCA(raw.poly, graph = F)
plot(pca.orho, choix='var')
plot(pca.raw, choix="var")
  

  

