
####################################################################################
#Regression Examples  Feb. 28           Stat 154 Spring 2014             Derek Bean#
####################################################################################


library(glmnet)
library(lars)
library(MASS)

?lars   #lars, forward-stagewise, LASSO; linear model
?glmnet #lasso, ridge, elastic net; linear model, logistic binary, logistic multinomial, poisson...


#Lars vs. glmnet - speed

y = rnorm(1000)
x = matrix(rnorm(1000*10000), nrow = 1000)

data = data.frame(cbind(y, x))

fit.lars = lars(x,y, type = "lasso")
fit.lars1 = lars(x, y, type = "lasso", use.Gram = FALSE)

fit.glmnet = glmnet(x,y, family = "gaussian", alpha = 1)


#normalizations

y = rnorm(100)
x = matrix(rnorm(100*5), nrow = 100)

data = data.frame(cbind(y,x))
names(data) <- c("y", "x1", "x2", "x3", "x4", "x5")

fit.lars <- lars(x, y, normalize = FALSE)

fit.glmnet <- glmnet(x, y, family = "gaussian", lambda = (1/length(y))*fit.lars$lambda, standardize = FALSE)

coef(fit.lars, s = fit.lars$lambda[27], mode = "lambda")
predict(fit.lars, s=fit.lars$lambda[27], mode = "lambda", type = "coefficients")

predict(fit.glmnet, s=fit.lars$lambda[27]/length(y), type = "coefficients")

predict(fit.lars, s = 0.45, mode = "lambda", type = "coefficients")
predict(fit.glmnet, s = 0.45/length(y), type = "coefficients")

#plotting methods
plot(fit.lars)
plot(fit.glmnet) #note different x-axes; just scaled differently
plot(fit.glmnet, xvar = "lambda")

plot(fit.lars)
plot(fit.glmnet, xvar = "lambda")
points(log(fit.lars$lambda/100), rep(0, length(fit.lars$lambda)), pch = 'x')


Sigma <- function(p, cor)
{
    Sigma <- matrix(cor, nrow = p, ncol = p)
    diag(Sigma) = rep(1, p)
    Sigma
}

#simulations to play with

p = 1000
s = 500
n = 100
cor = 0.85

X = mvrnorm(n, mu = rep(0, p), Sigma(p, cor))
Y = X%*%c(rnorm(s, 0, sd = 1/sqrt(10)), rep(0, p-s)) + rnorm(n)

stepwise.fit = lars(X, Y, type = "stepwise", use.Gram = FALSE)
#not the only stepwise procedure in R. stepAIC in MASS library will work with p<n, uses AIC criterion to make decision to add/drop variable. See also library "leaps" (best subset, when this is feasible!!!), addterm, dropterm, update

stagewise.fit = lars(X, Y, type = "forward.stagewise", use.Gram = FALSE)

lasso.fit = glmnet(X, Y, family = "gaussian", alpha = 1)
ridge.fit = glmnet(X, Y, family = "gaussian", alpha = 0)

?cv.glmnet
cv.lasso = cv.glmnet(X, Y, family = "gaussian", type.measure = "mse", alpha = 1)
cv.ridge = cv.glmnet(X, Y, family = "gaussian", type.measure = "mse", alpha = 0)

enet.fit = glmnet(X, Y, family = "gaussian", alpha = 0.25)
cv.enet = cv.glmnet(X, Y, family = "gaussian", type.measure = "mse", alpha = 0.25)

plot(cv.lasso)
plot(cv.ridge)
plot(cv.enet)

coef(cv.lasso)
coef(cv.lasso)
coef(cv.net)

## real data example ##

#multinomial classification 
install.packages("spls")
library("spls")

data(lymphoma)
lymphoma$x[1:5,1:5]
lymphoma$y

dim(lymphoma$x)
length(lymphoma$y)
