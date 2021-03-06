
################################################################################### Classification and Penalized Regression            Stat 154 Spring 2014        Derek Bean     ##
####################################################################################





##Write a k-nearest neighbors classifier for a famous benchmark data set: Fisher's iris data##

iris.data <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", header = FALSE)

names(iris.data) <- c("Sepal length", "Sepal width", "Petal length", "Petal width", "class")

str(iris.data) #3 factor levels for class.

pairs(iris.data[,1:4], col = c("red", "green", "blue")[iris.data$class], main = "Red = setosa, green = versicolor, blue = virginica")

perm = sample(1:nrow(iris.data))
test.indices = perm[1:30]
train.indices = perm[31:150]

train.data = iris.data[train.indices,]
test.data = iris.data[test.indices,]

























####My Code####

l2dist <- function(x) sum(x^2)

election <- function(x, datax, k)
{
    neighbor.indices = sort(apply(datax[,-5], 1, function(y) l2dist(y - x)), index.return = TRUE)
    k.nearest = neighbor.indices$ix[1:k]
    vote = tabulate(datax[,5][k.nearest])
    winner = levels(datax[,5])[which.max(vote)]
    winner
}

classify.test.points <- matrix(0, ncol = nrow(test.data), nrow = nrow(train.data))

for (k in 1:nrow(train.data))
{
classify.test.points[k,] <- apply(test.data[,-5], 1, election, datax = train.data, k = k)
}


percent.error = apply(classify.test.points, 1, function(x) sum(x!=test.data$class)/nrow(test.data))

plot(1:nrow(train.data), percent.error, xlab = "K nearest neighbor", ylab = "percentage error", main = "Percentage error for k-nearest neighbors", pch = ' ')
lines(1:nrow(train.data), percent.error, lwd = 2, col = "red")





neighbor.indices = sort(apply(train.data[,-5], 1, function(y) l2dist(y - test.data[,-5])), index.return = TRUE)
k.nearest = neighbor.indices$ix[1:k]

















##A taste of penalized methods ##

library(MASS) #Contains a method for ridge regression
library(lars) #LASSO package. Can also use "glmnet" to implement LASSO for very large datasets.
library(lasso2) #Contains a data set on prostate cancer I want to use (everything is nice and numeric)

data(Prostate)
?Prostate
str(Prostate)

lambdas.ridge = seq(0, 1200, by = 0.1)
Prostate.Ridge.Fit <- lm.ridge(lpsa~., data = Prostate, lambda = lambdas.ridge)


lambdas.lasso = seq(0, 900, by = 0.01)
Prostate.Lasso.Fit <- lars(model.matrix(lpsa~., data = Prostate),Prostate$lpsa, type = "lasso")
Prostate.Lasso.coef <- predict.lars(Prostate.Lasso.Fit, type = "coefficients", mode = "lambda", s = lambdas.lasso/100)



par(mfrow = c(1,2))

#plot(lambdas.ridge, Prostate.Ridge.Fit$coef[1,]), xlab = "lambda", ylab = "coefficients", main = "Ridge coefficients on prostate cancer date", xlim = c(-200, max(lambdas.ridge)), ylim = c((min(Prostate.Ridge.Fit$coef)-1), (max(Prostate.Ridge.Fit$coef)+1)), pch = ' ')

matplot(lambdas.ridge, t(Prostate.Ridge.Fit$coef), type = "l", xlab = "lambda", ylab = "coefficients", main = "Ridge coefficients on prostate cancer data", lty = 2, col = 1:8, xlim = c(-200, max(lambdas.ridge)))

abline(h = 0)

identify(rep(lambdas.ridge[1], 8), Prostate.Ridge.Fit$coef[,1], labels = names(Prostate)[1:8])

matplot(lambdas.lasso, Prostate.Lasso.coef$coefficients[,2:9], type = "l", xlab = "lambda", ylab = "coefficients", main = "Lasso coefficients on prostate cancer data", lty = 2, col = 1:8, xlim = c(-50, max(lambdas.lasso)))

abline(h = 0)

identify(rep(lambdas.lasso[1], 8), Prostate.Lasso.coef$coefficients[1,][2:9], labels = names(Prostate)[1:8])





