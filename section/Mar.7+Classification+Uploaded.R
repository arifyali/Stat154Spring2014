
################################################################################### Classification            Stat 154 Spring 2014        Derek Bean     ##
####################################################################################

library(MASS)
library(glmnet)



##Compare classifiers for a famous benchmark data set: Fisher's iris data##

iris.data <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", header = FALSE)

names(iris.data) <- c("Sepal length", "Sepal width", "Petal length", "Petal width", "class")

str(iris.data) #3 factor levels for class.

pairs(iris.data[,1:4], col = c("red", "green", "blue")[iris.data$class], main = "Red = setosa, green = versicolor, blue = virginica")

n.test = 50
n.train = nrow(iris.data) - n.test

perm = sample(1:nrow(iris.data))
test.indices = perm[1:n.test]
train.indices = perm[(n.test+1):nrow(iris.data)]

train.data = iris.data[train.indices,]
test.data = iris.data[test.indices,]

###KNN
knn.data = (knn(train.data[,-5], test.data[,-5], train.data[,5], k=1))
sum(knn.data!=test.data[,5])

logistic.data = 