setwd("Dropbox/School/Statistics/Stat 154 Spring 2014/HW3")
#############Problem 1#############

#############Problem 2#############
ZipTrain = read.table(file="zip.train", header=F)
ZipTest = read.table('zip.test', header=F)
ZipTrain = ZipTrain[ZipTrain$V1== 2 | ZipTrain$V1== 3, ]
ZipTest = ZipTest[ZipTest$V1== 2 | ZipTest$V1== 3, ]
Problem2 = glm(V1~., data=ZipTrain)
summary(Problem2)$coefficients[,1]#errors
error.for.test = function(ZipTest){Testing = t(ZipTest[, -1])
                                   print(dim(Testing))
                                   Test.yhat = c()
                                   for(i in 1:ncol(Testing)){
                                     Test.yhat[i] = Problem2$coefficients[1]+sum(Problem2$coefficients[-1]*(Testing[,i]))}
                                   Test.yhat = round(Test.yhat)
                                   Test.error = sum(ZipTest$V1==Test.yhat)/length(ZipTest$V1)
return(Test.error)}
error.for.test(ZipTrain)
error.for.test(ZipTest)
library("FNN")
k.error.train.test = function(k){
Train.error.k = sum(ZipTrain$V1==knn(ZipTrain[,-1], ZipTrain[, -1], ZipTrain$V1, k = k))/length(ZipTrain$V1)
Test.error.k = sum(ZipTest$V1==knn(ZipTrain[,-1], ZipTest[, -1], ZipTrain$V1, k = k))/length(ZipTest$V1)
return(data.frame(Train.error.k, Test.error.k))
}
k.error.train.test(1)
k.error.train.test(3)
k.error.train.test(5)
k.error.train.test(7)
k.error.train.test(15)
#############Problem 3#############
X = rnorm(30, 0,1)
Y = 0 + 1*X + 2*X^2 + 3*X^3 + rnorm(30, 0, 1)
X.lm = lm(Y~1+X+I(X^2)+I(X^3))
X.lm
#  Coefficients:
# (Intercept)            X       I(X^2)       I(X^3)  
#      51.421       -1.154        2.030        3.000

X.Sum.lm = summary(X.lm)
X.Sum.lm
# Call:
#   lm(formula = Y ~ 1 + X + I(X^2) + I(X^3))
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.6499 -0.4746 -0.1368  0.5540  1.3134 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.06983    0.15975   0.437  0.66561    
# X            0.87570    0.28053   3.122  0.00437 ** 
#   I(X^2)       2.15108    0.11686  18.407  < 2e-16 ***
#   I(X^3)       2.98545    0.08381  35.620  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7275 on 26 degrees of freedom
# Multiple R-squared:  0.9953,  Adjusted R-squared:  0.9947 
# F-statistic:  1820 on 3 and 26 DF,  p-value: < 2.2e-16
errors = X.Sum.lm$coefficients[,1]
plot(X, Y, pch = 20)
y = X^3*X.lm$coefficients[4] + X^2*X.lm$coefficients[3] + (X)*X.lm$coefficients[2] + 1*X.lm$coefficients[1]
X.x = X[order(X)]
y = y[order(X)]
points(X.x, y, type="l", col = "red")

CI.X.lm = predict(X.lm, data.frame(X.x,y), interval="confidence")
upper.CI = 1.96*(CI.X.lm[,3]-CI.X.lm[,1]) + CI.X.lm[,1]
lower.CI = 1.96*(CI.X.lm[,2]-CI.X.lm[,1]) + CI.X.lm[,1]
points(X.x, upper.CI[order(X)],type="l", col = "blue")
points(X.x, lower.CI[order(X)],type="l", col = "blue")

upper.CI = qchisq(1-0.025, 5)*(CI.X.lm[,3]-CI.X.lm[,1]) + CI.X.lm[,1]
lower.CI = qchisq(1-0.025, 5)*(CI.X.lm[,2]-CI.X.lm[,1]) + CI.X.lm[,1]
points(X.x, upper.CI[order(X)],type="l", col = "green")
points(X.x, lower.CI[order(X)],type="l", col = "green")


#############Problem 5#############
brains = read.csv("brains.csv")
plot(brains$BrainWt, brains$BodyWt)
brains.lm = lm(brains$BrainWt~brains$BodyWt)
plot(brains.lm)

plot(brains$BrainWt[-c(33,32,19)], brains$BodyWt[-c(33,32,19)])
brains.lm = lm(brains$BrainWt[-c(33,32,19)]~brains$BodyWt[-c(33,32,19)])
plot(brains.lm)
