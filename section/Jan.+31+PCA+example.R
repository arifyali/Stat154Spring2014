##############################################################################################
## PCA example code            Stat 154 Spring 2014        Derek Bean     ##
####################################################################################

###### Handwritten Digits Section 14.5 HTF
three.digits <- read.csv("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/zip.digits/train.3")

dim(three.digits) #657 gray-scale images

three.digits <- as.matrix(three.digits)

colors<-c('white','black')
cus_col<-colorRampPalette(colors=colors)

#An 'average' 3
par(pty = 's', mar = c(1,1,1,1), xaxt = 'n', yaxt = 'n')

image(1:16, 1:16, matrix(colMeans(three.digits), nrow = 16), main = "'Average 3'", col = cus_col(256))

#Compute principal components and singular values


prin.comps = prcomp(three.digits, retx = TRUE) #centers by default
prin.comps$sdev

image(1:16, 1:16, matrix(prin.comps$rotation[,1], nrow = 16), main = "First Rotation", col = cus_col(256))

image(1:16, 1:16, matrix(prin.comps$rotation[,2], nrow = 16), main = "Second Rotation", col = cus_col(256))

screeplot(prin.comps, type = "lines")
explained.var = sapply(1:256, function(i) sum(prin.comps$sdev[1:i]^2)/sum(prin.comps$sdev^2))
sum(explained.var<=0.9)
sum(explained.var<=0.95)
sum(explained.var<=0.99)


plot(prin.comps$x[,1], prin.comps$x[,2], xlab = "First Principal Component", ylab = "Second Principal Component", pch = 20, col = "green")

pc1quantiles = quantile(prin.comps$x[,1], probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
pc2quantiles = quantile(prin.comps$x[,2], probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
abline(v = pc1quantiles, col = "blue", lty = 3)
abline(h = pc2quantiles, col = "blue", lty = 3)

identify(prin.comps$x[,1], prin.comps$x[,2])
edges = c(72, 237, 549, 316, 639, 283, 83, 132, 91, 395, 391, 487, 553, 416, 181, 94, 343, 141, 404, 648, 183, 454, 233, 374, 175)
par(mfrow=c(5,5), pty = 's', mar = c(1,1,1,1), xaxt = 'n', yaxt = 'n')
for(i in 1:25)
{
    image(1:16, 1:16, matrix(three.digits[edges[i],], nrow = 16), main = edges[i], col = cus_col(256))
}


######## Simulation Examples

#Multivariate Gaussian

n = 100
p = 2
X = (matrix(rnorm(n*p), nrow = n)%*%diag(c(4, 1)))%*%matrix(c(1/sqrt(2), 1/sqrt(2), 1/sqrt(2), -1/sqrt(2)), nrow=2, byrow=TRUE)
plot(X[,1], X[,2], col = "blue")
princomp.X <- prcomp(X, retx = TRUE)
princomp.X
prcomp(X, scale = TRUE) #does working on the correlation matrix change anything?

abline(a = 0, b = -princomp.X$rotation[1,2]/princomp.X$rotation[2,2], col = "green") #first principal rotation direction
abline(a = 0, b = -princomp.X$rotation[1,1]/princomp.X$rotation[2,1], col = "yellow")#second principal rotation direction

plot(princomp.X$x[,1], col = "blue")
plot(princomp.X$x[,2], col = "blue")


#Outliers: mixture with a corrupting distribution
n = 100
p = 2
prop = diag(rbinom(n, 1, prob = 0.85))
X = (prop%*%matrix(rnorm(n*p), nrow = n)%*%diag(c(4, 1)) + (diag(n) - prop)%*%matrix(rnorm(n*p), nrow = n)%*%diag(c(1,16)))%*%matrix(c(1/sqrt(2), 1/sqrt(2), 1/sqrt(2), -1/sqrt(2)), nrow=2, byrow=TRUE)
cols <- rep("red", n)
cols[which(diag(prop)==1)] <- "blue"
plot(X[,1], X[,2], col = cols)
princomp.X <- prcomp(X, retx = TRUE)
princomp.X
prcomp(X, scale = TRUE) #does working on the correlation change anything?

abline(a = 0, b = -princomp.X$rotation[1,2]/princomp.X$rotation[2,2], col = "green") #first principal rotation direction
abline(a = 0, b = -princomp.X$rotation[1,1]/princomp.X$rotation[2,1], col = "yellow")#second principal rotation direction

plot(princomp.X$x[,1], col = cols)
plot(princomp.X$x[,2], col = cols) #preferred axis for visualizing data is the smallest PC!

#Clusters
library(MASS)
n = 100
p = 2
prop = diag(rbinom(n, 1, prob = 0.5))
Sigma = matrix(c(1, 0, 0, 2), byrow = TRUE, nrow = 2)
X = (prop%*%mvrnorm(n, mu = c(5,0), Sigma) + (diag(n) - prop)%*%mvrnorm(n, mu = c(-5,0), Sigma))%*%matrix(c(1/sqrt(2), 1/sqrt(2), 1/sqrt(2), -1/sqrt(2)), nrow=2, byrow=TRUE)
cols <- rep("red", n)
cols[which(diag(prop)==1)] <- "blue"
plot(X[,1], X[,2], col = cols)

princomp.X <- prcomp(X, retx = TRUE)
princomp.X

abline(a = 0, b = -princomp.X$rotation[1,2]/princomp.X$rotation[2,2], col = "green") #first principal rotation direction
abline(a = 0, b = -princomp.X$rotation[1,1]/princomp.X$rotation[2,1], col = "yellow")#second principal rotation direction

plot(princomp.X$x[,1], col = cols)
plot(princomp.X$x[,2], col = cols)


prop = diag(rbinom(n, 1, prob = 0.5))
Sigma = matrix(c(1, 0, 0, 16^2), byrow = TRUE, nrow = 2)
X = (prop%*%mvrnorm(n, mu = c(3,0), Sigma) + (diag(n) - prop)%*%mvrnorm(n, mu = c(-3,0), Sigma))%*%matrix(c(1/sqrt(2), 1/sqrt(2), 1/sqrt(2), -1/sqrt(2)), nrow=2, byrow=TRUE)
cols <- rep("red", n)
cols[which(diag(prop)==1)] <- "blue"
plot(X[,1], X[,2], col = cols)

princomp.X <- prcomp(X, retx = TRUE)
princomp.X
s
abline(a = 0, b = -princomp.X$rotation[1,2]/princomp.X$rotation[2,2], col = "green") #first principal rotation direction
abline(a = 0, b = -princomp.X$rotation[1,1]/princomp.X$rotation[2,1], col = "yellow")#second principal rotation direction

plot(princomp.X$x[,1], col = cols)
plot(princomp.X$x[,2], col = cols)
#Eigenvalue spacing

n = 1000
p = 10

X = matrix(rnorm(n*p), nrow = n)%*%diag(sqrt(p:1)) #What should the sample principal vectors be close to?
prcomp(X)

par(mfrow = c(3,3))
for (i in 1:9)
{
    X = matrix(rnorm(n*p), nrow = n)%*%diag(sqrt(p:1))
    princomp.X = prcomp(X, retx = TRUE)
    plot(princomp.X$rotation[,1], princomp.X$rotation[,2])
}

X = matrix(rnorm(n*p), nrow = n)%*%diag(sqrt(c(p, p-0.1, (p-2):1)))
prcomp(X)


X = matrix(rnorm(n*p), nrow = n)%*%diag(sqrt(c(p, p, (p-2):1)))
prcomp(X)

par(mfrow = c(3,3))
for (i in 1:9)
{
    X = matrix(rnorm(n*p), nrow = n)%*%diag(sqrt(c(p, p, (p-2):1)))
    princomp.X = prcomp(X, retx = TRUE)
    plot(princomp.X$rotation[,1], princomp.X$rotation[,2])
}
