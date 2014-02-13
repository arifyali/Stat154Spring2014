##############################################################################################
## PCA example code            Stat 154 Spring 2014        Derek Bean     ##
####################################################################################




###### Handwritten Digits Section 14.5 HTF
three.digits <- read.csv("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/zip.digits/train.3")

dim(three.digits) #657 gray-scale images

three.digits <- as.matrix(three.digits)

colors<-c('white','black') #want to put the digits at a gray scale
cus_col<-colorRampPalette(colors=colors) #this function allows to interpolate x amount of colors from white to black. E.g. cus_col(1) returns the code for white; cus_col(2) returns white and black; cus_col(3) returns white, a gray color in between, and black.

#An 'average' 3. Tilted right, due to most people being right handed...?
par(pty = 's', mar = c(1,1,1,1), xaxt = 'n', yaxt = 'n')

image(1:16, 1:16, matrix(colMeans(three.digits), nrow = 16), main = "'Average 3'", col = cus_col(256))

#Compute principal components and singular values


prin.comps = prcomp(three.digits, retx = TRUE) #centers by default; we've already computed the mean 3.
prin.comps$sdev #always good to look at the singular values, check multiplicities or long tail in the decay.

image(1:16, 1:16, matrix(prin.comps$rotation[,1], nrow = 16), main = "First Rotation", col = cus_col(256))

image(1:16, 1:16, matrix(prin.comps$rotation[,2], nrow = 16), main = "Second Rotation", col = cus_col(256))

#compare the first and second projection directions to the mean 3 and observe how they capture different variations in how people write.

screeplot(prin.comps, type = "lines")
explained.var = sapply(1:256, function(i) sum(prin.comps$sdev[1:i]^2)/sum(prin.comps$sdev^2))
sum(explained.var<=0.9) #How many components do we need to pick up 90% of the variance?
sum(explained.var<=0.95) #95%
sum(explained.var<=0.99) #99%

# Look at the principal components themselves (not the projection directions; the actual projections; each principal component contains the n projections of the n data points on the first projection direction); as in HTF we create a grid based on the marginal sample quantiles and choose samples closest to the grid intersection to see if we can spot meaningful variation in the 3s; in that sense, the PCA might "explain" how handwritten 3s tend to vary.
plot(prin.comps$x[,1], prin.comps$x[,2], xlab = "First Principal Component", ylab = "Second Principal Component", pch = 20, col = "green")

pc1quantiles = quantile(prin.comps$x[,1], probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
pc2quantiles = quantile(prin.comps$x[,2], probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
abline(v = pc1quantiles, col = "blue", lty = 3)
abline(h = pc2quantiles, col = "blue", lty = 3)

identify(prin.comps$x[,1], prin.comps$x[,2])
edges = c(72, 237, 549, 316, 639, 283, 83, 132, 91, 395, 391, 487, 553, 416, 181, 94, 343, 141, 404, 648, 183, 454, 233, 374, 175) #used identify and eye-balled to select these points
par(mfrow=c(5,5), pty = 's', mar = c(1,1,1,1), xaxt = 'n', yaxt = 'n')
for(i in 1:25)
{
    image(1:16, 1:16, matrix(three.digits[edges[i],], nrow = 16), main = edges[i], col = cus_col(256))
}


######## Simulation Examples

#Multivariate Gaussian

n = 100
p = 2
X = (matrix(rnorm(n*p), nrow = n)%*%diag(c(4, 1)))%*%matrix(c(1/sqrt(2), 1/sqrt(2), 1/sqrt(2), -1/sqrt(2)), nrow=2, byrow=TRUE) #one dominant direction
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
plot(princomp.X$x[,2], col = cols) #PCA with the largest eigenvalue captures the clustering structure


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
plot(princomp.X$x[,2], col = cols) #PCA with the smallest eigenvalue captures the clustering structure

#No guarantee high-dimensional data don't cluster in a different dimension then the one spanned by the leading PC axes!

