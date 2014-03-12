####################################################################################
## Homework 1          Stat 154 Spring 2014        Derek Bean                     ##
####################################################################################

###1###

#Part 1#
############### Clean the data ###############
#Make sure you set the working directory to the location of the sp500 csv file
sp500 <- read.csv("bf4024f5c75fc062.csv", header = TRUE)

sp500$PRC <- abs(sp500$PRC) #some prices are negative
sp500$date <- as.factor(sp500$date) #easier to encode the dates as factors instead of integers

length(levels(sp500$date)) #1343 dates
tabulate(sp500$date) #one date at index 1301 has only 5 entries recorded; most have around 500

levels(sp500$date)[1301] #October 29, 2012 - the day the storm surge from Hurricane Sandy hit New Jersey and Manhattan

sp500$PRC[which(sp500$date=="20121029")]
#[1] NA NA NA NA NA
#Market closed that day.


length(levels(sp500$COMNAM)) #565 companies
tabulate(sp500$COMNAM) #many companies have 1342 price dates; they are missing 10/31/2012.
sum(tabulate(sp500$COMNAM) >= 1342) #433 stocks have more than 1342 dates
which(tabulate(sp500$COMNAM) == 1343) #4 companies have entries for 1343 dates

which(tabulate(sp500$COMNAM) > 1343) #some companies have more than 1343 dates.
tabulate(sp500$COMNAM)[which(tabulate(sp500$COMNAM) > 1343)] #5 companies have 2684 = 2*1342 dates. So there may be double entries. 2 companies do not have multiples of 1342. We need to figure out what's going on.

levels(sp500$COMNAM)[which(tabulate(sp500$COMNAM) > 1343)] #Chipotle has 1924 entries and Lowes has 1535 entries. None of the companies with more than 1343 dates have the 29th of October, 2012. So may be safe to delete this date and still have 433 stocks with 1342 dates for prices.

sp500$date[which(sp500$COMNAM=="C B S CORP NEW")]
sp500$date[which(sp500$COMNAM=="CHIPOTLE MEXICAN GRILL INC")]
sp500$date[which(sp500$COMNAM=="LOEWS CORP")] #visual checks

hurricane.sandy.indices = which(sp500$date=="20121029")
sp500.mod = sp500[-hurricane.sandy.indices,]#delete hurricane sandy dates
dates = levels(sp500.mod$date)[-1301] #pick the dates we want to use; delete the date of Sandy
sp500.mod$date = as.character(sp500.mod$date) #we've extracted the unique dates so we turn this back to a character vector for technical reasons

comnames = levels(sp500.mod$COMNAM)[which(tabulate(sp500.mod$COMNAM) >= 1342)] #pick the companies to use

date.mismatches = c()
for (j in 1:length(comnames))
{
    date.mismatches[j] = sum(abs(as.numeric(dates) - as.numeric(sp500.mod$date[which(sp500.mod$COMNAM==comnames[j])][1:1342])))
}

sum(date.mismatches>0) #0 mismatches. WE are verifying that for each stock, the first 1342 dates listed for that stock are the unique 1342 dates in the time series that we care about, and any more dates after that are just repeats. This makes extracting the right prices for the right companies at the right dates much easier.

#now we can begin constructing the data matrix of returns for the 433 stocks that have prices for the 1342 dates
prices <- matrix(0, nrow = length(dates), ncol = length(comnames))

for (j in 1:length(comnames))
{
    prices[,j] = sp500.mod$PRC[which(sp500.mod$COMNAM==comnames[j])][1:1342]

}

sum(is.na(prices)) #2 NAs to investigate.
NAs.by.stocks = apply(prices, 2, function(x) sum(is.na(x)))
which(NAs.by.stocks > 0)
#[1] 110
comnames[110]
#[1] "CONSTELLATION BRANDS INC"
which(is.na(prices[,110])==TRUE)
#[1]  1 14
length(sp500.mod$PRC[which(sp500.mod$COMNAM=="CONSTELLATION BRANDS INC")])
#[1] 2684 Luckily this is one of the stocks whose prices were double counted. Perhaps we can replace the NAs with the correct values

sum(is.na(sp500.mod$PRC[which(sp500.mod$COMNAM=="CONSTELLATION BRANDS INC")][1343:2684]))
#[1] 0
#Those values aren't missing!

sum(abs(as.numeric(dates) - as.numeric(sp500.mod$date[which(sp500.mod$COMNAM=="CONSTELLATION BRANDS INC")][1343:2684])))
#[1] 0
#So prices for Constellation Brands have been doubled up.

prices[,110] = sp500.mod$PRC[which(sp500.mod$COMNAM=="CONSTELLATION BRANDS INC")][1343:2684]

sum(is.na(prices))

sum(prices < 0)

#now we calculate excess returns and construct a data frame.

returns = matrix(0, nrow = length(dates)-1, ncol = length(comnames))
for (i in 1:(length(dates)-1))
{
    returns[i,] = (prices[(i+1),]/prices[i,]) -1
}

sp500returns = data.frame(returns, row.names = dates[2:1342])

names(sp500returns) = comnames

############## PCA analysis ###############

sp500returns.pca = princomp(sp500returns, scores = TRUE)

screeplot(sp500returns.pca, type = "lines", main = "Scree Plot")

cum.variance = sapply(1:length(comnames), function(x) sum(sp500returns.pca$sdev[1:x]^2)/sum(sp500returns.pca$sdev^2))

cum.variance[5]
cum.variance[6]

sum(cum.variance <= 0.9)
sum(cum.variance <= 0.95)



sp500returns.pca$loadings[,1]
sum(sp500returns.pca$loadings[,1] >= 0)

sp500returns.pca$loadings[,2]
sum(sp500returns.pca$loadings[,2] <= 0)

plot(sp500returns.pca$loadings[,1], sp500returns.pca$loadings[,2], xlab = "First Principal Component Direction", ylab = "Second Principal Component Direction", main = "Loadings Scatterplot", col = "blue", pch = 20)

identify(sp500returns.pca$loadings[,1], sp500returns.pca$loadings[,2], labels = comnames)


plot(sp500returns.pca$scores[,1], sp500returns.pca$scores[,2], xlab = "First Principal Component Scores", ylab = "Second Principal Component Scores", main = "Scores Scatterplot", col = "red", pch = 20)

identify(sp500returns.pca$scores[,1], sp500returns.pca$scores[,2], labels = row.names(sp500returns))

plot(sp500returns.pca$scores[-460,1], sp500returns.pca$scores[-460,2], xlab = "First Principal Component Scores", ylab = "Second Principal Component Scores", main = "Scores Scatterplot", col = "red", pch = 20)

identify(sp500returns.pca$scores[-460,1], sp500returns.pca$scores[-460,2], labels = row.names(sp500returns)[-460])

#From identify function, captured the indices of key dates and studied market behavior:
sp500returns[260,]
sp500returns[280,]
sp500returns[314,]
sp500returns[385,]

sp500returns.pca$scores[385,1] - sp500returns[385,26]*sp500returns.pca$loadings[385,1]
#[1] -0.2928771



plot(sp500returns.pca$loadings[,3], sp500returns.pca$loadings[,4], xlab = "Third Principal Component Direction", ylab = "Fourth Principal Component Direction", main = "Loadings Scatterplot", col = "blue", pch = 20)

identify(sp500returns.pca$loadings[,3], sp500returns.pca[,4], labels = comnames)

plot(sp500returns.pca$scores[,3], sp500returns.pca$scores[,4], xlab = "Third Principal Component Scores", ylab = "Fourth Principal Component Scores", main = "Scores Scatterplot", col = "red", pch = 20)

identify(sp500returns.pca$scores[,3], sp500returns.pca$scores[,4], labels = row.names(sp500returns)))





which.max(abs(sp500returns.pca$scores[,1]))
which.max(abs(sp500returns.pca$scores[,2]))
which.max(abs(sp500returns.pca$scores[,3]))
which.max(abs(sp500returns.pca$scores[,4]))

sp500returns.pca.nooutliers = princomp(sp500returns[-c(460, 691, 927),], scores = TRUE)

plot(sp500returns.pca.nooutliers$loadings[,1], sp500returns.pca.nooutliers$loadings[,2], xlab = "First Principal Component Direction", ylab = "Second Principal Component Direction", main = "Loadings Scatterplot w/o Outliers", col = "blue", pch = 20)

identify(sp500returns.pca.nooutliers$loadings[,1], sp500returns.pca.nooutliers$loadings[,2], labels = comnames)


#Part 2#

sp500returns.rev = data.frame(t(returns), row.names = comnames)
names(sp500returns.rev) = row.names(sp500returns)

company.indices = sample(433,30) #select 30 companies at random
comnames[company.indices]
sp500returns.rev.mod = sp500returns.rev[company.indices,]

dissim.stocks = dist(sp500returns.rev.mod)

sp500.clustering = hclust(dissim.stocks, method = "average")

plot(sp500.clustering)




###2###

#Part 1#

x.train <- read.table("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/14cancer.xtrain")
x.test <- read.table("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/14cancer.xtest")

#labels
y.train <- read.table("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/14cancer.ytrain")
y.test <- read.table("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/14cancer.ytest")

cancer.data <- rbind(t(x.train), t(x.test))
labels <- c(as.numeric(y.train), as.numeric(y.test))
labelnames <- c("breast", "prostate", "lung", "collerectal", "lymphoma", "bladder", "melanoma", "uterus", "leukemia", "renal", "pancreas", "ovary", "meso", "cns")
col.pallete <- c("brown","black", "gray", "orange", "yellow", "blue", "purple", "red", "green", "pink", "aquamarine", "darkorange", "darkblue", "darkred")


min(apply(cancer.data, 2, var))
max(apply(cancer.data, 2, var))

#variance of each variable are different orders of magnitude. Advisable to do principal components on the scaled data matrix

pc.cancer.data <- prcomp(cancer.data, scale = TRUE, retx = TRUE)

screeplot(pc.cancer.data, type = "lines", main = "Scree plot")

cum.variance = sapply(1:nrow(cancer.data), function(x) sum(pc.cancer.data$sdev[1:x]^2)/sum(pc.cancer.data$sdev^2))


cum.variance[3]
cum.variance[4]

sum(cum.variance <= 0.9)
sum(cum.variance <= 0.95)


plot(pc.cancer.data$rotation[,1], pc.cancer.data$rotation[,2], xlab = "First PC loadings", ylab = "Second PC loadings", pch = 20, col = "blue")
identify(pc.cancer.data$rotation[,1], pc.cancer.data$rotation[,2])

plot(pc.cancer.data$x[,1], pc.cancer.data$x[,2], xlab = "First PC scores", ylab = "Second PC scores", main = "First v second principal component scores", col = col.pallete[labels])

legend("topright", legend = labelnames, col = col.pallete, pch = 1)


plot(pc.cancer.data$rotation[,2], pc.cancer.data$rotation[,4], xlab = "Second PC loadings", ylab = "Fourth PC loadings", pch = 20, col = "blue")
identify(pc.cancer.data$rotation[,2], pc.cancer.data$rotation[,4])

plot(pc.cancer.data$x[,2], pc.cancer.data$x[,4], xlab = "Second PC scores", ylab = "Fourth PC scores", main = "Second v fourth principal component scores", col = col.pallete[labels])

legend("bottomright", legend = labelnames, col = col.pallete, pch = 1)

library(kernlab)

kpca.cancer.data1 <- kpca(scale(cancer.data), kernel = "rbfdot", kpar = list(sigma = 0.008))

jitter.first.kpc1 <- jitter(rotated(kpca.cancer.data1)[,1], amount = 0.5)
jitter.second.kpc1 <- jitter(rotated(kpca.cancer.data1)[,2], amount = 0.5)

plot(jitter.first.kpc1, jitter.second.kpc1, pch = 20, col = col.pallete[labels], xlab = "First KPC score", ylab = "Second KPC score", main = "1st v 2nd prin. comp. scores, KPCA, sigma^(-1) = 0.008")
identify(jitter.first.kpc1, jitter.second.kpc1)
legend("topright", legend = labelnames, col = col.pallete, pch = 20)

kpca.cancer.data2 <- kpca(scale(cancer.data), kernel = "rbfdot", kpar = list(sigma = 0.001))

plot(rotated(kpca.cancer.data2)[,1:2], pch = 20, col = col.pallete[labels], xlab = "First KPC score", ylab = "Second KPC score", main = "1st v 2nd prin. comp. scores, KPCA, sigma^(-1) = 0.001")
legend("bottomleft", legend = labelnames, col = col.pallete, pch = 20)


#Part 2#

kmeans.cancer.data <- kmeans(cancer.data, 14)
kmeans.cancer.data$totss

for (j in 1:14)
{
    table = table(kmeans.cancer.data$cluster[which(labels==j)])
    print(labelnames[j])
    print(table)
}

pairs(cancer.data[,1:4], col = kmeans.cancer.data$cluster)

library(cluster)

kmedoids.cancer.data <- pam(cancer.data, 14, diss = FALSE, metric = "euclidean")

kmedoids.cancer.data$id.med

for (j in 1:14)
{
    table = table(kmedoids.cancer.data$clustering[which(labels==j)])
    print(labelnames[j])
    print(table)
}


###3###

gthetas = runif(150, 0, 2*pi)
greensamplesx <- 5*cos(gthetas)
greensamplesy <- 5*sin(gthetas)
greensamples <- cbind(greensamplesx, greensamplesy)

bthetas = runif(150, 0, 2*pi)
bluesamplesx <- 2.8*cos(bthetas)
bluesamplesy <- 2.8*sin(bthetas)
bluesamples <- cbind(bluesamplesx, bluesamplesy)

othetas = runif(150, 0, 2*pi)
orangesamplesx <- cos(othetas)
orangesamplesy <- sin(othetas)
orangesamples <- cbind(orangesamplesx, orangesamplesy)

samples = rbind(greensamples, bluesamples, orangesamples) + matrix(rnorm(2*450, sd = 0.25), ncol = 2)

colors = c(rep("green", 150), rep("blue", 150), rep("orange", 150))

weights = matrix(0, nrow = 450, ncol = 450)

for (i in 1:450)
{
    for (j in 1:450)
    {
        weights[i,j] = exp(-sum((samples[i,] - samples[j,])^2)/(2*0.1))
    }
}

degree = diag(colSums(weights - diag(450)))

Laplacian = degree - weights

weighted.Laplacian = diag(450) - solve(degree)%*%weights

spectrum = eigen(Laplacian)

weighted.spectrum = eigen(weighted.Laplacian)

plot(jitter(spectrum$vectors[, 449], amount = 0.01), jitter(spectrum$vectors[,448], amount = 0.01), xlab = "Second smallest eigenvector", ylab = "Third smallest eigenvector", main = "Spectral clustering", pch = 20, col = colors)


plot(jitter(weighted.spectrum$vectors[, 449], amount = 0.01), jitter(weighted.spectrum$vectors[,448], amount = 0.01), xlab = "Second smallest eigenvector", ylab = "Third smallest eigenvector", main = "Spectral clustering, normalized Laplacian", pch = 20, col = colors)

sample.kpca <- kpca(samples, kernel = "rbfdot", kpar = list(sigma = 1/sqrt(10)))
plot(rotated(sample.kpca)[,1:2], xlab = "First kPC component", ylab = "Second kPC component", main = "Kernel PCA, sigma = 10", pch = 20, col = colors)

sample.pca = prcomp(samples, scale = TRUE, retx = TRUE)
plot(jitter(sample.pca$x[,1], amount = 0.1), jitter(sample.pca$x[,2], amount = 0.1), xlab = "First PC component", ylab = "Second PC component", main = "PCA", pch = 20, col = colors)

###4##
n = 1000
p = 10

bad.pca.data = matrix(rnorm(n*p), nrow = n)
bad.pca.example = prcomp(bad.pca.data, retx = TRUE)
screeplot(bad.pca.example, type = "lines", main = "Bad Scree Plot")

mat = matrix(rnorm(p^2), nrow = p)
rotation = eigen(mat)$vectors
good.pca.data = matrix(rnorm(n*p), nrow = n)%*%diag(1:p)%*%rotation

good.pca.example = prcomp(good.pca.data, retx = TRUE, scale = FALSE)
screeplot(good.pca.example, type = "lines", main = "Good Scree Plot, no scaling")

good.pca.example = prcomp(good.pca.data, retx = TRUE, scale = TRUE)
screeplot(good.pca.example, type = "lines", main = "Good Scree Plot, scaling")



###5###

n = 100
B = 1000
v = 5

p = 5

data1 = matrix(rnorm(n*p), nrow = n)%*%diag(1:v)
pca.1 = prcomp(data1, retx = TRUE, scale = FALSE)

screeplot(pca.1, type = "lines", main = "Scree Plot, p/n = 0.05")

angles = c()
for (j in 1:B)
{
    data1 = matrix(rnorm(n*p), nrow = n)%*%diag(1:v)
    pca.1 = prcomp(data1, retx = TRUE, scale = FALSE)
    angles[j] = abs(pca.1$rotation[v,1])
}

hist(angles, main = "Histogram of projections, p/n = 0.05")



p = 50

data2 = matrix(rnorm(n*p), nrow = n)%*%diag(c(1:v, rep(1, p-v)))
pca.2 = prcomp(data2, retx = TRUE, scale = FALSE)

screeplot(pca.2, type = "lines", main = "Scree Plot, p/n = 0.5")

angles = c()
for (j in 1:B)
{
    data2 = matrix(rnorm(n*p), nrow = n)%*%diag(c(1:v, rep(1, p-v)))
    pca.2 = prcomp(data2, retx = TRUE, scale = FALSE)
    angles[j] = abs(pca.2$rotation[v,1])
}

hist(angles, main = "Histogram of projections, p/n = 0.5")

p = 100

data3 = matrix(rnorm(n*p), nrow = n)%*%diag(c(1:v, rep(1, p-v)))
pca.3 = prcomp(data3, retx = TRUE, scale = FALSE)

screeplot(pca.3, type = "lines", main = "Scree Plot, p/n = 1")

angles = c()
for (j in 1:B)
{
    data3 = matrix(rnorm(n*p), nrow = n)%*%diag(c(1:v, rep(1, p-v)))
    pca.3 = prcomp(data3, retx = TRUE, scale = FALSE)
    angles[j] = abs(pca.3$rotation[v,1])
}

hist(angles, main = "Histogram of projections, p/n = 1")

p = 200

data4 = matrix(rnorm(n*p), nrow = n)%*%diag(c(1:v, rep(1, p-v)))
pca.4 = prcomp(data4, retx = TRUE, scale = FALSE)

screeplot(pca.4, type = "lines", main = "Scree Plot, p/n = 2")

angles = c()
for (j in 1:B)
{
    data4 = matrix(rnorm(n*p), nrow = n)%*%diag(c(1:v, rep(1, p-v)))
    pca.4 = prcomp(data4, retx = TRUE, scale = FALSE)
    angles[j] = abs(pca.4$rotation[v,1])
}

hist(angles, main = "Histogram of projections, p/n = 2")

p = 500

data5 = matrix(rnorm(n*p), nrow = n)%*%diag(c(1:v, rep(1, p-v)))
pca.5 = prcomp(data5, retx = TRUE, scale = FALSE)

screeplot(pca.5, type = "lines", main = "Scree Plot, p/n = 5")

angles = c()
for (j in 1:B)
{
    data5 = matrix(rnorm(n*p), nrow = n)%*%diag(c(1:v, rep(1, p-v)))
    pca.5 = prcomp(data5, retx = TRUE, scale = FALSE)
    angles[j] = abs(pca.5$rotation[v,1])
}

hist(angles, main = "Histogram of projections, p/n = 5")

p = 1000

data6 = matrix(rnorm(n*p), nrow = n)%*%diag(c(1:v, rep(1, p-v)))
pca.6 = prcomp(data6, retx = TRUE, scale = FALSE)

screeplot(pca.6, type = "lines", main = "Scree Plot, p/n = 10")

angles = c()
for (j in 1:B)
{
    data6 = matrix(rnorm(n*p), nrow = n)%*%diag(c(1:v, rep(1, p-v)))
    pca.6 = prcomp(data6, retx = TRUE, scale = FALSE)
    angles[j] = abs(pca.6$rotation[v,1])
}

hist(angles, main = "Histogram of projections, p/n = 10")




###6###

n = 1000
p = 20
d = 4
B = 10000

X = cbind(matrix(rnorm(n*d), ncol = d), matrix(rnorm(n*16, sd = 0.25), ncol = 16))

pc.X = princomp(X, scores = TRUE)
pc.X$sdev[1:4]^2
pc.X$sdev[5:20]^2
screeplot(pc.X, type = "lines", main = "Scree Plot 1 run of experiment")
pc.X$sdev^2/sum(pc.X$sdev^2)
cum.variance = sapply(1:20, function(x) sum(pc.X$sdev[1:x]^2)/sum(pc.X$sdev^2))
cum.variance
pc.X$loadings[,1]

firstpc.individual.cosines.angles = matrix(0, nrow = d, ncol = B)
sines.principal.angles = matrix(0, nrow = d, ncol = B)


for (i in 1:B)
{
	X = cbind(matrix(rnorm(n*d), ncol = d), matrix(rnorm(n*16, sd = 0.25), ncol = 16))
	pr.comp = svd(X)
    firstpc.projections = pr.comp$v[1:4,1]
	firstpc.individual.cosines.angles[,i] = abs(firstpc.projections)
	sines.principal.angles[,i] = svd(t(pr.comp$v[,(d+1):p])%*%diag(20)[,1:4])$d
}

par(mfrow = c(2,2))
hist(firstpc.individual.cosines.angles[1,], freq = FALSE, xlab = "cos(theta_11)",main = "Histogram of cos(theta_11)")

hist(firstpc.individual.cosines.angles[2,], freq = FALSE, xlab = "cos(theta_12)",main = "Histogram of cos(theta_12)")

hist(firstpc.individual.cosines.angles[3,], freq = FALSE, xlab = "cos(theta_13)",main = "Histogram of cos(theta_13)")

hist(firstpc.individual.cosines.angles[4,], freq = FALSE, xlab = "cos(theta_14)",main = "Histogram of cos(theta_14)")

hist(apply(sines.principal.angles,2,max), freq = FALSE, xlab = "max sin(theta_j)", main = "Histogram max sin(theta_j)")

