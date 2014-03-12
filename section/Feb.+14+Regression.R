##############################################################################################
## Regression Examples            Stat 154 Spring 2014        Derek Bean     ##
####################################################################################


##Sparse data
regf <- function(x) 0.5*cos(8*x) + 0.5*x

grid <- seq(0, 1, length.out = 1000)

plot(grid, sapply(grid, regf), pch = 20, col = "blue")

l2dist <- function(x) sum(x^2)

##Some useful code to do nearest neighbors regression. No built in warning for k > n.
knearest <- function(x, datax, datay, k)
{
    neighbor.indices = sort(sapply(datax - x, l2dist), index.return = TRUE)
    k.nearest = neighbor.indices$ix[1:k]
    mean(datay[k.nearest])
}

n = 10

#One run of the simulation
data.xpoints = runif(n, 0, 1)
data.ypoints = sapply(data.xpoints, regf) + rnorm(n)

plot(data.xpoints, data.ypoints, pch = '')
points(data.xpoints, data.ypoints, pch = 'x', col = "green")


#Get a visual sense of the variability of 1-nearest neighbors

for (i in 1:50)
{
    data.xpoints
    data.xpoints = runif(n, 0, 1)
    data.ypoints = sapply(data.xpoints, regf) + rnorm(n)
    
    plot(data.xpoints, data.ypoints, pch = '')
    points(data.xpoints, data.ypoints, pch = 'x', col = "green")
    lines(grid, sapply(grid, knearest, datax = data.xpoints, datay = data.ypoints, k = 1), col = "red", lwd = 2)

}


#Variability of n-nearest neighbors (very stable)

for(i in 1:50)
{
    data.xpoints = runif(n, 0, 1)
    data.ypoints = sapply(data.xpoints, regf) + rnorm(n)
    
    plot(data.xpoints, data.ypoints, pch = '')
    points(data.xpoints, data.ypoints, pch = 'x', col = "green")
    #lines(grid, sapply(grid, regf), lwd = 2, col = "blue")
    abline(h = mean(data.ypoints), col = "red")
}

#Variability of least squares regression line (somewhat stable)
for(i in 1:50)
{
    data.xpoints = runif(n, 0, 1)
    data.ypoints = sapply(data.xpoints, regf) + rnorm(n)
    
    lin.reg = lm(data.ypoints~data.xpoints)
    
    plot(data.xpoints, data.ypoints, pch = '')
    points(data.xpoints, data.ypoints, pch = 'x', col = "green")
    #lines(grid, sapply(grid, regf), lwd = 2, col = "blue")
    abline(lin.reg, col = "red")
}

#variability of least squares quadratic fit
for(i in 1:50)
{
    data.xpoints = runif(n, 0, 1)
    data.ypoints = sapply(data.xpoints, regf) + rnorm(n)
    
    data.sq.xpoints = data.xpoints^2
    quad.reg = lm(data.ypoints~data.xpoints + data.sq.xpoints)
    
    plot(data.xpoints, data.ypoints, pch = '')
    points(data.xpoints, data.ypoints, pch = 'x', col = "green")
    lines(grid, predict(quad.reg, newdata = list(data.xpoints = grid, data.sq.xpoints = grid^2)), lwd = 2, col = "red")
}

#Choose a test point for predictive estimation

testpoint.x = runif(1, 0, 1)
estimates.fx = c()
eps = 0.1

#data.xpoints = seq(from = (1/n), to = 1, length.out = n) Fixed vs. random points.


knearestsims <- matrix(0, nrow = 1000, ncol = length(grid))
for(i in 1:1000)
{
    data.xpoints = runif(n, 0, 1)
    data.ypoints = sapply(data.xpoints, regf) + rnorm(n)
    knearestsims[i,] = sapply(grid, knearest, datax = data.xpoints, datay = data.ypoints, k = 1)
    estimates.fx[i] = knearest(testpoint.x, data.xpoints, data.ypoints, 1)
}

#Plot the average of the 1-nearest neighbor functions to get a sense of the bias. Very low.
plot(grid, sapply(grid, regf), pch= '')
lines(grid, sapply(grid, regf), lwd = 2, col = "blue")
lines(grid, colMeans(knearestsims), lwd = 2, col = "red")
points(testpoint.x, regf(testpoint.x), pch = 'x', col = "green")

#Compute statistics on the residuals for our estimates of the function at the test point. Bias and variance both play a role.
knearest.resid.x = estimates.fx - regf(testpoint.x)
mean(knearest.resid.x^2)
mean(abs(knearest.resid.x) > eps)
#hist(knearest.resid.x)

meansims <- c()
for(i in 1:1000)
{
    data.xpoints = runif(n, 0, 1)
    data.ypoints = sapply(data.xpoints, regf) + rnorm(n)
    meansims[i] = mean(data.ypoints)
}

#the simple mean is highly biased for an oscillating function
plot(grid, sapply(grid, regf), pch= '')
lines(grid, sapply(grid, regf), lwd = 2, col = "blue")
abline(h = mean(meansims), col = "green")
points(testpoint.x, regf(testpoint.x), pch = 'x', col = "green")


mean.resid.x = meansims - regf(testpoint.x)
mean(mean.resid.x^2)
mean(abs(mean.resid.x) > eps)
hist(mean.resid.x)

#line is biased but does pick up linear trend in the function. So it can knock out linear bias, but not any bias coming from higher order terms of underlying function.
linregsims <- matrix(0, nrow = 1000, ncol = length(grid))
linestimates.fx <- c()
for (i in 1:1000)
{
    data.xpoints = runif(n, 0, 1)
    data.ypoints = sapply(data.xpoints, regf) + rnorm(n)
    lin.mod = lm(data.ypoints~data.xpoints)
    linregsims[i,] = predict(lin.mod, newdata = list(data.xpoints = grid))
    linestimates.fx[i] = predict(lin.mod, newdata = list(data.xpoints = testpoint.x))
}

plot(grid, sapply(grid, regf), pch= '')
lines(grid, sapply(grid, regf), lwd = 2, col = "blue")
lines(grid, colMeans(linregsims), lwd = 2, col = "red")
points(testpoint.x, regf(testpoint.x), pch = 'x', col = "green")


lin.resid.x = linestimates.fx - regf(testpoint.x)
mean(lin.resid.x^2)
mean(abs(lin.resid.x) > eps)
#hist(lin.resid.x)


quadregsims <- matrix(0, nrow = 1000, ncol = length(grid))
quadestimates.fx <- c()
for (i in 1:1000)
{
    data.xpoints = runif(n, 0, 1)
    data.sq.xpoints = data.xpoints^2
    data.ypoints = sapply(data.xpoints, regf) + rnorm(n)
    quad.mod = lm(data.ypoints~data.xpoints + data.sq.xpoints)
    quadregsims[i,] = predict(quad.mod, newdata = list(data.xpoints = grid, data.sq.xpoints = grid^2))
    quadestimates.fx[i] = predict(lin.mod, newdata = list(data.xpoints = testpoint.x, data.sq.xpoints = testpoint.x^2))
}

#Quadratic least squares less bias - can pick up quadratic terms.
plot(grid, sapply(grid, regf), pch= '')
lines(grid, sapply(grid, regf), lwd = 2, col = "blue")
lines(grid, colMeans(quadregsims), lwd = 2, col = "red")
points(testpoint.x, regf(testpoint.x), pch = 'x', col = "green")


quad.resid.x = quadestimates.fx - regf(testpoint.x)
mean(quad.resid.x^2)
mean(abs(quad.resid.x) > eps)
#hist(quad.resid.x)


####Try:
n = 100

n = 1000

#Different values of K for k nearest neighbors

#Higher order polynomials





