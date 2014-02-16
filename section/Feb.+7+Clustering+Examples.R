##############################################################################################
## Clustering example code            Stat 154 Spring 2014        Derek Bean     ##
####################################################################################


###Toy Example###
Toydata = matrix(c(2, 6, 3, 4, 3, 8, 4, 7, 6, 2), nrow = 5, byrow = TRUE)

#Cluster the data by k-Medoids with k = 2, using L1 distance

ToyDissMatrix = dist(Toydata, method = "manhattan") #Compute dissimilarity matrix of data based on L1 (Manhattan) norm

library(cluster)

ToyMedoids1 = pam(Toydata, k = 2, diss = FALSE, metric = "manhattan", medoids = c(2,4), do.swap = FALSE)

ToyMedoids2 = pam(ToyDissMatrix, k = 2, diss = TRUE, medoids = c(2, 4), do.swap = FALSE)

#do.swap is set to FALSE to make the algorithm the same as that described in HTF. The method implemented in R is actually a slight improvement by finding a configuration such that NO switching of observations and medoids results in an improved cost. Here we only do switching of medoids and observations *within* already given cluster assignments (this also makes the results conform with what we did by hand)


#Cluster the data using hierarchical (agglomerative) clustering with different linkages

ToyHierCluster = hclust(ToyDissMatrix, method = "complete")

plot(ToyHierCluster)

ToyHierClusterAvg = hclust(ToyDissMatrix, method = "average")

plot(ToyHierClusterAvg)

ToyHierClusterSing = hclust(ToyDissMatrix, method = "single")

plot(ToyHierClusterSing)

###Fun Example###

#1984 U.S. Congressional Voting record: 434 Congresspeoples' votes on 16 bills plus party affiliation. Read the description of the data set at http://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.names

VotingData <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data")

str(VotingData)

#Get rid of subjects with unknown votes
unknownVote = apply(VotingData[,-1], 1, function(x) sum(x=="?"))
VotingDataMod <- VotingData[unknownVote==0,]

str(VotingDataMod)

n = nrow(VotingDataMod)

#Create dissimilarity matrices. I used medoids (fun: try to see if the chosen medoids have any significance vis a vis their voting record, political party, etc. First convert the factor voting variables to binary 0-1 variables

facttobin <- function(x) {x[which(x=="n")] <- 0; x[which(x=="y")] <-1; as.numeric(x)}

VotingDataMod1<-t(apply(VotingDataMod[,-1], 1, facttobin))

#For 0-1 data the L1 and L0 norm coincide. They both add up to the number of bills on which two congresspeople voted differently
dissim.raw = dist(VotingDataMod1, method = "manhattan")

vote.medoids = pam(dissim.raw, k = 2, diss = TRUE)

#Investigate (qualitatively!) how the clustering assignment matches party affiliation
table(vote.medoids$cluster[which(VotingDataMod[,1]=="democrat")])

table(vote.medoids$cluster[which(VotingDataMod[,1]=="republican")])



#Would be nice to visualize the data. Many candidates: do raw PCA and see if any structure is revealed (probably not; the data lie on the vertices of the L1 ball. MDS could map the data points into euclidean space where any number of dimension reducing techniques could be used to visualize the data. I chose to do spectral clustering using the L0 metric to help define an affiinity matrix and computing the graph Laplacian that way.

#Compute the weight matrix
l1norm <- function(x) sum(abs(x))
Weights <- matrix(0, nrow = n, ncol = n)
#Use the usual Gaussian kernel
for (i in 1:n)
{
    for(j in 1:n)
    {
        Weights[i,j] = exp(-(l1norm(VotingDataMod1[i,] - VotingDataMod1[j,])/16)^2)
    }
}
#Yeah, yeah, double for loop is a no-no but I was pressed for time
Degree = diag(rowSums(Weights - diag(n))) #subtract the diagonal elements of Weights (which are equal to 1: why?) and then rowSums (or colSums) will quickly compute the degree node

Degree.inv = solve(Degree)

Laplace.reg = diag(n) - Degree.inv%*%Weights #normalized Laplacian. There are some outliers in the unnormalized Laplacian that made the plots difficult to interpret. Normalization by the degree seemed to help.

Laplace.reg.decomp = svd(Laplace.reg)#extract components

plot(Laplace.reg.decomp$u[,(n-1)], Laplace.reg.decomp$u[,(n-2)], col = c("blue", "red")[VotingDataMod[,1]]) #the smallest eigenvector, because we regularized, is not exactly constant, but nearly so (check this)

#There are many ways to form the affinity matrix: we could play with different kernels, tuning parameters, and threshold the weights by k-nearest neighbors to see if the clusters exhibit more separation. We can also look at different eigenvectors.
