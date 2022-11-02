###############################################################
#                                                             #
#     Cluster Analysis with R Using SAS.BASEBALL dataset      #
#                                                             #
###############################################################


######################################################
### install packages we'll be using for clustering ###
######################################################

#install.packages("readr")
#install.packages("cluster")
#install.packages("dbscan")


###################################
### Import and prepare dataset  ###
###################################
#ZOOM ALL THE WAY OUT TO BE ABLE TO VIEW DATA!!
# read in data
library(readr)
BASEBALL <- read_csv("BASEBALL.csv")

# remove observations with missing values
BASEBALL = na.omit(BASEBALL)

# remove non-numeric variables, and use player's names as row names
BASEBALL.names = BASEBALL[,1]
BASEBALL = BASEBALL[,-c(1,2,16,17,18,23)]
rownames(BASEBALL) = t(BASEBALL.names)


###############################
### Hierarchical Clustering ###
###############################

par(mfrow=c(1,1))

## average ##
hier.av = hclust(dist(BASEBALL), method="average")
plot(hier.av)

## single ##
hier.sin = hclust(dist(BASEBALL), method="single")
plot(hier.sin)

## complete ##
hier.com = hclust(dist(BASEBALL), method="complete")
plot(hier.com)


#############################################
### Perform Principal Components Analysis ### 
#############################################

# perform PCA
pca = prcomp(BASEBALL, scale.=T)

# calculate the eigenvalues
var.pca = sum((pca$sdev)^2)
eig.pca = (pca$sdev)^2/var.pca

# sum the first two to determine how much variation they capture
sum(eig.pca[1:2]) # they capture 0.69%, which should be plenty 

# create our dimension-reduced dataset using only the first 2 components
pca.data = pca$x[,1:2]

# plot the eigenvalues and the dataset using the first two components
par(mfrow=c(1,2))
plot(eig.pca, main="Eignenvalues from PCA of BASEBALL Data")
plot(pca.data, main="First and Second Principal Components")


##################################
### Perform K-means clustering ###
##################################

library(cluster)

# calculate gap statistic
Gap = clusGap(pca.data, FUN=kmeans, nstart=25, K.max=20)

# plot gap statistic
par(mfrow=c(1,1))
plot(Gap, main="Gap Statistic Plot") # 3, 6, 8 seem like possible options

# perform k-means with each of our possible choices for k
k.means.3 = kmeans(pca.data, 3, nstart=25)
k.means.6 = kmeans(pca.data, 6, nstart=25)
k.means.8 = kmeans(pca.data, 8, nstart=25)


# plot the clustering results along with our gap statistic plot
par(mfrow=c(2,2))

plot(Gap, main="Gap Statistic Plot")

plot(pca.data, col = k.means.3$cluster, main="K-Means K=3")
points(k.means.3$centers, col = 1:3, pch = 8, cex = 2)

plot(pca.data, col = k.means.6$cluster, main="K-Means K=6")
points(k.means.6$centers, col = 1:6, pch = 8, cex = 2)

plot(pca.data, col = k.means.8$cluster, main="K-Means K=8")
points(k.means.8$centers, col = 1:8, pch = 8, cex = 2)


###############################
### DBSCAN ####################
###############################

library(dbscan)

# create k-nearest-neighbor plot to determine epsilon. Note here K=C.
par(mfrow=c(1,1))
kNNdistplot(pca.data, k=4) # looks like 0.9, 1, 1.1 could be good choices for epsilon

# perform DBSCAN with each of our three possibilities for epsilon
density.9 = dbscan(pca.data, minPts = 4, eps=0.9)#change eps do not change minpts
density.1 = dbscan(pca.data, minPts = 4, eps=1)
density.1.1 = dbscan(pca.data, minPts = 4, eps=1.1)
density.8 = dbscan(pca.data, minPts = 4, eps=.5)
# plot results

par(mfrow=c(2,2))

kNNdistplot(pca.data, k=4)

plot(pca.data, type="n", main="Epsilon=0.8")
points(pca.data, col = density.8$cluster, pch=16)
points(pca.data[density.8$cluster==0,], pch = 16, col = "grey")

plot(pca.data, type="n", main="Epsilon=0.9")
points(pca.data, col = density.9$cluster, pch=16)
points(pca.data[density.9$cluster==0,], pch = 16, col = "grey")

plot(pca.data, type="n", main="Epsilon=1")
points(pca.data, col = density.1$cluster, pch=16)
points(pca.data[density.1$cluster==0,], pch = 16, col = "grey")

plot(pca.data, type="n", main="Epsilon=1.1")
points(pca.data, col = density.1.1$cluster, pch=16)
points(pca.data[density.1.1$cluster==0,], pch = 16, col = "grey")
#grey = outliers
#black = 1 cluster
#red = a different cluster
#green = another different cluster

