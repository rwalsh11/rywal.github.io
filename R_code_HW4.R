###############################################################
#                                                             #
#       Homework 4: Cluster Analysis with R                   #
#                                                             #
###############################################################


######################################################
### install packages we'll be using for clustering ###
######################################################

# Note: if you've already installed these packages, there is no need to install them again!

#install.packages("readr")
#install.packages("cluster")
#install.packages("dbscan")


###################################
### Import and prepare dataset  ###
###################################

# read in data -- Note you will have to change this to the path for the file on YOUR computer
library(readr)
library(dbscan)
library(cluster)
CARS <- read_csv("cars.csv")

# remove observations with missing values
CARS = na.omit(CARS)
ncol(CARS)
count(CARS)
# remove non-numeric variables, use only cars with Types SUV, Truck or Sports

a = which(CARS[,3]=="Sports")
b = which(CARS[,3]=="SUV")
c = which(CARS[,3]=="Truck")

CARS = CARS[c(a,b,c),]
CARS.type = CARS[,3]
CARS = CARS[,-c(1:5)]


#############################################
### Perform Principal Components Analysis ### 
#############################################

# perform PCA
pca = prcomp(CARS, scale.=T)

# calculate the eigenvalues
var.pca = sum((pca$sdev)^2)
eig.pca = (pca$sdev)^2/var.pca

# shows values of eigenvalues for each component
eig.pca

par(mfrow=c(1,1))

# plot the eigenvalues of the dataset
plot(eig.pca, main="Eignenvalues from PCA of CARS Data")

# sum the first two to determine how much variation they capture
sum(eig.pca[1:2]) 

# sum the first three to determine how much variation they capture
sum(eig.pca[1:3]) 

# sum the first four to determine how much variation they capture
sum(eig.pca[1:4]) 

# create our dimension-reduced dataset using only the number of components that we need.
# Replace NUMBER with how many you wish to use

pca.data = pca$x[,1:2]

##################################
### Perform K-means clustering ###
##################################

library(cluster)

# calculate gap statistic
Gap = clusGap(pca.data, FUN=kmeans, nstart=25, K.max=20)

# plot gap statistic
plot(Gap, main="Gap Statistic Plot") 

# perform k-means by replacing K-VALUE with the number you would like to use
#change numeric mdidle value to establish K-Value
k.means = kmeans(pca.data, 13, nstart=25)


# To plot, change col = 1: K-VALUE to the number you used for K
# Note that triangle=SUV, circle=Truck, square=Sports car

plot(pca.data, col = k.means$cluster, main="K-Means", type="n")
points(k.means$centers, col = 1:K-VALUE, pch = 8, cex = 2)
points(pca.data[which(CARS.type=="SUV"),], col = k.means$cluster[which(CARS.type=="SUV")], pch=2)
points(pca.data[which(CARS.type=="Truck"),], col = k.means$cluster[which(CARS.type=="Truck")], pch=1)
points(pca.data[which(CARS.type=="Sports"),], col = k.means$cluster[which(CARS.type=="Sports")], pch=0)


###############################
### DBSCAN  Clustering ########
###############################

library(dbscan)

# create k-nearest-neighbor plot to determine epsilon. Note here K=C.
par(mfrow=c(1,1))
kNNdistplot(pca.data, k=4)

# perform DBSCAN by filling in your choice for EPSILON
#vary epsilon
density = dbscan(pca.data, minPts = 4, eps=.7)

# plot results

plot(pca.data, type="n", main="DBSCAN")
points(pca.data[which(CARS.type=="SUV"),], col = density$cluster[which(CARS.type=="SUV")], pch=2)
points(pca.data[which(CARS.type=="Truck"),], col = density$cluster[which(CARS.type=="Truck")], pch=1)
points(pca.data[which(CARS.type=="Sports"),], col = density$cluster[which(CARS.type=="Sports")], pch=0)
points(pca.data[density$cluster[which(CARS.type=="SUV")]==0,], pch = 2, col = "grey")
points(pca.data[density$cluster[which(CARS.type=="Truck")]==0,], pch = 1, col = "grey")
points(pca.data[density$cluster[which(CARS.type=="Sports")]==0,], pch = 0, col = "grey")



