# Multidimensional Scaling

# Create a matrix from Iris dataset
# We remove the row 143 because is repeated
data.mtx <- as.matrix(scale(iris[-143,1:4]))

# Obtain a Euclidean Distance matrix
d <- dist(data.mtx,method = "euclidean")

# Cluster the data with Ward Method
hcc <- hclust(d,method = "ward.D")

# Define THREE clusters
clusters <- cutree(hcc,k = 3)

# Multi Dimensional Scaling Plot
Color<-c("#ec7531ff","#4b2a00ff","#b40000ff")

library(MASS)
mds = isoMDS(d)
plot(mds$points,las=1,bty='n',pty='l',pch = 21, cex = 3, bg=c("#EC753196", "#4B2A0096", "#B4000096")[clusters],col = Color[clusters], xlab = "X", ylab = "Y",main="Multi Dimensional Scaling") 

text(mds$points, labels = rownames(mds$points), cex = 1, col=c("#EC753196", "#4B2A0096", "#B4000096")[clusters])
