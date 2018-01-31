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
Color<-c("deeppink","forestgreen","red3")

# Multi Dimensional Scaling
library(MASS)
mds = isoMDS(d)

# plots the color
plot(mds$points,las=1,bty='n',pty='l',pch = 21, cex = 4, bg=c("#FF149364","#228B2264","#CD000064")[clusters],col = Color[clusters], xlab = "X", ylab = "Y",ylim=c(-3,4),xlim=c(-3,4),main="Multi Dimensional Scaling") 
# Add color labels to each observation
text(mds$points, labels = rownames(mds$points), cex = 0.75, col=Color[clusters])

