#### PACKAGES ####
library("dendextend")  
library("circlize")
library("ape")

#### Default Dendrogram ####
# Create a matrix from Iris dataset
# We remove the row 143 because is repeated
data.mtx <- as.matrix(iris[-143,1:4])

# Obtain a Euclidean Distance matrix
D <- dist(data.mtx,method = "euclidean")

# Cluster the data with Ward Method
ward.clust <- hclust(D,method = "ward.D")

# Plot of the Default Dengrogram
plot(ward.clust)

#### DENDROGRAM with ape library ####
# Manually Defines the cluster of k=3
clus3 <- cutree(ward.clust, 3)

# Colors for each cluster k=3
Color<-c("royalblue1","purple4","forestgreen")

# Types of phylograms from ape library
types <- c("phylogram", "cladogram", "fan", "unrooted", "radial")

# Plots the dendrograms, tip.color= labels color matching the cluster
par(mfrow=c(1,3))
for (i in types) {plot(as.phylo(ward.clust), 
                       type= i, 
                       tip.color = Color[clus3], 
                       edge.color = "gray35", 
                       edge.width = 1.5,
                       main=paste("Type:",i))}

#### Dendrogram Object + `dendextend` library ####
# Creates a Dendrogram Object
dend.obj <- as.dendrogram(ward.clust)

# Modify the dendrogram to have cluster-based colors in the branches and labels
dend.obj <- dend.obj %>% 
  color_branches(dend.obj,col=c("royalblue1","forestgreen","purple4"),k=3) %>% 
  color_labels(dend.obj,col=c("royalblue1","forestgreen","purple4"),k=3) %>% 
  set("branches_lwd", 3)

# Plot normal dendrogram
par(mfrow=c(1,3))
plot(dend.obj, cex = 0.6, main="Default: dendextend")

# Triangle type Colored Dendrogram
plot(dend.obj, type = "triangle", horiz = FALSE, main="Triangle Type")

# Horizontal Colored Dendrogram
plot(dend.obj, type = "rectangle", horiz = TRUE, main="Horizontal")

#### Circular dendrogram: circlize ####
# Only one line. Try to change the dend_track_height parameter.
circlize_dendrogram(dend.obj, labels_track_height = NA, dend_track_height = 0.6) 
