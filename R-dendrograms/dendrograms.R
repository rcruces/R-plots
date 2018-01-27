install.packages("dendextend")
install.packages("circlize")
require(dendextend)
require(circlize)
# https://cran.r-project.org/web/packages/dendextend/vignettes/introduction.html

#### Create a dendrogram #### 
# Create a matrix from Iris dataset
# We remove the row 143 because is repeated
data.mtx <- as.matrix(scale(iris[-143,1:4]))

# Obtain a Euclidean Distance matrix
d <- dist(data.mtx,method = "euclidean")

# Cluster the data with Ward Method
hcc <- hclust(d,method = "ward.D")

# Define THREE clusters
clusters <- cutree(hcc,k = 3)

# Creates a Dendrogram Object
dend <- as.dendrogram(hcc)

# Plot of the Default Dengrogram
plot(dend)
par(mfrow=c(1,2))
# Multi Dimensional Scaling Plot
Color<-c("#ec7531ff","#4b2a00ff","#b40000ff")

library(MASS)
mds = isoMDS(d)
plot(mds$points,las=1,bty='n',pty='l',pch = 21, cex = 3, bg=c("#EC753196", "#4B2A0096", "#B4000096")[clusters],col = Color[clusters], xlab = "X", ylab = "Y",main="Multi Dimensional Scaling") 

text(mds$points, labels = rownames(mds$points), cex = 1)


# Modify the dendrogram to have some colors in the branches and labels
dend <- dend %>% 
  color_branches(dend,col=c("#ec7531ff","#b40000ff","#4b2a00ff"),k=3) %>% 
  color_labels(dend,col=c("#ec7531ff","#b40000ff","#4b2a00ff"),k=3) %>% 
  set("branches_lwd", 3)

# Plot normal dendrogram
plot(hcc,hang = 0.03, cex = 0.6)
plot(hcc,hang = -1, cex = 0.6)

# Horizontal dendrogram with colors
plot(dend, cex = 0.6,horiz = TRUE)

# Phylogenetic trees
# ape (Analyses of Phylogenetics and Evolution)
library("ape")

# Unrooted
plot(as.phylo(dend), type = "unrooted", cex = 0.6,no.margin = TRUE)

# Fan
plot(as.phylo(dend), type = "fan")

# Fan
plot(as.phylo(dend), type = "radial")

# Cut the dendrogram into 3 clusters
clus4 = cutree(dend, 3)
plot(as.phylo(dend), type = "fan", tip.color = Color[clus4],
     label.offset = 4, cex = 0.7,main="Phylo")

# plot the radial plot
par(mar = rep(0,4))

# circlize_dendrogram(dend, dend_track_height = 0.8) 
circlize_dendrogram(dend, labels_track_height = NA, dend_track_height = .4) 

