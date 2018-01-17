# "Heatmap of Iris Database"
# author: "Raúl RC"


#### Heatmap ####  
# Uploads the library
require(gplots)

# Iris data frame should be transformed to a matrix 
iris.mtx <- as.matrix(iris[,1:4])

# Heatmap with the default characteristics
heatmap.2(iris.mtx)

# ------------------------------------ #
## Heatmap Customization  

# ------------------------------------ #
#### Color map #### 
# Vector of custom colors
pinzon <- c("#ffdfbaff","#ec7531ff","#9e2f0eff","#4b2a00ff","#64070cff","#b40000ff","#ed1c17ff","#f7202fff")

# Funtion that interpolates the vector of colors
pinzon.Ramp <- colorRampPalette(pinzon)

# Heatmap with our custom colormap
heatmap.2(iris.mtx, col=pinzon.Ramp(20))

# ------------------------------------ #
#### Data Scaling  #### 
# Standardizing variables
iris.scaled <- scale(iris.mtx)

# Heatmap with our custom colormap & Standarized variables 
heatmap.2(iris.scaled, col=pinzon.Ramp(20))

# ------------------------------------ #
#### Colormap Optimization ####  
# Function that optimizes the color distribution
optim.color <- function(Data,Colors) {
  #       Data: Is the matrix of data to plot
  #       Colors: Vector of colors we want to use
  # Input
  mtx <- as.matrix(Data)
  # Following code limits the lowest and highest color to 5%, and 95% of your range, respectively
  quantile.range <- quantile(mtx, probs = seq(0, 1, 0.01))
  palette.breaks <- seq(quantile.range["5%"], quantile.range["95%"], 0.1)
  # Find optimal divergent color palette (or set own)
  color.function <- colorRampPalette(Colors)
  color.palette  <- color.function(length(palette.breaks) - 1)
  # Returns a list with the color map and the optimal number of color breaks
  return(list(color.palette=as.vector(color.palette),palette.breaks=as.vector(palette.breaks)))
}

# We apply the our color optimization function to the scaled matrix
col.map <- optim.color(iris.scaled,pinzon)

# Heatmap with Data scaled and custom colors optimized
heatmap.2(iris.scaled, col=col.map$color.palette, breaks = col.map$palette.breaks)

# ------------------------------------ #
#### More Arguments  #### 
# Vector of colors per group
col.group <- c("#7331adff","#6d62d9ff","#2c1563ff")[as.factor(iris$Species)]

# Legend of the group colors
plot(rep(0.5,3),1:3,xlim=c(0,2),ylim=c(0.5,3.5),pch=15,cex=8,col=c("#7331adff","#6d62d9ff","#2c1563ff"),axes = F,bty='n',xlab = "",ylab = "")
text(rep(0.75,3),1:3,levels(iris$Species),cex = 3,pos =4)

# More Customizations
heatmap.2(iris.scaled,                 # Data matrix
          main = "Iris Dataset",       # Main Title
          density.info="histogram",    # should be one of “histogram”, “density”, “none”
          trace="none",                # Draw the trace over "column", "row" or "both"
          tracecol="white",            # Color of the Trace
          key.title = "Histogram",     # Title of the color key
          dendrogram='both',           # Draw dendrogram: "column", "row", "both"
          Rowv=TRUE,
          Colv=TRUE,
          RowSideColors = col.group,        # Group color
          col    = col.map$color.palette,   # Colormap
          breaks = col.map$palette.breaks,  # Color breaks
          labCol = colnames(iris.scaled),   # Column labels
          srtCol=1,                         # Column names rotation 1=0°,0=90°
          cexCol=1.5,                       # Column names font size
          adjCol = 0.5                      # Column names position 0.5=centered
)


# ------------------------------------ #
#### Heatmap per cluster ####
# Transpose the distance matrix and clusters the columns
hc.cols <- hclust(dist(t(iris.scaled)),method = "ward.D2")

# Clusterization of the rows
hc.rows <- hclust(dist(iris.scaled))

# Draw a heatmap per each cluster
# we define three cluster K=3
iris.cluster <- data.frame()
col.cluster <- c()
for (K in 1:3) {
  # Conditional for cluster selection
  rows.cluster <- cutree(hc.rows,k=3)==K
  # New cluster matrix
  iris.cluster <- rbind(iris.scaled[rows.cluster,],iris.cluster)
  # New cluster group color
  col.cluster <- c(col.group[rows.cluster],col.cluster)
  
  heatmap.2(iris.scaled[rows.cluster,], 
            main = paste("Iris Cluster",K), 
            density.info="histogram",
            trace="none",
            tracecol="white",
            key.title = "Histogram",
            dendrogram='both',
            Rowv=TRUE,
            Colv=as.dendrogram(hc.cols),
            RowSideColors = col.group[rows.cluster],
            col=col.map$color.palette, 
            breaks = col.map$palette.breaks,
            srtCol=1, cexCol=1.5, adjCol = 0.5)
}

# Cluster Heatmap 
heatmap.2(as.matrix(iris.cluster), 
          main = "Iris Clusters", 
          density.info="histogram",
          key = FALSE,           # Removes the color Key
          trace="column",
          tracecol="white",
          dendrogram='both',
          labRow = NA,           # Removes the row labels
          Rowv=TRUE,
          Colv=as.dendrogram(hc.cols),
          RowSideColors = col.cluster,
          col=col.map$color.palette, 
          breaks = col.map$palette.breaks,
          srtCol=1, cexCol=1.5, adjCol = 0.5)