# Package for ploting heatmaps
require(gplots)

# iris
df <- iris

# Z-scores each column
zscore <- function(V) {
  V <- as.numeric(V)
  Vz <- (V-(mean(V)))/sd(V)
  return(Vz)
}
df[,1:4] <- apply(iris[,1:4],2,zscore)

# following code limits the lowest and highest color to 5%, and 95% of your range, respectively
df.matrix <- as.matrix(df[,1:4])
quantile.range <- quantile(df.matrix, probs = seq(0, 1, 0.01))
palette.breaks <- seq(quantile.range["5%"], quantile.range["95%"], 0.1)

# Find optimal divergent color palette (or set own)
color.function <- colorRampPalette(c("navy","dodgerblue4","dodgerblue2","deepskyblue","lightskyblue","gray70","red","firebrick2","firebrick3","firebrick","firebrick4"))
color.palette  <- color.function(length(palette.breaks) - 1)

heatmap.2(df.matrix,
          main = "Data",
          density.info="none",  
          trace="none",
          key.title = "",
          dendrogram='column',
          Rowv=FALSE,
          Colv=TRUE,
          RowSideColors = c("yellow","purple","forestgreen")[as.factor(df$Species)],
          col    = color.palette,
          breaks = palette.breaks,
          labCol = colnames(df.matrix)
)
