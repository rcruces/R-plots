# title: "R Custom Colormaps"
# author: "Raúl RC"

# ---------------------------------------------------------------------------- #
#### Function - optimizing the color #### 
optim.color <- function(Data,Colors) {
  mtx <- as.matrix(Data)
  # Following code limits the lowest and highest color to 5%, and 95% of your range, respectively
  quantile.range <- quantile(mtx, probs = seq(0, 1, 0.01))
  palette.breaks <- seq(quantile.range["5%"], quantile.range["95%"], 0.1)
  # Find optimal divergent color palette (or set own)
  color.function <- colorRampPalette(Colors)
  color.palette  <- color.function(length(palette.breaks) - 1)
  return(list(color.palette=as.vector(color.palette),palette.breaks=as.vector(palette.breaks)))
}

# ---------------------------------------------------------------------------- #
#### Function - Color Palette Visualization #### 
color.test <- function(color.vec) {
  n=length(color.vec)
  barplot(rep(1,n),space = 0,border = NA, axes = F, col = color.vec, main=deparse(substitute(color.vec)))
}

# ---------------------------------------------------------------------------- #
#### Function - Color Interpolation Visualization #### 
# Function that plot the circular interpolation of a given color vector
plot.palette <- function(color.vec,N) {
  # col.vector - vector of colors as input
  # N - the number of out colors desired
  color.pal <- colorRampPalette(c(color.vec,color.vec[1]))
  pie(rep(1,N),col = color.pal(N), labels = NA,border = NA,radius = 4, main=deparse(substitute(color.vec)))
}


# ---------------------------------------------------------------------------- #
#### Function for saving the colormaps as csv for matlab use #### 
save.colmap <- function(color.vec,outfile){
  Colmap <- colorRampPalette(color.vec)
  Colmap<-t(col2rgb(Colmap(256)))
  Colmap<-Colmap/256
  write.csv(Colmap,file=outfile,row.names=FALSE,quote = FALSE) }


# ---------------------------------------------------------------------------- #
#### This function belong to the package qgraph #### 
addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}
