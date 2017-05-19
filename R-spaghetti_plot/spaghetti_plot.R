# Creator: Raúl Rodríguez Cruces
# Date: August 2016
# Scrip name: R-spaghetti-plot
# This script generates a spaghetti plot from simulated data.
#
# -------------------------------------------------------------------------
#
#### Spaghetti Plot ####
#
# -------------------------------------------------------------------------

# First we generate a sintetic data frame of 2 groups with diferent means and random standar deviation, 10 observations and 8 variables
values = c()
for (i in 1:5) {
values <- rbind(values,(rnorm(8,mean = 5,sd=runif(1)*10)))}
for (i in 1:5) {
  values <- rbind(values,(rnorm(8,mean = 20,sd=runif(1)*10)))}
group = c(rep(1, 5),rep(2,5))   # Vector of the group
df = data.frame(group, values)  # We combine the values and the group into a Data Frame

# We assign a unique line width, and point size to each observation for easier visualization
n <- length(df[,1])                                       # Number of total observations (rows)
N <- length(df[1,])-1                                     # Number of variables per observation (columns)
colV <- c("cornflowerblue","darkolivegreen3")[df$group]   # Vector of colors
Lwd <- seq(2,4,length.out = n)                            # Vector of line widths
Cex <- seq(0.5,1.5,length.out = n)                        # Vector for point sizes
x <- 1:N                                                  # Position of the variables at the X-axis
ylimit <- c(floor(min(df)/5)*5,ceiling(max(df)/5)*5)      # Limits of the Y-axis

# Empty plot
plot(rep(1,n),df$X1,ylim=ylimit,xlim = c(0,N),col=NA, ylab="",xlab="",main="Spaghetti Plot",col.main="gray25",cex.main=3,cex.axis=1.5,axes=F)
axis(1, at=x, col.axis="gray25",labels=colnames(df[2:(N+1)]), lty=1, col="gray25", las=1,lwd=3,cex.axis=1.5,col.lab="gray25")
axis(2, at=seq(ylimit[1],ylimit[2],10), lty=1, col="gray25",col.axis="gray25",las=1,lwd=3,cex.axis=1.5)

# Adds the lines and points
x1 = rep(x[1],n)
for (i in 1:(N-1)) { x2 <- rep(x[i+1],n)
    for (j in 1:n) { 
                     lines(c(x1[j],x2[j]),c(df[,2:(N+1)][j,i],df[,2:(N+1)][j,i+1]),col=colV[j],lwd=Lwd[j])
                     points(x1[j],df[,2:N][j,i],col=colV[j],pch=19,cex=Cex[j])}
    x1 <- x2}
points(x1,df[,N+1],col=colV,cex=Cex,pch=19)

