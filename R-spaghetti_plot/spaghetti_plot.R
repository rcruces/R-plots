# Creator: Raúl Rodríguez Cruces
# Date: August 2016
# Modified: January 2018
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
n=10   # Number of observations
V=8    # Number of variables
for (i in 1:n) {
  values <- rbind(values,(rnorm(V,mean = 5,sd=runif(1)*10)))}
for (i in 1:n) {
  values <- rbind(values,(rnorm(V,mean = 20,sd=runif(1)*10)))}
group = c(rep(1, n),rep(2,n))   # Vector of the group
df = data.frame(group, values)  # We combine the values and the group into a Data Frame

# We assign a unique line width, and point size to each observation for easier visualization
Nr <- length(df[,1])                                       # Number of total observations (rows)
Nc <- length(df[1,])-1                                     # Number of variables per observation (columns)
colV <- c("cornflowerblue","darkolivegreen3")[df$group]    # Vector of colors
Lwd <- seq(2,4,length.out = Nr)                            # Vector of line widths
Cex <- seq(0.5,1.5,length.out = Nr)                        # Vector for point sizes
x <- 1:Nc                                                  # Position of the variables at the X-axis
ylimit <- c(floor(min(df)/5)*5,ceiling(max(df)/5)*5)       # Limits of the Y-axis

# Empty plot
plot(rep(1,Nr),df$X1,ylim=ylimit,xlim = c(0,Nc),col=NA, ylab="",xlab="",main="Spaghetti Plot",col.main="gray25",cex.main=3,cex.axis=1.5,axes=F)
axis(1, at=x, col.axis="gray25",labels=colnames(df[2:(Nc+1)]), lty=1, col="gray25", las=1,lwd=3,cex.axis=1.5,col.lab="gray25")
axis(2, at=seq(ylimit[1],ylimit[2],10), lty=1, col="gray25",col.axis="gray25",las=1,lwd=3,cex.axis=1.5)

# Add the lines
for (r in 1:Nr) {lines(x,df[r,2:9],col=colV[r],lwd=Lwd[r])}

# Add the points
for (r in 1:Nr) {points(x,df[r,2:9],lwd=Lwd[r],col=colV[r],cex=Cex[r],pch=19)}

