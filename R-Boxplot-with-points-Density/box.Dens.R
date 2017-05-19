# Creator: Raúl Rodríguez Cruces
# Date: April 2016
# Scrip name: R-spaghetti-plot
# This script generates a spaghetti plot from simulated data.
#
# -------------------------------------------------------------------------
#
#### Boxplot with points & Density ####
#
# -------------------------------------------------------------------------

#Generates a set of data
a <- rnorm(80 , mean=5 , sd=10)
b <- rnorm(120 , mean=-5 , sd=6)

#Blank PLot
boxplot(a,b, axes=FALSE,col=NA,border=NA,xlim=c(0,4))

# Plot the points
len <- length(a)
points(jitter(rep(1,len),15),a,col="#6CA6CD64",lwd=4,cex=seq(1,3,length.out = len))
len <- length(b)
points(jitter(rep(2,len),8),b,col="#CD853F64",lwd=4,cex=seq(1,3,length.out = len))

# Boxes
boxplot(a,b, add=TRUE,outline=FALSE,axes=FALSE,xlim=c(0,4),at=c(1,2),las=1,xaxt='n'
        ,main=" ",xlab="",ylab="Value",xlab="Groups"
        ,medcol="darkred", col=c("#6CA6CD32","#CD853F32"),border=c("#6CA6CDFA","#CD853FFA")
        ,col.axis="gray8",col.lab="gray8",col.main="gray8"
        ,whisklty = 2, staplelwd = 5,whisklwd=5,boxlwd=5,medlwd=5,frame.plot=FALSE
        ,cex.axis=1.5,cex.lab=1.2,cex=0.5)
# Axes
axis(1, at=c(1,2), col.axis="gray8",labels=c("a","b"), lty=1, col="gray8", las=1,lwd=3,cex.axis=1.5,col.lab="gray8")
axis(2, at=seq(round(min(a,b),-1),round(max(a,b),-1),10), lty=1, col="gray8",las=1,lwd=3)
mtext("Density",1,line = 1,col="gray8",at = 3,cex=1.5)

#Plot the Density
par(xpd=TRUE)
d=density(a,bw = "SJ")
y=seq(min(a),max(a),length.out = length(d$x))
polygon(x=3+d$y*10,y=y,col="#6CA6CD32",border = "#6CA6CDFA",lwd=5)

d=density(b,bw="SJ")
y=seq(min(b),max(b),length.out = length(d$x))
polygon(x=3+d$y*10,y=y,col="#CD853F32",border = "#CD853FFA",lwd=5)




