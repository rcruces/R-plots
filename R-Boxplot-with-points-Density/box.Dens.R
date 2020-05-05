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

#Generates two vectors
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


# -------------------------------------------------------------------------
#
#### Functoin: Boxplot with points & Density ####
#
# -------------------------------------------------------------------------
#Generates a set of data
# define numbers of groups
N <- 5
x <- rnorm(100*N , mean=0 , sd=10)
Gr <- as.factor(sample(1:N, 100*N, replace = TRUE))

box.density <- function(x, Gr=NULL, x.disp=0.5, p.cex=1, lwd=1, pch=19, xlim=c(0,N+1.5),
                        Col=NULL, main="Box-Density plot", xlab='Group', ylab='x', medcol="black",
                        col.axis="gray15",col.lab="gray8",col.main="gray8",
                        whisklty = 2, staplelwd = 2, whisklwd=2,boxlwd=2, medlwd=3,frame.plot=FALSE,
                        cex.axis=1.5,cex.lab=1.2,cex=0.5, density=TRUE
                        ) {
        if (is.null(Gr)) {Gr <- as.factor(rep(1,length(x)))}
        if (class(Gr)!="factor") {stop("[ERROR]  Gr must be a factor defining group belonging")}
        N <- length(levels(Gr))
        if (is.null(Col)) {Col <- rainbow(N, alpha = 1)}
        # if horizontal==TRUE
        # 
        # Blank PLot
        boxplot(x~Gr, axes=FALSE,col=NA,border=NA,xlim=xlim,
                main=main, xlab=xlab, ylab=ylab)
        
        levels(Gr)
        # Plot the points
        for (i in 1:N) { inx <- Gr==levels(Gr)[i]
                len <- length(x[inx])
                points(i+rnorm(len , mean=0 , sd=x.disp), x[inx], 
                col=scales::alpha(Col[i],0.6), lwd=lwd,cex=p.cex, pch=pch, bg="white")
                
                # Plot the density
                # density value and label
                if (density==TRUE){
                        par(xpd=TRUE)
                        d=density(x[inx], bw = "SJ")
                        y=seq(min(x[inx]),max(x[inx]),length.out = length(d$x))
                        polygon(x=N+1+d$y*10,y=y,col=scales::alpha(Col[i],0.4),border = Col[i], lwd=1.5)        
                }
                }
        # Boxes
        boxplot(x~Gr,  add=TRUE, outline=FALSE, axes=FALSE, xlim=xlim, at=1:N, las=1, xaxt='n'
                ,medcol=medcol, col=scales::alpha(Col, 0.3), border=Col
                ,col.axis=col.axis, col.lab=col.lab, col.main=col.main
                ,whisklty = whisklty, staplelwd = staplelwd, whisklwd=whisklwd
                ,boxlwd=boxlwd, medlwd=medlwd, frame.plot=frame.plot
                ,cex.axis=cex.axis, cex.lab=cex.lab, cex=cex)
        # Axes
        axis(1, at=1:N, col.axis="gray8",labels=levels(Gr), lty=1, col="gray8", las=1,lwd=2,cex.axis=1,col.lab="gray8")
        axis(2, at=seq(round(min(x),-1),round(max(x),-1),10), lty=1, col="gray8",las=1,lwd=2)
        
}

box.density(x, Gr, x.disp=0.15, Col=viridis::viridis(5), density = TRUE)
