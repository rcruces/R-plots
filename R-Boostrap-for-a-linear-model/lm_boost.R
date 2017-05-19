# Creator: Raúl Rodríguez Cruces
# Date: August 2016
# Scrip name: lm_boost
# This script applies a boost trap to a set of points and gets the simulated statistics of a linear regression.
# Plots the points with the linear regression and its 95% confidence interval as shade. 
# Adds the two distribution densities estimated statistics with their CI as lines
#
# -------------------------------------------------------------------------
#
#    Boostrap for Linear Regression with confidence interval
#
# -------------------------------------------------------------------------

require(boot)

# Generates the variables X and Y
set.seed(3764)
x <- rnorm(20,mean = 5,sd=3)
y <- rnorm(20,mean = 100,sd=3)
Data <- data.frame(x,y)

par(col.axis="gray25",col.lab="gray25",col.main="gray25") # Graphical parameters color settings
layout(matrix(c(1,1,2,1,1,3), 2, 3, byrow = TRUE))        # Combines plots

# Scatter plot of X & Y variables
plot(Data$x,Data$y,pch=20,cex=2,col="darkslateblue",main="Linear model & Boostrap",cex.main=2.5,cex.lab=1.5,axes=F,ylab="Y",xlab="X",las=1)
axis(1, lty=1, col="gray25", las=1,lwd=3,cex.axis=1.5)
axis(2, lty=1, col="gray25", las=1,lwd=3,cex.axis=1.5)

# Confidence Intervals at 95%
newx <- seq(min(x), max(x), length.out=20)
preds <- predict(lm(y~x), newdata = data.frame(x=newx),interval = 'confidence')
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = '#483D8B64', border = NA)

# Regression line
abline(lm(y~x,data = Data),col="#483D8BFF",lwd=6)

#### Boostrapping with Boot ####
# Function that applies the Lineal regression and obtains only the betas and r2
stats_estimate <- function(data, statistics) {
  d <- data[statistics, ]
  linModel <- lm(y~x,data = d)
  betas <- as.numeric(linModel$coefficients[2])
  r2 <- summary(linModel)$r.square
  estimates <- c(betas,r2)
  return(estimates)   }

# This generates a boot objet with the parameters estimated from our function
boost <- boot(data=Data,statistic = stats_estimate, R=1000)

# in our case t1* = betas & t2* =r2
print(boost)

# Histogram of the betas estimation
betas <- boost$t[,1] # Estimated beta values
CI <- boot.ci(boost, index = 1, conf = 0.95, type = 'bca')[[4]][c(4,5)]     # Confidence Interval of the beta obtained with boot.ci
hist(betas,border=NA,probability = TRUE,cex.main=2,main=expression(paste("Estimated distribution of ", beta)),xlab="betas",
     ,ylim=c(0,max(density(betas)[[2]])*1.1),xlim=mean(betas)+c(-3*sd(betas),3*sd(betas)),lwd=2,cex.lab=1.5)
polygon(density(betas),col="#483D8B64",border = "#483D8BFF",lwd=3)          # Shaded density
abline(v=c(mean(betas),CI),lwd=c(1.5,2,2),lty=c(2,1,1),col="#483D8BFF")     # Lines of the mean, lower CI and upper CI

# Histogram of the r2 estimation
r2 <- boost$t[,2] # Estimated r2 values
CI <- boot.ci(boost, index = 2, conf = 0.95, type = 'bca')[[4]][c(4,5)]     # Confidence Interval of the beta obtained with boot.ci
hist(r2,border=NA,probability = TRUE,cex.main=2,main=expression(paste("Estimated distribution of ", r^2)),xlab=expression(r^2),
     ,ylim=c(0,max(density(r2)[[2]])*1.1),xlim=mean(r2)+c(-3*sd(r2),3*sd(r2)),lwd=2,cex.lab=1.5)
polygon(density(r2),col="#483D8B64",border = "#483D8BFF",lwd=3)             # Shaded density
abline(v=c(mean(r2),CI),lwd=c(1.5,2,2),lty=c(2,1,1),col="#483D8BFF")        # Lines of the mean, lower CI and upper CI 

