#In this problem, we want to fit a linear model onto data of people inhaling and exhaling

#getting appropriate libraries
library(MASS)
library(leaps)
library(readxl)
setwd("~/Desktop/Stat 353 Data")

#importing data into R and plotting
vent <- read_excel("~/Desktop/Stat 353 Data/ventilation.xlsx")
venty <- vent$Y
ventx <- vent$X
plot(ventx,venty,type="p",xlab="Oxygen Uptake",ylab="Expired Ventilation",main="Expired ventilation against oxygen uptake")

#scatter plot appears to be nonlinear
#however, we will fit a simple linear model anyway
ventlm <- lm(venty ~ ventx)
par(mfrow=c(2,2))
plot(ventlm)

#just like hypothesized, the data is not linearly correlated
#the scatter plot, along with the plot of residuals versus fitted values, reveal a strong correlation between the variance and the values of mu hat
#so try a logarithmic transformation
ventylog <- log(venty)
ventloglm <- lm(ventylog ~ ventx)
par(mfrow=c(2,2))
plot(ventloglm)


#observe the outlying point with a large studentized residual and a large leverage and thus a large cook's distance (greater than 0.5)
#remove this point and fit the model again
ventylog2 <- ventylog[-1]
ventx2 <- ventx[-1]
ventloglm2 <- lm(ventylog2 ~ ventx2)
par(mfrow=c(2,2))
plot(ventloglm2)
summary(ventloglm2)$coefficients

#this model fits the data well with r-squared = 0.9899
#so the final transformed model is ln(y) = 2.407 + 5.762*10^(-4)*x
#plot model over data with outlier removed
plot(ventx2,ventylog2,xlab="Oxygen Uptake",ylab="Log of Expired Ventilation",main="Transformed Model with Single Outlier Removed")
abline(ventloglm2)



