# Project from §2.1 http://www.stat.cmu.edu/~cshalizi/350/lectures/22/lecture-22.pdf
# On estimating CA real estate values
# Required package
library("tree")
# Data set on housing prices in CA
calif <- read.table("http://www.stat.cmu.edu/~cshalizi/350/hw/06/cadata.dat", header=TRUE)
# Tree function with default argument values
# Dependent variable is $MedianHouseValue, independent variables
# $Longitude and $Latitude
treefit <- tree(log(MedianHouseValue) ~ Longitude+Latitude,data=calif)
# Plotting treefit will show the tree
plot(treefit)
# Overwrite the decisional criteria on the tree
text(treefit,cex=0.75)
# Plotting treefit with 10 cuts; each cut representing housing price
price.deciles <- quantile(calif$MedianHouseValue,0:10/10,data=calif)
cut.prices <- cut(calif$MedianHouseValue,price.deciles,include.lowest=TRUE)
plot(calif$Longitude,calif$Latitude,col=grey(10:2/11)[cut.prices],pch=20,
	xlab="Longitude",ylab="Latitude")
partition.tree(treefit,ordvars=c("Longitude","Latitude"),add=TRUE)
# Summary
# RMS 0.41
summary(treefit)

# Second Iteration with 68 termial nodes
# Reducing mindev from default of 0.01 to 0.001
treefit2 <- tree(log(MedianHouseValue) ~ Longitude+Latitude,data=calif,
	control=tree.control(nobs=20640, mincut = 5, minsize = 10, mindev = 0.001))
plot(treefit2)
text(treefit2,cex=.3)
plot(calif$Longitude,calif$Latitude,col=grey(10:2/11)[cut.prices],pch=20,
	xlab="Longitude",ylab="Latitude")
partition.tree(treefit2,ordvars=c("Longitude","Latitude"),add=TRUE,cex=0.3)
# RMS 0.32
summary(treefit2)

# Third Iteration with only 15 terminal nodes, but similar RMS error
# Includes all independent variables
treefit3 <- tree(log(MedianHouseValue) ~., data=calif)
plot(treefit3)
text(treefit3,cex=0.5,digits=3)
cut.predictions <- cut(predict(treefit3),log(price.deciles),include.lowest=TRUE)
plot(calif$Longitude,calif$Latitude,col=grey(10:2/11)[cut.predictions],pch=20,
	xlab="Longitude",ylab="Latitude")
# RMS 0.36
summary(treefit3)