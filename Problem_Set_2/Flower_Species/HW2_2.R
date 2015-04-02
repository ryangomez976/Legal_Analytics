# Homework Problem Set 2, No. 2
# Assignement from: https://www.youtube.com/watch?v=kdUVUINpFmI
# Packages
library(Cairo)
library(tree)
library(MASS)
# Output file
CairoPDF("HW2_2.pdf", 8, 8, bg="white")
# Data Set
data(iris)
names(iris)
table(iris$Species)
# Goal is to classify each observation into its species
plot(iris$Petal.Width, iris$Sepal.Width, pch=19, col=as.numeric(iris$Species))
legend(1, 4.5, legend=unique(iris$Species), col=unique(as.numeric(iris$Species)), pch=19)
# Building the classification tree
tree1 <- tree(Species ~ Sepal.Width + Petal.Width, data=iris)
summary(tree1)
plot(tree1)
text(tree1)
# Plot w/ classification divisions
plot(iris$Petal.Width, iris$Sepal.Width, pch=19, col=as.numeric(iris$Species))
partition.tree(tree1, label="Species", add=TRUE)
legend(1.75, 4.5, legend=unique(iris$Species), col=unique(as.numeric(iris$Species)), pch=19)
# Predicting new values
set.seed(32313)
newdata <- data.frame(Petal.Width = runif(20, 0, 2.5), Sepal.Width = runif(20, 2, 4.5))
pred1 <- predict(tree1, newdata)
pred1
# New data plot w/ divisions
pred1 <- predict(tree1, newdata, type="class")
plot(newdata$Petal.Width, newdata$Sepal.Width, col=as.numeric(pred1), pch=19)
partition.tree(tree1, "Species", add=TRUE)

# Pruning w/ new dataset "Cars93"
# Goal is predicting the type of drive train
treeCars <- tree(DriveTrain ~ MPG.city + MPG.highway + AirBags + EngineSize +Width + Length + Weight + Price + Cylinders + Horsepower + Wheelbase, data=Cars93)
plot(treeCars)
text(treeCars)
# Plotting errors misclassification rates vs. deviance
par(mfrow=c(1,2))
plot(cv.tree(treeCars, FUN=prune.tree, method="misclass"))
plot(cv.tree(treeCars))
# Pruning the treeCars
pruneTree <- prune.tree(treeCars, best = 4)
plot(pruneTree)
text(pruneTree)
# Show resubstitution error
table(Cars93$DriveTrain, predict(pruneTree, type = "class"))
table(Cars93$DriveTrain, predict(treeCars, type = "class"))
dev.off()