# Homework Problem Set 2, No. 2
# Assignement from: https://www.youtube.com/watch?v=kdUVUINpFmI
# Packages
library(ggplot2)
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
species.plot <- ggplot(iris, aes(Petal.Width, Sepal.Width)) + geom_point(size = 3, aes(colour = Species)) + labs(x = "Petal Width", y = "Sepal Width")
# Building the classification tree
tree1 <- tree(Species ~ Sepal.Width + Petal.Width, data=iris)
summary(tree1)
plot(tree1)
text(tree1)
# Plot w/ classification divisions
species.plot.treeoverlay <- species.plot + geom_line(aes(x=c(1.35,1.75),y=c(2.65,2.65))) + geom_vline(aes(xintercept=0.8)) + geom_vline(aes(xintercept=1.75)) + geom_vline(aes(xintercept=1.35)) + annotate("text", x = .4, y = 4, label = "Setosa", family="Equity Caps A", size = 4) + annotate("text", x = 1.1, y = 4, label = "Versicolor", family="Equity Caps A", size = 4) + annotate("text", x = 1.55, y = 4, label = "Versicolor", family="Equity Caps A", size = 4) + annotate("text", x = 1.55, y = 2.3, label = "Virginica", family="Equity Caps A", size = 4) + annotate("text", x = 2.2, y = 4, label = "Virginica", family="Equity Caps A", size = 4)
species.plot.treeoverlay + theme(text = element_text(family = "Concourse C4", size = 18))
# Predicting new values
set.seed(32313)
newdata <- data.frame(Petal.Width = runif(20, 0, 2.5), Sepal.Width = runif(20, 2, 4.5))
pred1 <- predict(tree1, newdata)
pred1
# New data plot w/ divisions
pred1 <- predict(tree1, newdata, type="class")
pred1.plot <- ggplot(newdata, aes(Petal.Width, Sepal.Width)) + geom_point(size = 3, aes(colour = pred1)) + labs(x = "Petal Width", y = "Sepal Width")
pred1.plot + geom_line(aes(x=c(1.35,1.75),y=c(2.65,2.65))) + geom_vline(aes(xintercept=0.8)) + geom_vline(aes(xintercept=1.75)) + geom_vline(aes(xintercept=1.35)) + annotate("text", x = .4, y = 4.2, label = "Setosa") + annotate("text", x = 1.1, y = 4.2, label = "Versicolor") + annotate("text", x = 1.55, y = 4.2, label = "Versicolor") + annotate("text", x = 1.55, y = 2.3, label = "Virginica") + annotate("text", x = 2.2, y = 4, label = "Virginica")
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