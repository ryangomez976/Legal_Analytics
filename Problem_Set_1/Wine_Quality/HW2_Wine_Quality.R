# Required packages
library(caret)
library(pROC)
library(nnet)
library(randomForest)
library(kernlab)
library(corrplot)
# Dataset: project from p.22 http://www3.nd.edu/~mclark19/learn/ML.pdf
wine <- read.csv("http://www.nd.edu/~mclark19/learn/data/goodwine.csv")
# Correlation Plot for collinearity issues
corrplot(cor(wine[, -c(13, 15)]), method = "number", tl.cex = 0.5)
# Psuedo RNG
set.seed(1234)
# Training partition w/ 80% of the dataset
trainIndices <- createDataPartition(wine$good, p = 0.8, list = F)
# Remove listed columns due to collinearity issues
wanted <- !colnames(wine) %in% c("free.sulfur.dioxide", "density", "quality",
	"color", "white")
# Training set
wine_train <- wine[trainIndices, wanted]
# Validation set
wine_test <- wine[-trainIndices, wanted]
wine_trainplot <- predict(preProcess(wine_train[,-10], method="range"), wine_train[,-10])
featurePlot(wine_trainplot, wine_train$good, "box")
# K-nearest Neighbor
set.seed(1234)
# 10-fold cross validation
cv_opts <- trainControl(method="cv", number=10)
knn_opts <- data.frame(.k=c(seq(3, 11, 2), 25, 51, 101))
results_knn <- train(good~., data=wine_train, method="knn", 
	preProcess="range", trControl=cv_opts,tuneGrid = knn_opts)
results_knn
# Confusion Matrix
preds_knn <- predict(results_knn, wine_test[,-10])
confusionMatrix(preds_knn, wine_test[,10], positive='Good')
# Dot Plot
dotPlot(varImp(results_knn))