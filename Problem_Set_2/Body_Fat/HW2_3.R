# Homework Set 2, No. 3
# Project from pg. 32-36: http://cran.r-project.org/doc/contrib/Zhao_R_and_data_mining.pdf
# Necessary Packages
library(mboost)
library(TH.data)
library(rpart)
# Data set
data("bodyfat", package="TH.data")
dim(bodyfat)
attributes(bodyfat)
bodyfat[1:5,]
# Split into training and test sets
set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace=TRUE, prob=c(0.7, 0.3))
bodyfat.train <- bodyfat[ind==1,]
bodyfat.test <- bodyfat[ind==2,]
# train a decision tree
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat_rpart <- rpart(myFormula, data = bodyfat.train, control = rpart.control(minsplit = 10))
attributes(bodyfat_rpart)
print(bodyfat_rpart$cptable)
print(bodyfat_rpart)
# Regression tree
plot(bodyfat_rpart)
text(bodyfat_rpart, use.n=T)
# Pruning
opt <- which.min(bodyfat_rpart$cptable[,"xerror"])
cp <- bodyfat_rpart$cptable[opt, "CP"]
bodyfat_prune <- prune(bodyfat_rpart, cp = cp)
print(bodyfat_prune)
# Regression tree
plot(bodyfat_prune)
text(bodyfat_prune, use.n=T)
# Predictions
DEXfat_pred <- predict(bodyfat_prune, newdata=bodyfat.test)
xlim <- range(bodyfat$DEXfat)
plot(DEXfat_pred ~ DEXfat, data=bodyfat.test, xlab="Observed", ylab="Predicted", ylim=xlim, xlim=xlim)
abline(a=0, b=1)