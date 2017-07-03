cat("\014"); rm(list = ls())
source("lib/load_libraries.R")
source("lib/load_data.R")
## -------------------------------------------------------------------------- ##
## Linear Regularized Solution Paths
## -------------------------------------------------------------------------- ##
set.seed(20150513)
## Split the dataset
n <- nrow(train.infrared)
p <- ncol(train.infrared)
index.train <- sample(1:n,round(n*0.7))
X.tr <- as.matrix(train.infrared[index.train,],  ncol=p)
X.te <- as.matrix(train.infrared[-index.train,], ncol=p)
y.tr <- as.matrix(train.Y[index.train, "Ca"])
y.te <- as.matrix(train.Y[-index.train,"Ca"])
## Step 1: Fits Least Angle Regression
library(lars)
mdl.Ca.lars <- lars(x=X.tr,y=y.tr,type="lasso",use.Gram=FALSE)
hist(apply(mdl.Ca.lars$beta,1,function(x) sum(x!=0)))
## Step 2: Build linear model on the first k variables and evaluate the model
X.tr <- as.data.frame(X.tr)
X.te <- as.data.frame(X.te)
y.tr <- as.data.frame(y.tr)
y.te <- as.data.frame(y.te)
rmse_lars <- c(NA)
for (k in 2:nrow(mdl.Ca.lars$beta)){
        ## Choose k variables
        var.set <- (1:p)[mdl.Ca.lars$beta[k,]!=0]
        ## Fit linear model to the training set
        mdl.Ca.reg <- lm(y ~ ., data = data.frame(X.tr[,var.set], y=y.tr))
        ## Predict on unseen data
        y.hat <- predict(mdl.Ca.reg, data = X.te[,var.set])
        ## Calculate the estimation error
        rmse_lars <- rbind(rmse_lars,
                           sqrt(sum((y.hat-y.te)^2)/nrow(X.te)))
}
## Plot the error as function of penalty path 
plot(1:nrow(rmse_lars),rmse_lars,type='l')
