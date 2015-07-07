## Load the required packages for the project
packages.list = c("knitr","pander","rmarkdown",      # Documentation packages
                  "ggplot2","gridExtra",             # Visualisation 
                  "devtools","testthat",             # Development tools for R
                  "caret",                           # Classification Algorithms
                  "kernlab","randomForest",          # SVM, RF
                  "plyr","reshape2",                 # Data Munging Tools
                  "pROC","ROCR","Metrics",           # Performance tools 
                  "doParallel","foreach",            # Parallel Tools
                  "xlsx",                            # Exporting Tools
                  "devtools","plyr","lubridate")
for (i in 1:length(packages.list)){
        if(!require(packages.list[i], character.only=TRUE)) {
                install.packages(packages.list[i])}
        require(packages.list[i], character.only = TRUE)  
}

## install the R-packages of xgboost, and fixed gbm for caret
# if(!require("xgboost")) devtools::install_github('dmlc/xgboost',subdir='R-package')
## http://www.kaggle.com/c/otto-group-product-classification-challenge/forums/t/13273/problems-with-r-caret-gbm
if(!require("gbm")) devtools::install_github("harrysouthworth/gbm")




rm(packages.list, i)