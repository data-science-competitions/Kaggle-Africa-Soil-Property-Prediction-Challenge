################################################################################
#                 SVM with Genetic Algorithm Feature Selection                 #
################################################################################
# Beating the benchmark in Kaggle AFSIS Challenge.
# This script is inspired from the python code published at
# http://www.kaggle.com/c/afsis-soil-properties/forums/t/10351/beating-the-benchmark
set.seed(2014)
cost <- c(1e0,1e0,1e0,1e0,1e0)*1e4
labels <- c("Ca","P","pH","Sand","SOC")
names(cost) <- labels


#################
# Features Sets #
#################
# 1. Set the default features suggestions
suggestions = matrix(0, 
                     nrow=length(labels), 
                     ncol=ncol(train.infrared), 
                     dimnames=list(labels,colnames(train.infrared)))
suggestions[,1531:3578] = 1
suggestions = as.data.frame(suggestions)

# 2. Load the features suggested by the genetic algorithm
for(label in labels){
        
        selected_columns = getSelectedFeatures(label)
        if(is.null(selected_columns)) next
        suggestions[label,] = selected_columns
                
}




##########################
# Fit models to the data #
##########################
cat("Fitting models to the data...\n")

for (current_label in labels){
        
        inc = which(unlist(suggestions[current_label,])==1)
        X_te = test.infrared[,inc] 
        X_tr = train.infrared[,inc]
        y = train.Y[,current_label]
        
        mdl.svm <- e1071::svm(X_tr,
                              y,
                              scale=FALSE, cost=cost[[current_label]],
                              cachesize=512)
        
        sampleSubmission[,current_label] <- predict(mdl.svm, X_te)
        cat(current_label,"was predicted successfully\n")
}

write.csv(sampleSubmission, 
          file.path(getwd(),"submissions",paste0(Sys.Date(),"-","svm","-","genetic-selection",".csv")),
          row.names=F)


###########
# Results #
###########
# ID    | Model                 | Variables     | Rank  | Private Score | Public Score
# 1     | All Zeros Benchmark   |               | 1150  | 0.91393       |
# 2     | BART Benchmark        |               | 796   | 0.56551       |
# 3     | SVM w. cost=1e0       | 2500          | 1050  | 0.69305       | 0.72128
# 4     | SVM w. cost=1e3       | 2500          | 577   | 0.52256       | 0.45488
# 5     | SVM w. cost=1e4       | 2500          | 61    | 0.49556       | 0.41910 
# 6     | SVM w. cost=1e4       | GA            | 141   | 0.50244       | 0.42847
# 7     | SVM w. cost=1e4       | GA            | 490   | 0.51318       | 0.43284
# 8     | SVM w. custom costs   | 2500          | 71    | 0.49625       | 0.42853