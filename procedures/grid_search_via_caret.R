################################################################################
#                            Grid Search via Caret                             #
################################################################################
EXECUTION_ID <- generateID()
labels <- c("Ca","P","pH","Sand","SOC")
label <- labels[2]


######################
# Parallel Computing #
######################
availableCores = detectCores()
cl <- makeCluster(availableCores, type="PSOCK", outfile="")   
registerDoParallel(cl)


#########
# Setup #
#########
# Copy the data
X = train.infrared
y = train.Y[,label]
# Select the features
selected_columns = getSelectedFeatures(label)
if(!is.null(selected_columns)) X = X[,which(selected_columns==1)]


#################
# Train Control #
#################
fitControl = trainControl(
        ## Repeated k-fold Cross Validation
        method="repeatedcv",
        number=8,
        repeats=5,
        allowParallel=TRUE,
        returnData=FALSE) # saves memory


###############
# Tuning Grid #
###############
costs = round(exp(seq(from=1,to=log(1e4),length.out=8)))
svmGrid = expand.grid(cost=costs,
                      stringsAsFactors=FALSE)


################
# Start Search #
################
set.seed(1024)
startTime = Sys.time()
svmFit = train(x=X, y=y,
               method=libSVM(), 
               trControl=fitControl,
               tuneGrid=svmGrid,
               #preProc=c("center", "scale")
               verbose=TRUE)
finishTime = Sys.time()
stopCluster(cl) # shut down the cluster


###################
# Analyze Results #
###################
#timeDiff = round(as.numeric(finishTime-startTime, units="mins"),0)
round(as.numeric(finishTime-startTime, units="mins"),0)
svmFit