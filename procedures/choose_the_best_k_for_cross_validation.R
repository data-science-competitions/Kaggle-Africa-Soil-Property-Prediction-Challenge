################################################################################
#              Choose the Best K for Performing Cross Validation               #
################################################################################
#' Question: What value should we choose for K?
#' Answer: It depends on how the performance of the learning method varies with 
#' the size of the training set.
set.seed(2017)
labels <- c("Ca","P","pH","Sand","SOC")
choosen_label = labels[1]
costs <- c(1e0,1e0,1e0,1e0,1e0)*1e4
names(costs) <- labels


################
# Get the Data #
################
shuffledIndex = sample(nrow(train.infrared))
X_tr = train.infrared[shuffledIndex,1531:3578]
Y_tr = train.Y[shuffledIndex,choosen_label]

N = nrow(X_tr)
N_max = nrow(X_tr)-1
N_min = round(nrow(X_tr)/2)


#####################
# Start CPU cluster #
#####################
availableCores = detectCores()
cl <- makeCluster(availableCores, type="PSOCK", outfile="")   
registerDoSNOW(cl)


####################
# Set Progress Bar #
####################
pb = txtProgressBar(min=N_min,max=N_max, style=3)
progress = function(n) setTxtProgressBar(pb, n)
opts = list(progress=progress)

#####################################################
# Fit Models to Different Sizes of the Training set #
#####################################################
err = foreach(n=N_min:N_max, .combine='c', .options.snow=opts, .errorhandling='stop', .inorder=TRUE) %dopar% {
        
        # Split the Data
        X_Train <- X_tr[1:n,]
        y_Train <- Y_tr[1:n]
        X_Test  <- X_tr[(n+1):N,]
        y_Test  <- Y_tr[(n+1):N]
        
        # Fit model to the train set
        svm_model <- e1071::svm(
                x=X_Train,
                y=y_Train,
                # Procedures
                scale=FALSE, shrinking=TRUE,
                cost=costs[[choosen_label]],
                # Memory 
                cachesize=512,
                fitted=FALSE)
        # Predict the test set
        y_hat = predict(svm_model, X_Test)
        # Calculate the error
        RMSE = caret::RMSE(y_hat, y_Test)
        
        return(RMSE)
}
stopCluster(cl)
close(pb)


#################
# Visualisation #
#################
# learning curve for a classifier on a given task:
par(pty="s")
x = N_min:N_max
y = err
plot(x, y, type="l",
     xlab="Size of Training Set", ylab="RMSE of Test Set")
# Compute fold sizes
folds = c(2^2,2^3,2^4,2^5,2^6)
folds_sizes = round(N / folds)
abline(v=N-folds_sizes, lty=2)
text(x=N-folds_sizes, y=mean(err), paste0('K=',folds), srt=90, pos=2)
# Add Test size axis
par(new=T)
plot(N-x, y, type="n", pch=16, axes=F, xlab=NA, ylab=NA, cex=1.2, 
     xlim=rev(range(N-x)))
axis(side=3)
mtext(side=3, line=3, 'Size of Test Set')
