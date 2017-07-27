################################################################################
#                             Bootstrap SVM Models                             #
################################################################################
# Beating the benchmark in Kaggle AFSIS Challenge.
# This script is inspired from the python code published at
# http://www.kaggle.com/c/afsis-soil-properties/forums/t/10351/beating-the-benchmark
set.seed(2014)
labels <- c("Ca","P","pH","Sand","SOC")
cost <- c(1e0,1e0,1e0,1e0,1e0)*1e4
names(cost) <- labels
n_boot <- 200 # number of bootsrap models to create


#################
# Features Sets #
#################
X_te = test.infrared[,1531:3578] 
X_tr = train.infrared[,1531:3578]
Y_tr = train.Y


######################
# Parallel Computing #
######################
availableCores = detectCores()
cl <- makeCluster(availableCores, type="PSOCK", outfile="")   
registerDoSNOW(cl)


##############
# Fit Models #
##############
# Set progress bar
pb <- txtProgressBar(max=n_boot, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)
# Begin execution
svm_bootstrap_models <- foreach(i=1:n_boot, .options.snow=opts, .errorhandling=c('stop'), .packages=c('foreach')) %dopar% {
        
        #########
        # Setup #
        #########
        # Allocate list for the models
        svm_models = list()
        # Bootstrap the data
        set.seed(i)
        s = sample(nrow(X), replace=TRUE)
        X_b = X_tr[s,]
        Y_b = Y_tr[s,]
        
        ######################################
        # Fit Models to the Bootstrap Sample #
        ######################################
        foreach(current_label=labels, .errorhandling=c('stop')) %do% {
                # Fit model to the data
                svm_model <- e1071::svm(
                        # Data
                        x=X_b,
                        y=Y_b[,current_label],
                        # Procedures
                        scale=FALSE, shrinking=TRUE,
                        cost=cost[[current_label]],
                        # Memory 
                        cachesize=512,
                        fitted=FALSE)
                # Append the model to the list
                if(current_label == "Ca")
                        svm_models[["Ca"]] = svm_model
                else if(current_label == "P")
                        svm_models[["P"]] = svm_model 
                else if(current_label == "pH")
                        svm_models[["pH"]] = svm_model 
                else if(current_label == "Sand")
                        svm_models[["Sand"]] = svm_model 
                else if(current_label == "SOC")
                        svm_models[["SOC"]] = svm_model
                else
                        stop("No recognized label")
        }
        
        return(svm_models)
}

close(pb)
stopCluster(cl)


########################
# Predict the Test set #
########################
# Allocate prediction matrices
n_bs = length(svm_bootstrap_models)
n_te = nrow(X_te)
pred = list("Ca"=matrix(0,nrow=n_te,ncol=n_bs), 
            "P"=matrix(0,nrow=n_te,ncol=n_bs),
            "pH"=matrix(0,nrow=n_te,ncol=n_bs),
            "Sand"=matrix(0,nrow=n_te,ncol=n_bs),
            "SOC"=matrix(0,nrow=n_te,ncol=n_bs))

# Predict test set for each label
l = length(svm_bootstrap_models)
for(i in 1:l){
        
        percent = 100*i/l
        if(i==1) cat('\nPredicting the test set...')
        if(percent %% 10 == 0) cat(paste0('\t',percent,'%'))
        for(current_label in labels){
                
                svm_model = svm_bootstrap_models[[i]][[current_label]]
                pred[[current_label]][,i] = predict(svm_model, X_te)
                
        }
        
}


####################
# Save Predictions #
####################
for(current_label in labels){
        
        folder_path = file.path(getwd(),'data','bootstrap_predictions')
        dir.create(folder_path, showWarnings=FALSE)
        file_path = file.path(folder_path,paste0(current_label,".csv"))
        write.csv(pred[[current_label]], file_path, row.names=FALSE)
        
}




