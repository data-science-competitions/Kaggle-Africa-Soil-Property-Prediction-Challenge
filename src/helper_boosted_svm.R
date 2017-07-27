################################################################################
#                         Boosted SVM Helper Functions                         #
################################################################################
# ---------------------------------------------------------------------------- #
#' @function boosted_svm_fit
#' @details Fit bootstrap svm models 
boosted_svm_fit <- function(X, Y, 
                            n_bs=200, # number of bootstrap samples
                            costs=rep(1e4,5))
{
        
        labels = c("Ca","P","pH","Sand","SOC")
        
        # Set progress bar
        pb = txtProgressBar(max=n_bs, style=3)
        progress = function(n) setTxtProgressBar(pb, n)
        opts = list(progress=progress)
        
        # Begin execution
        svm_bootstrap_models <- foreach(i=1:n_bs, .options.snow=opts, .errorhandling='stop', .packages=c('foreach')) %dopar% {
                
                #########
                # Setup #
                #########
                # Allocate list for the models
                svm_models = list()
                # Bootstrap the data
                set.seed(i)
                s = sample(nrow(X), replace=TRUE)
                X_b = X[s,]
                Y_b = Y[s,]
                
                
                ######################################
                # Fit Models to the Bootstrap Sample #
                ######################################
                foreach(current_label=labels, .errorhandling='stop', .inorder=FALSE) %do% {
                        # Fit model to the data
                        svm_model <- e1071::svm(
                                # Data
                                x=X_b,
                                y=Y_b[,current_label],
                                # Procedures
                                scale=FALSE, shrinking=TRUE,
                                cost=costs[[current_label]],
                                # Memory 
                                cachesize=512,
                                fitted=FALSE)
                        # Append the model to the list
                        if(current_label=="Ca")
                                svm_models[["Ca"]] = svm_model
                        else if(current_label=="P")
                                svm_models[["P"]] = svm_model 
                        else if(current_label=="pH")
                                svm_models[["pH"]] = svm_model 
                        else if(current_label=="Sand")
                                svm_models[["Sand"]] = svm_model 
                        else if(current_label=="SOC")
                                svm_models[["SOC"]] = svm_model
                        else
                                stop("No recognized label")
                }# end foreach do
                return(svm_models)
                
        }# end foreach dopar
        return(svm_bootstrap_models)
        
}# end boosted_svm_fit


# ---------------------------------------------------------------------------- #
#' @function boosted_svm_prdicet
#' @details Predict given set X
boosted_svm_prdicet <- function(svm_bootstrap_models, X){
        
        labels <- c("Ca","P","pH","Sand","SOC")
        
        # Allocate prediction matrices
        n_bm = length(svm_bootstrap_models)
        n_bs = length(svm_bootstrap_models)
        n_sa = nrow(X)
        pred = list("Ca"=matrix(0,nrow=n_sa,ncol=n_bs), 
                    "P"=matrix(0,nrow=n_sa,ncol=n_bs),
                    "pH"=matrix(0,nrow=n_sa,ncol=n_bs),
                    "Sand"=matrix(0,nrow=n_sa,ncol=n_bs),
                    "SOC"=matrix(0,nrow=n_sa,ncol=n_bs))
        
        # Predict test set for each label
        l = length(svm_bootstrap_models)
        for(i in 1:l){
                
                percent = 100*i/l
                if(i==1) cat('\nPredicting the test set...')
                if(percent %% 10 == 0) cat(paste0('\t',percent,'%'))
                
                for(current_label in labels){
                        
                        svm_model = svm_bootstrap_models[[i]][[current_label]]
                        pred[[current_label]][,i] = predict(svm_model, X)
                        
                }# end for labels
                
        }# end for elements
        
        close(pb)
        return(pred)
}# end boosted_svm_prdicet

