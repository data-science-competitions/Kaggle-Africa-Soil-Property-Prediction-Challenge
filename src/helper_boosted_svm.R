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
        svm_bootstrap_models <- foreach(i=1:n_bs, .options.snow=opts, .errorhandling='stop', .packages=c('foreach'), .inorder=FALSE) %dopar% {
                
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
        close(pb)
}# end boosted_svm_fit


# ---------------------------------------------------------------------------- #
#' @function boosted_svm_predict
#' @details Predict given set X
boosted_svm_predict <- function(svm_bootstrap_models, X){
        
        labels <- c("Ca","P","pH","Sand","SOC")
        
        # Allocate prediction matrices
        n_bs = length(svm_bootstrap_models)
        n_sa = nrow(X)
        M = matrix(0,nrow=n_sa,ncol=n_bs)
        row.names(M) = row.names(X)
                
        pred = list("Ca"=M, 
                    "P"=M,
                    "pH"=M,
                    "Sand"=M,
                    "SOC"=M)
        
        # Set progress bar
        pb = txtProgressBar(max=n_bs, style=3)
        
        # Predict test set for each label
        for(i in 1:n_bs){
                
                for(current_label in labels){
                        
                        svm_model = svm_bootstrap_models[[i]][[current_label]]
                        pred[[current_label]][,i] = predict(svm_model, X)
                                           
                }# end for labels
                
                setTxtProgressBar(pb,i) 
                
        }# end for elements
        
        
        close(pb)
        return(pred)
}# end boosted_svm_predict

