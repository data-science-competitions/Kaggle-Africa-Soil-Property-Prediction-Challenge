################################################################################
#                               Helper Functions                               #
################################################################################


#######################
# Caret Custom Models #
#######################
libSVM <- function(){
        # 1. libSVM 
        ## Model Components
        libSVM <- list(type = c("Classification", "Regression"), library = "e1071")
        ## The parameters Element
        prm <- data.frame(parameter=c("cost"),
                          class=rep("numeric", 1),
                          label=c("cost"))
        libSVM$parameters <- prm
        ## The grid Element
        libSVMGrid <- function(x, y, len=NULL) {
                ## If no grid parameters are entered, we use these:
                expand.grid(cost = dim(x)[1])
        }
        libSVM$grid <- libSVMGrid
        ## The fit Element
        libSVMFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
                e1071::svm(x=x, y=y,
                           cost=param$cost,
                           kernel="radial",
                           scale=FALSE,
                           cachesize=512,
                           ...)
        }
        libSVM$fit <- libSVMFit
        ## The predict Element
        libSVMPred <- function(modelFit, newdata, preProc=NULL, submodels=NULL)
                predict(modelFit, newdata)
        libSVM$predict <- libSVMPred
        ## The prob Element
        libSVM$prob <- libSVMPred
        ## The sort Element
        libSVMSort <- function(x)
                x[order(x$cost), ]
        libSVM$sort <- libSVMSort
        
        return(libSVM)
}


#####################################
# Discrete Wavelet Transforms (DWT) #
#####################################
## WaveletTransform
## INPUT:
## X.M: [n,2048] or [n,4096] dataframe containing mid-infrared absorbance measurements
##
## lvl: Number of basis function to construct. 1<=lvl<=10. 
##      e.g. lvl=4 -> 2^4 = 16 basis function will be returned for each signal
##
## thresholding: TRUE/FALSE to use SURE shrinkage on signal
##
## OUTPUT:
## X.W: A [n,2^lvl] dataframe containing each signal coefficients 
##
## Example: training.W <- Africa.WaveletTransform(training.M,9,FALSE)
## 
WaveletTransform <- function(X.M, lvl, thresholding){
        ## Load the required packages for the project                                
        if(!require(wavethresh)) {install.packages(wavethresh)}
        require(wavethresh) 
        
        X.W <- matrix(0,dim(X.M)[1],2^lvl)
        
        if (thresholding==TRUE){
                ## With thresholding
                for (i in 1:dim(X.M)[1]){
                        X.W[i,] <- 
                                accessD(   # Get detail coefficients from wavelet object (wd).
                                        threshold( # SURE Thresholding
                                                wd(as.numeric(X.M[i,])), # discrete wavelet transform
                                                policy="sure", dev=madmad), level=lvl)
                }    
        } else {
                ## Without thresholding
                for (i in 1:dim(X.M)[1]){
                        X.W[i,] <- 
                                accessD(   # Get detail coefficients from wavelet object (wd).
                                        wd(as.numeric(X.M[i,])), # discrete wavelet transform
                                        level=lvl)
                }
        }
        
        return(as.data.frame(X.W))
}