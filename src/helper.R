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