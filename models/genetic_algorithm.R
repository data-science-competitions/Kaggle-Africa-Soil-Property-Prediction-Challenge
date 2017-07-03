#   ____    _    
#  / ___|  / \     Genetic Algorithm
# | |  _  / _ \           for
# | |_| |/ ___ \    Subset Selection
#  \____/_/   \_\ 
#
# <https://www.jstatsoft.org/article/view/v053i04/v53i04.pdf>
set.seed(1850)
label <- c("Ca","P","pH","SOC","Sand")[5]


############################
# Setup Parallel Computing #
############################
cl <- makeCluster(detectCores(), type="PSOCK", outfile="")   
registerDoParallel(cl)


####################
# Fitness Function #
####################
fitness <- function(string){
        
        # potential solution
        inc = which(string==1)
        # resample the data
        n = nrow(train.infrared)
        s = sample(n, replace=TRUE)
        x = train.infrared[s,inc]
        y = train.Y[s,label]
        # split the data
        n_tr = round(n*0.7)
        x_tr = x[1:n_tr,]
        y_tr = y[1:n_tr]
        x_te = x[(n_tr+1):n,]
        y_te = y[(n_tr+1):n]
        # build the model
        mdl <- lm(y ~ ., data = data.frame(x_tr, y=y_tr))
        # evaluate the model
        y_hat = predict(mdl, newdata=x_te)
        MSE = sqrt(sum((y_hat-y_te)^2))
        return(1/MSE)
        
}# fitness


###############
# Memoization #
###############
#' In certain circumstances, particularly with binary GAs, memoization can be 
#' used to speed up calculations by using cached results. This is easily 
#' obtained using the memoise package.
mfitness <- memoise(fitness)
               

###################
# Start Evolution #
###################                         
GA <- ga(type="binary", fitness=fitness,
         # Genetic nuances 
         #selection=gaperm_nlrSelection,
         #mutation=gareal_raMutation,
         popSize=8*4,
         maxiter=100, run=100,
         names=colnames(train.infrared),
         nBits=ncol(train.infrared),
         parallel=TRUE,
         min=10, max=100,
         monitor=plot,
         seed=2047)
summary(GA)


forget(mfitness) # clear cache
stopCluster(cl) # shut down the cluster
