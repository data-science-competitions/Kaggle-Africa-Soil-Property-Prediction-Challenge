#   ____    _    
#  / ___|  / \     Genetic Algorithm
# | |  _  / _ \           for
# | |_| |/ ___ \    Subset Selection
#  \____/_/   \_\ 
#
# <https://www.jstatsoft.org/article/view/v053i04/v53i04.pdf>
set.seed(1850)
label <- c("Ca","P","pH","SOC","Sand")[1]


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
        RMSE = sqrt(mean((y_hat-y_te)^2))
        return(-RMSE)
        
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
GA <- ga(type="binary", fitness=mfitness,
         # Genetic nuances 
         #selection=gaperm_nlrSelection,
         #mutation=gareal_raMutation,
         #elitism=8,
         popSize=8*5,
         maxiter=100, run=100,
         names=colnames(train.infrared),
         nBits=ncol(train.infrared),
         parallel=TRUE,
         min=10, max=100,
         monitor=plot,
         seed=2047)
forget(mfitness) # clear cache
stopCluster(cl) # shut down the cluster


###########
# Summary #
###########
summary(GA)


#######################
# Export the solution #
#######################
solution = data.frame(label=label,t(drop(GA@solution)[1,]))
destfile = file.path(getwd(),"data","feature_selection_GA.csv")

# Check if file exists then add the new solution, otherwise create a new file
if(file.exists(destfile)){
        
        temp = read.csv(destfile)
        temp = rbind(temp,solution)
        write.csv(temp, destfile, row.names=FALSE)
        
} else {
        
        write.csv(solution, destfile, row.names=FALSE)
        
}





