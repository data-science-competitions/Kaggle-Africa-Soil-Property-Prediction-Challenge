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
        x = train.infrared[,inc]
        y = train.Y[,label]
        # split the data
        n_tr = round(n*0.7)
        x_tr = x[1:n_tr,]
        y_tr = y[1:n_tr]
        x_te = x[(n_tr+1):n,]
        y_te = y[(n_tr+1):n]
        # build the model
        mdl <- svm(x=x_tr,
                   y=y_tr,
                   kernel="radial",
                   scale=FALSE, cost=1,
                   cachesize=1024*10)
        # evaluate the model
        y_hat <- predict(mdl, newdata=x_te)
        
        RMSE = sqrt(mean((y_hat-y_te)^2))
        return(-RMSE)
        
}# fitness


###############
# Suggestions #
###############
# Sample the relevant spectrum in windows of W (prime numbers)
prime_numbers = c(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97)
suggestions = matrix(0, nrow=length(prime_numbers), ncol=ncol(train.infrared))
for(i in 1:length(prime_numbers))
        suggestions[i,seq(from=1,to=3578, by=prime_numbers[i])] = 1 


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
         #elitism=dim(suggestions)[2],
         popSize=8*5,
         maxiter=100, run=100,
         names=colnames(train.infrared),
         nBits=ncol(train.infrared),
         suggestions=suggestions,
         parallel=TRUE,
         min=10, max=100, # not relevant in case of type="binary"
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
solution = drop(GA@solution)
if (!is.null(dim(solution))) solution = solution[,1]
solution = data.frame(label=label,t(solution))
destfile = file.path(getwd(),"data","feature_selection_GA.csv")

# Check if file exists then add the new solution, otherwise create a new file
if(file.exists(destfile)){
        
        temp = read.csv(destfile)
        temp = rbind(temp,solution)
        write.csv(temp, destfile, row.names=FALSE)
        
} else {
        
        write.csv(solution, destfile, row.names=FALSE)
        
}





