#   ____    _    
#  / ___|  / \     Genetic Algorithm
# | |  _  / _ \           for
# | |_| |/ ___ \    Subset Selection
#  \____/_/   \_\ 
#
# <https://www.jstatsoft.org/article/view/v053i04/v53i04.pdf>
labels <- c("Ca","P","pH","SOC","Sand")
label <- labels[5]
cost <- c(1e0,1e0,1e0,1e0,1e0)
names(cost) <- labels


#########
# Setup #
#########
par(pty="s")
destfile = file.path(getwd(),"data","feature_selection_GA.csv")
# Copy the data
X = train.infrared
y = train.Y[,label]
# Setup the computational nuances of the model training phase
fitControl <- trainControl(
        ## k-fold CV        
        method="cv",
        number=5,
        seeds=(803):(803+5+1),
        #repeats=1, # relevant only when method = "repeatedcv"        
        allowParallel=TRUE,
        returnData=FALSE) # saves memory
# Parallel Computing 
availableCores = detectCores()
cl <- makeCluster(availableCores, type="PSOCK", outfile="")   
registerDoParallel(cl)


####################
# Fitness Function #
####################
fitness <- function(string){
        
        # potential solution
        inc = which(string==1)
        # resample the data
        n = nrow(train.infrared)
        x = X[,inc]
        y = y
        # build the model
        mdl <- train(x=x, y=y,
                     method=libSVM(), 
                     trControl=fitControl,
                     #preProc=c("center", "scale")
                     tuneGrid=expand.grid(cost=cost[[label]]))
        # evaluate the model
        RMSE = mean(mdl$resample$RMSE)
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
# Get the previous creatures
if(file.exists(destfile)){
        
        temp = read.csv(destfile)
        previous_creatures = temp[temp$label %in% label,-1]
        
        if(nrow(previous_creatures)>0)
                suggestions = rbind(suggestions,data.matrix(previous_creatures))
} 


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
gaControl("binary") # show the current values of GA
GA <- ga(type="binary", fitness=mfitness,
         # Genetic nuances 
         #population="gabin_Population",
         #selection="gabin_lrSelection",
         #crossover="gabin_spCrossover",
         #mutation="gabin_raMutation",
         popSize=availableCores*2,
         maxiter=100, run=100,
         names=colnames(train.infrared),
         nBits=ncol(train.infrared),
         suggestions=suggestions,
         parallel=TRUE,
         min=10, max=100, # not relevant in the case of type="binary"
         monitor=plot,
         seed=2047)
forget(mfitness) # clear cache
stopCluster(cl) # shut down the cluster


###########
# Summary #
###########
summary(GA)


###############
# Save Visual #
###############
destimage = file.path(getwd(), "reports", paste0("genetic algorithm for ",label," ",Sys.Date(),".png"))
dev.copy(png, destimage)
dev.off()


#######################
# Export the solution #
#######################
solution = drop(GA@solution)
if (!is.null(dim(solution))) solution = solution[,1]
solution = data.frame(label=label,t(solution))

# Check if file exists then add the new solution, otherwise create a new file
if(file.exists(destfile)){
        
        temp = read.csv(destfile)
        temp = rbind(temp,solution)
        write.csv(temp, destfile, row.names=FALSE)
        
} else {
        
        write.csv(solution, destfile, row.names=FALSE)
        
}





