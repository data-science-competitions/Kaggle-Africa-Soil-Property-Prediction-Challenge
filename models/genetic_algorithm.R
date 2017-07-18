#   ____    _    
#  / ___|  / \     Genetic Algorithm
# | |  _  / _ \           for
# | |_| |/ ___ \    Subset Selection
#  \____/_/   \_\ 
#
# <https://www.jstatsoft.org/article/view/v053i04/v53i04.pdf>
labels <- c("Ca","P","pH","SOC","Sand")
label <- labels[1]
cost <- c(1e0,1e0,1e0,1e0,1e0)*1e0
names(cost) <- labels
DWT <- FALSE # Should the Discrete Wavelet Transforms be applied?


#########
# Setup #
#########
par(pty="s")
# Copy the data
X = if(DWT) train.infrared.dwt else train.infrared
y = train.Y[,label]
# Setup the computational nuances of the model training phase
fitControl <- trainControl(
        ## k-fold CV        
        method="cv",
        number=8,
        seeds=(803):(803+8+1),
        allowParallel=TRUE,
        returnData=FALSE) # saves memory
# Parallel Computing 
availableCores = detectCores()
cl <- makeCluster(availableCores, type="PSOCK", outfile="")   
registerDoParallel(cl)


#######################
# Set Genetic Nuances #
#######################
gaControl("binary"=list(selection=c("gabin_lrSelection","gabin_rwSelection","gareal_sigmaSelection","gabin_tourSelection")[4], 
                        crossover=c("gabin_spCrossover","gabin_uCrossover")[2],
                        mutation=c("gabin_raMutation")[1]))

# Population size
popSize = availableCores

# Set the number of best fitness individuals to survive at each generation
#' Because K-tournament selection guarantees to keep K copies of the best 
#' individual, we disable the elitism mechanism 
if(gaControl("binary")$selection == "gabin_tourSelection"){ 
        elitism=0 
} else {
        elitism=max(1, round(popSize*0.05))
}

# Set file name prefix
file_prefix = paste0('(',gaControl("binary")$selection,')',
                     '(',gaControl("binary")$crossover,')',
                     '(',gaControl("binary")$mutation,')')


####################
# Fitness Function #
####################
fitness <- function(string){
        
        # potential solution
        inc = which(string==1)
        # resample the data
        n = nrow(X)
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
suggestions = matrix(0, nrow=length(prime_numbers), ncol=ncol(X))
for(i in 1:length(prime_numbers))
        suggestions[i,seq(from=1,to=ncol(X), by=prime_numbers[i])] = 1 
# Get the previous creatures
destfile = file.path(getwd(),"data",paste0(file_prefix,".csv"))
if(file.exists(destfile)){
        # Read the file
        temp = read.csv(destfile)
        # Get the chromosome for the previous individuals of the same label
        previous_creatures = temp[temp$label %in% label,-2:-1]
        # Select only the most fitted individuals
        if(nrow(previous_creatures)>0){
                fitnessValue = temp[temp$label %in% label,2]
                the_most_fitted_individual = previous_creatures[which.max(fitnessValue),]
                suggestions = rbind(suggestions,data.matrix(the_most_fitted_individual))
        }
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
startTime = Sys.time()
GA <- ga(type="binary", fitness=mfitness,
         popSize=popSize,
         elitism=elitism,
         pcrossover=0.8, 
         pmutation=0.1,
         maxiter=100, run=250,
         names=colnames(X),
         nBits=ncol(X),
         suggestions=suggestions,
         parallel=TRUE,
         min=100, max=100, # not relevant in the case of type="binary"
         monitor=plot,
         seed=2047)
finishTime = Sys.time()
forget(mfitness) # clear cache
stopCluster(cl) # shut down the cluster


###########
# Summary #
###########
summary(GA)
timeDiff = round(as.numeric(finishTime-startTime, units="mins"),0)


###############
# Save Visual #
###############
file_suffix = paste0('(',timeDiff,' minutes',')',
                     '(',Sys.Date(),')')
if(DWT){
        destimage = file.path(getwd(), "reports", 
                              paste0(file_prefix,"(DWT ",2**lvl,")","(",label,")",file_suffix,".png"))
} else {
        destimage = file.path(getwd(), "reports", 
                              paste0(file_prefix,"(no encoding)","(",label,")",file_suffix,".png"))
}
dev.copy(png, destimage)
dev.off()


#######################
# Export the solution #
#######################
# Get the chromosome of the most fitted individual
solution = drop(GA@solution)
if (!is.null(dim(solution))) solution = solution[,1]
# Get the fitted value
fitnessValue = GA@fitnessValue[1]
# Record the solution
solution = data.frame(label=label,fitnessValue=fitnessValue,t(solution))

# Check if file exists then add the new solution, otherwise create a new file
if(file.exists(destfile)){
        
        temp = read.csv(destfile)
        temp = rbind(temp,solution)
        write.csv(temp, destfile, row.names=FALSE)
        
} else {
        
        write.csv(solution, destfile, row.names=FALSE)
        
}





