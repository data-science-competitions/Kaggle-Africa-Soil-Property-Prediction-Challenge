#   ____    _    
#  / ___|  / \     Genetic Algorithm
# | |  _  / _ \           for
# | |_| |/ ___ \    Subset Selection
#  \____/_/   \_\ 
#
# <https://www.jstatsoft.org/article/view/v053i04/v53i04.pdf>
labels <- c("Ca","P","pH","Sand","SOC")
label <- labels[1]
cost <- c(1e0,1e0,1e0,1e0,1e0)*1e0
names(cost) <- labels
USE_SUGGESTIONS <- FALSE # check to see whether there are past suggestions?
EXECUTION_ID <- paste0(sample(c(letters,toupper(letters),0:9,0:9),12),collapse="")


#########
# Setup #
#########
par(pty="s")
# Copy the data
X = train.infrared
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
parameters = data.frame(selection=gaControl("binary")$selection,
                        crossover=gaControl("binary")$crossover,
                        mutation=gaControl("binary")$mutation,
                        stringsAsFactors=FALSE)
# Strategy Parameter Setting
#' De Jong’s strategy parameters for online (offline) performance are:
#' popSize = 30(80)
#' pcrossover = 0.95(0.45)
#' pmutation = 0.01(0.01)
parameters$maxgen = 100
parameters$popSize = max(8*2,availableCores)
parameters$pcrossover = 0.95
parameters$pmutation = 0.1


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
destfile = file.path(getwd(),"data","features selected by GA.csv")
if(file.exists(destfile) & USE_SUGGESTIONS){
        # Read the file
        temp = read.csv(destfile)
        # Get the chromosome for the previous individuals of the same label
        previous_creatures = temp[temp$label %in% label,-12:-1]
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
         popSize=parameters$popSize,
         pcrossover=parameters$pcrossover, 
         pmutation=parameters$pmutation,
         maxiter=parameters$maxgen, #run=100,
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
# Set the folder name 
folder_name = paste0('(',parameters$selection,')',
                     '(',parameters$crossover,')',
                     '(',parameters$mutation,')')
folder_path = file.path(getwd(),'figures',folder_name)
dir.create(folder_path, recursive=TRUE, showWarnings=FALSE)
# Save Image
image_info = paste0('(',label,')',
                    '(popSize=',parameters$popSize,')',
                    '(pcrossover=',parameters$pcrossover,')',
                    '(pmutation=',parameters$pmutation,')')
image_path = file.path(folder_path,paste0(image_info,'(',EXECUTION_ID,')',".png"))
dev.copy(png, image_path)
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
solution = data.frame(executionID=EXECUTION_ID, executionDate=Sys.Date(), executionTimeInMinutes=timeDiff,
                      label=label,fitnessValue=fitnessValue,
                      as.vector(parameters),
                      t(solution))
# Check if file exists then add the new solution, otherwise create a new file
if(file.exists(destfile)){
        
        temp = read.csv(destfile)
        temp = rbind(temp,solution)
        write.csv(temp, destfile, row.names=FALSE)
        
} else {
        
        write.csv(solution, destfile, row.names=FALSE)
        
}