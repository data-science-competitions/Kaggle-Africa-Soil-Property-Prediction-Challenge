################################################################################
#                             Bootstrap SVM Models                             #
################################################################################
# Beating the benchmark in Kaggle AFSIS Challenge.
# This script is inspired from the python code published at
# http://www.kaggle.com/c/afsis-soil-properties/forums/t/10351/beating-the-benchmark
source(file.path(getwd(),"src","helper_boosted_svm.R"))
set.seed(2014)
labels <- c("Ca","P","pH","Sand","SOC")
costs <- c(1e0,1e0,1e0,1e0,1e0)*1e4
names(costs) <- labels
n_bs <- 200 # number of bootsrap models to create


#################
# Features Sets #
#################
X_te = test.infrared[,1531:3578] 
X_tr = train.infrared[,1531:3578]
Y_tr = train.Y


##############
# Fit Models #
##############
# Start CPU cluster
availableCores = detectCores()
cl <- makeCluster(availableCores, type="PSOCK", outfile="")   
registerDoSNOW(cl)
# Fit models
cat('\n',rep('#',80),
    '\n# Fitting bootstrap models to the train data',
    '\n',rep('#',80), sep="")
svm_bootstrap_models = boosted_svm_fit(X=X_tr, Y=Y_tr, n_bs, costs)
# Stop CPU cluster
stopCluster(cl)


########################
# Predict the Test set #
########################
cat('\n',rep('#',80),
    '\n# Predicting the test set',
    '\n',rep('#',80), sep="")
pred = boosted_svm_prdicet(svm_bootstrap_models, X=X_te)


##################################
# Save Predictions to Excel File #
##################################
cat('\n',rep('#',80),
    '\n# Exporting predictions to csv files',
    '\n',rep('#',80), sep="")
folder_path = file.path(getwd(),'data','bootstrap_predictions')
dir.create(folder_path, showWarnings=FALSE)

for(current_label in labels){
        file_path = file.path(folder_path,paste0(current_label,".csv"))
        write.table(x=pred[[current_label]], file=file_path, sep=",", 
                    row.names=FALSE, col.names=FALSE)
}


##################
# Visualisations #
##################
# Read the bootstrap predictions file
labels = c("Ca","P","pH","Sand","SOC")
folder_path = file.path(getwd(),'data','bootstrap_predictions')
pred = list()
for(current_label in labels){
        file_path = file.path(folder_path,paste0(current_label,".csv"))
        pred[[current_label]] = read.csv(file_path, header=FALSE)
}
# Interactive Plotting with Manipulate
par(mfrow=c(2,2))
xlim=t(apply(Y_tr,2,range))
n_obs = nrow(pred[[1]])
manipulate(
        # Plot
        for(current_label in setdiff(labels,"P")){
                obs = unlist(pred[[current_label]][obs_id,])
                dens = density(obs)
                plot(dens, 
                     xlim=xlim[current_label,], main=current_label)
                points(obs, 0*obs, pch='|', col="red")
        },
        # Slider
        obs_id=slider(1,727,initial=sample(727,1),step=1)
)
