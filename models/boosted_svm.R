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
pred = boosted_svm_predict(svm_bootstrap_models, X=X_te)


##################################
# Save Predictions to Excel File #
##################################
cat('\n',rep('#',80),
    '\n# Exporting predictions to csv files',
    '\n',rep('#',80), sep="")
folder_path = file.path(getwd(),'data','predictions')
dir.create(folder_path, showWarnings=FALSE)

model_type = 'svm'
set_type = ifelse(nrow(pred[[1]])==nrow(X_tr), "train", "test")

for(current_label in labels){
        file_name = paste0('(',model_type,')','(',set_type,')','(',current_label,')','.csv')
        file_path = file.path(folder_path,file_name)
        write.table(x=pred[[current_label]], file=file_path, sep=",", 
                    row.names=TRUE, col.names=FALSE)
}


##################
# Visualisations #
##################
# Read the bootstrap predictions file
labels = c("Ca","P","pH","Sand","SOC")
chosen_model = c('svm')[1]
chosen_set = c('train','test')[1]

folder_path = file.path(getwd(),'data','predictions')
pred = list()
for(current_label in labels){
        file_name = paste0('(',chosen_model,')','(',chosen_set,')','(',current_label,')','.csv')
        file_path = file.path(folder_path,file_name)
        DF = read.csv(file_path, header=FALSE)
        row.names(DF) = DF[,1]
        pred[[current_label]] = as.matrix(DF[,-1])
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
                # Add the train set real value (if applicable)
                if(n_obs==1157){ 
                        abline(v=Y_tr[obs_id,current_label], col="blue", lty=2)
                        abline(v=mean(obs,trim=0), col="red", lty=3)
                }
        },
        # Slider
        obs_id=slider(1,n_obs,initial=sample(n_obs,1),step=1)
)


################################
# Bootstrap RMSE for Train set #
################################
par(mfrow=c(1,1))
if(nrow(pred[[1]])==1157){
        
        RMSE_table = data.frame(row.names=1:n_bs)
        for(current_label in labels){
                
                RMSE_current_label = apply(pred[[current_label]], 2,
                                           function(x) RMSE(x,train.Y[,current_label]))
                RMSE_table[,current_label] = RMSE_current_label
                
        }
        # What is the estimated leaderboard score?
        plot(density(rowMeans(RMSE_table)))
        # What is the RMSE for each label?
        colMeans(RMSE_table)
        
}



