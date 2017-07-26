################################################################################
#                      Multiplying the Output by a Factor                      #
################################################################################
# Choose a label
set.seed(1021)
labels <- c("Ca","P","pH","Sand","SOC")
label <- labels[1]


#####################
# Split the dataset #
#####################
p = runif(nrow(train.infrared))
X_train = train.infrared[p<0.7,]
X_test = train.infrared[p>=0.7,]
y_train = train.Y[p<0.7,label]
y_test = train.Y[p>=0.7,label]


##############################
# Fit Model to the train set #
##############################
svm_mod = e1071::svm(x=X_train, y=y_train,
                     cost=1e4,
                     kernel="radial",
                     scale=FALSE,
                     cachesize=512)


########################
# Predict the test set #
########################
y_hat = predict(svm_mod, newdata=X_test)


##############################################
# Estimate the effect of each scaling factor #
##############################################
constants = seq(0.5,1.5,length.out=1e3)
results = data.frame(row.names=constants)

for(const in constants){
        
        if(which(constants %in% const)==1) cat('\nCalculating factors effects')
        percentage_complete = 100*which(constants %in% const)/length(constants)
        if(percentage_complete %% 10 == 0) cat(paste0('\t',percentage_complete,'%'))
        
        y_hat_scale = y_hat*const
        # Bootstrap SD
        for(i in 1:200){
                s = sample(length(y_hat), replace=TRUE)
                results[toString(const),i] = postResample(y_hat_scale[s], y_test[s])[["RMSE"]]
        }
}

factor_mean = apply(results,1,mean)
factor_sd = apply(results,1,sd)
wald_lb = factor_mean - 1.96*factor_sd
wald_ub = factor_mean + 1.96*factor_sd

factor1 = factor_mean[which.min(abs(constants - 1))[1]]

        
#####################
# Visualize Results #
#####################
par(pty="s")
# Plot bootstrap means
plot(x=constants, y=factor_mean, type="l", 
     main="How multiplying the output by a factor affects RMSE?",
     ylim=c(0,1), ylab="RMSE")
abline(v=1, lty=3)
abline(h=factor1, lty=3)
# Wald confidence interval
lines(x=constants, y=wald_lb ,col="green", lty=2)
lines(x=constants, y=wald_ub ,col="green", lty=2)

x_min = constants[which.min(factor_mean)]
y_min = factor_mean[which.min(factor_mean)]
# No factor
points(rep(1,2),rep(factor1,2), col="red", pch=c('x','o'),cex=1:2)
text(x=1, y=factor1, 
     paste0("(",1,",",round(factor1,2),")"), 
     col="red", pos=3)
# Best factor
points(rep(x_min,2),rep(y_min,2), col="red", pch=c('x','o'),cex=1:2)
text(x=x_min, y=y_min, 
     paste0("(",round(x_min,2),",",round(y_min,2),")"), 
     col="red", pos=1)



