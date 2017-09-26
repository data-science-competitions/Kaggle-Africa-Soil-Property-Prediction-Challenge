################################################################################
#                          Exploratory Data Analysis                           #
################################################################################
EXECUTION_ID <- generateID()
labels <- c("Ca","P","pH","Sand","SOC")
label <- labels[1]


#######################
# Sub Sample the Data #
#######################
set.seed(2017)
p = 35 # < 3578
train = train.infrared[,sample(ncol(train),p)] # sub sample the data
train = data.frame(train, y=train.Y[[label]])


############################
# Collinearity Diagnostics #
############################
# Correlation Plots

# Variance Inflation Factor (VIF)
#' VIF measure the inflation in the variances of the parameter estimates due to 
#' collinearities that exist among the predictors. 
#' <https://en.wikipedia.org/wiki/Variance_inflation_factor>
model <- lm(y ~ ., data=train)
model_vif = car::vif(model)
#' The last line resulted in error dut to perfect multicollinearity
#' To handle it, the first step towards the solution is to identify which 
#' variable are the culprit
model_alias = alias(model, complete=TRUE)

