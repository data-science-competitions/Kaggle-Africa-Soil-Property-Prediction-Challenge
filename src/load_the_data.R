################################################################################
#                                Load the Data                                 #
################################################################################
train <- read.csv(file.path(getwd(),"data","train.csv.gz"))   
test <- read.csv(file.path(getwd(),"data","test.csv.gz"))   
sampleSubmission <- read.csv(file.path(getwd(),"data","sampleSubmission.csv.gz"))    


#########################
# Shuffle the Train set #
#########################
set.seed(280717)
train = train[sample(nrow(train)),]


#####################################
# Split the variables into 2 groups #
#####################################
#' * Full spectrum = [2:3579]
#' * Relevant spectrum = [1902:2579]
train.infrared <- train[,2:3579]
train.spatial <- train[,3580:3595]

test.infrared <- test[,2:3579]
test.spatial <- test[,3580:3595]

Y_labels = c("Ca","P","pH","SOC","Sand")
train.Y <- train[,Y_labels]

rm("train","test")

