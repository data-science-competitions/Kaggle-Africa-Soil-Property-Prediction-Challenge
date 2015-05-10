## Load the original data
train <- read.csv("./data/train.csv.gz")   
test  <- read.csv("./data/test.csv.gz")   
sampleSubmission  <- read.csv("./data/sampleSubmission.csv.gz") 
Y_labels <- c("Ca","P","pH","SOC","Sand")
## Split the variables into 2 groups
train.infrared <- train[,2:3579]
train.spatial  <- train[,3580:3595]
train.Y        <- train[,Y_labels]
test.infrared <- test[,2:3579]
test.spatial  <- test[,3580:3595]
##
rm("train","test","sampleSubmission")