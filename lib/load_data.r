## Load the original data
train <- read.csv("./data/train.csv.gz")   
test  <- read.csv("./data/test.csv.gz")   
sampleSubmission  <- read.csv("./data/sampleSubmission.csv.gz") 
Y_labels <- c("Ca","P","pH","SOC","Sand")
## Split the variables into 2 groups
#(1:ncol(train))[names(train) %in% "m7497.96"] # 2
#(1:ncol(train))[names(train) %in% "m3833.84"] # 1902
#train.infrared <- train[,2:3579]
#test.infrared  <- test[,2:3579]
## * Full spectrum = [2:3579]
## * Relevant spectrum = [1902:2579]
train.infrared <- train[,1902:3579]
train.spatial  <- train[,3580:3595]
train.Y        <- train[,Y_labels]
test.infrared <- test[,1902:3579]
test.spatial  <- test[,3580:3595]
rm("train","test","sampleSubmission")



