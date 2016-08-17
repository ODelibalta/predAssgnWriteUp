library(caret)
library(randomForest)
# you may need to change the setwd line below or just comment it out 
setwd('/home/odelibalta/Study/Coursera/8_PracticalMachineLearning')
 

# download.file( "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "train.csv", method = "curl" )
trainData <- read.csv( "train.csv", na.strings = c("", "NA", "#DIV/0!") )

# download.file( "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "test.csv", method = "curl" )
testData <- read.csv( "test.csv", na.strings = c("", "NA", "#DIV/0!") )

 
# Delete columns with all missing values
trainData<- trainData[ , colSums( is.na( trainData) ) == 0 ]
testData <- testData[ , colSums( is.na( testData ) ) == 0 ]

# Get rid of the columns we know are irrelevant to this research
# They happen to be the one after another at the beginning 
# user_name, raw_timestamp_part_1, raw_timestamp_part_,2 cvtd_timestamp, new_window, and  num_window 
trainData <- trainData[ , -c(1:7) ]
testData  <- testData[ , -c(1:7) ]


dim(trainData)
table(trainData$classe)

set.seed(323232)
trainSet <- createDataPartition(trainData$classe, p = 0.8, list = FALSE)

train <- trainData[trainSet, ]
validation <- train[-trainSet, ]

rfModel <- randomForest( classe ~ ., data = train, importance = TRUE, ntrees = 10)

predict_training <- predict( rfModel, train )
print( confusionMatrix( predict_training, train$classe ) )


predict_validation <- predict( rfModel, validation )
print( confusionMatrix( predict_validation, validation$classe ) )


ptest <- predict(rfModel, testData)
ptest






