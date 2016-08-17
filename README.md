Prediction Assignment Writeup
================
Okan Delibalta
8/16/2016

Introduction
------------

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

Basisc and Data
---------------

Work directory for the script is changed below. You may need to edit the line if you want to run the code on your machine or just comment it out. Code below downloads the data, reads it as a csv and cleans up the empty values.

``` r
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

``` r
library(randomForest)
```

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
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
```

Lets take a look at the data we have so far.

``` r
dim(trainData)
```

    ## [1] 19622    53

``` r
table(trainData$classe)
```

    ## 
    ##    A    B    C    D    E 
    ## 5580 3797 3422 3216 3607

PreProcess
==========

Partition the train set
-----------------------

Recall from the lectures that at the beginning we focus on the training set alone. We are going to make an %80, %20 split for training and validation. Most of the functioanlity we need is included in the caret package.

``` r
set.seed(323232)
trainSet <- createDataPartition(trainData$classe, p = 0.8, list = FALSE)

train <- trainData[trainSet, ]
validation <- train[-trainSet, ]
```

Picking our model
-----------------

If you have not noticed it yet, one of the libraries we have loaded is the randomForest library which we will use for this. We do not have a big dataset so we are shooting for accuracy with selecting randomForest. We will watch out for overfitting in the upcoming sections.

``` r
rfModel <- randomForest( classe ~ ., data = train, importance = TRUE, ntrees = 10)
```

Validating the model
--------------------

Curious to see how accurate we are ?

``` r
predict_training <- predict( rfModel, train )
print( confusionMatrix( predict_training, train$classe ) )
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 4464    0    0    0    0
    ##          B    0 3038    0    0    0
    ##          C    0    0 2738    0    0
    ##          D    0    0    0 2573    0
    ##          E    0    0    0    0 2886
    ## 
    ## Overall Statistics
    ##                                      
    ##                Accuracy : 1          
    ##                  95% CI : (0.9998, 1)
    ##     No Information Rate : 0.2843     
    ##     P-Value [Acc > NIR] : < 2.2e-16  
    ##                                      
    ##                   Kappa : 1          
    ##  Mcnemar's Test P-Value : NA         
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Prevalence             0.2843   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2843   0.1935   0.1744   0.1639   0.1838
    ## Detection Prevalence   0.2843   0.1935   0.1744   0.1639   0.1838
    ## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000

Model performs well and hits the sweet spot for me. If it was %100 all the way, I would be worried about the out of sample error and overfitting. We will check for those of course but so far it is not as worrisome.

Validation on the validation set - our out of sample test
---------------------------------------------------------

``` r
predict_validation <- predict( rfModel, validation )
print( confusionMatrix( predict_validation, validation$classe ) )
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   A   B   C   D   E
    ##          A 892   0   0   0   0
    ##          B   0 622   0   0   0
    ##          C   0   0 548   0   0
    ##          D   0   0   0 499   0
    ##          E   0   0   0   0 569
    ## 
    ## Overall Statistics
    ##                                      
    ##                Accuracy : 1          
    ##                  95% CI : (0.9988, 1)
    ##     No Information Rate : 0.285      
    ##     P-Value [Acc > NIR] : < 2.2e-16  
    ##                                      
    ##                   Kappa : 1          
    ##  Mcnemar's Test P-Value : NA         
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity             1.000   1.0000   1.0000   1.0000   1.0000
    ## Specificity             1.000   1.0000   1.0000   1.0000   1.0000
    ## Pos Pred Value          1.000   1.0000   1.0000   1.0000   1.0000
    ## Neg Pred Value          1.000   1.0000   1.0000   1.0000   1.0000
    ## Prevalence              0.285   0.1987   0.1751   0.1594   0.1818
    ## Detection Rate          0.285   0.1987   0.1751   0.1594   0.1818
    ## Detection Prevalence    0.285   0.1987   0.1751   0.1594   0.1818
    ## Balanced Accuracy       1.000   1.0000   1.0000   1.0000   1.0000

Our cross validation is at %95 which is acceptable


Final step - Test set
---------------------

``` r
predict_test <- predict(rfModel, testData)
predict_test
```

    ##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
    ##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
    ## Levels: A B C D E
