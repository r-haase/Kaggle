# author: Robert Haase


# load packages
library("caret")
library("plyr")
library("dplyr")  
#library("foreach")
library("DataCombine")

# showing training log



# set seed for random number generator
set.seed(1188)

# enable multicore/parallel computation and potentially show training log in console

#library("doMC")
#registerDoMC(cores = detectCores(all.tests = TRUE))
library(doSNOW)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)

# load data
peopleData <- read.csv('people.csv')
activityTrain <- read.csv('act_train.csv')
activityTest <- read.csv('act_test.csv')

# special variable char 10 for REST
char10 <- activityTrain$char_10
char10 <- as.data.frame(table(char10))
char10 <- char10[order(-char10[,2]),]

trainingData <- inner_join(activityTrain, peopleData, by = "people_id")
# subsetting or splitting the training data due based on activity category
trainingData_ONE <- trainingData[trainingData$activity_category == "type 1",]
trainingData_REST <- trainingData[trainingData$activity_category != "type 1",]
# subsetting for relevant features

##############################################################################
# Type 1
##############################################################################
trainingData_ONE <- trainingData_ONE[, -c(4,14)]
#columnIndexes <- nearZeroVar(trainingData_ONE, saveMetrics = TRUE)
#trainingData_ONE <- trainingData_ONE[,which(columnIndexes$zeroVar == FALSE)]

trainingData_ONE$date.x <- NULL
trainingData_ONE$date.y <- NULL
trainingData_ONE$group_1 <- NULL
trainingData_ONE$char_10.x <- NULL
trainingData_ONE$outcome <- as.factor(trainingData_ONE$outcome)
levels(trainingData_ONE$outcome) <- c("False","True")
trainingData_ONE$char_38 <- as.numeric(trainingData_ONE$char_38)
trainingData_ONE$people_id <- NULL
trainingData_ONE$activity_id <- NULL

##############################################################################
# REST
##############################################################################
trainingData_REST <- trainingData_REST[, -c(5:13)]
#columnIndexes <- nearZeroVar(trainingData_REST, saveMetrics = TRUE)
#trainingData_REST <- trainingData_REST[,which(columnIndexes$zeroVar == FALSE)]

trainingData_REST$date.x <- NULL
trainingData_REST$date.y <- NULL
trainingData_REST$group_1 <- NULL
#trainingData_REST$char_10.x <- NULL
# all levels of char10 with frequency > 10000
levels(trainingData_REST$char_10.x) <- c(levels(trainingData_REST$char_10.x), "default")
trainingData_REST$char_10.x[(trainingData_REST$char_10.x != "type 1") & 
                            (trainingData_REST$char_10.x != "type 2") & 
                            (trainingData_REST$char_10.x != "type 23") &
                            (trainingData_REST$char_10.x != " ") & 
                            (trainingData_REST$char_10.x != "type 61") &
                            (trainingData_REST$char_10.x != "type 452") &
                            (trainingData_REST$char_10.x != "type 489") &
                            (trainingData_REST$char_10.x != "type 52") &
                            (trainingData_REST$char_10.x != "type 481") &
                            (trainingData_REST$char_10.x != "type 433") &
                            (trainingData_REST$char_10.x != "type 8") &
                            (trainingData_REST$char_10.x != "type 3") &
                            (trainingData_REST$char_10.x != "type 450") &
                            (trainingData_REST$char_10.x != "type 649") &
                            (trainingData_REST$char_10.x != "type 899") &
                            (trainingData_REST$char_10.x != "type 464") &
                            (trainingData_REST$char_10.x != "type 400") ] <- "default" #| 
trainingData_REST$char_10.x <- factor(trainingData_REST$char_10.x)

trainingData_REST$outcome <- as.factor(trainingData_REST$outcome)
levels(trainingData_REST$outcome) <- c("False","True")
trainingData_REST$char_38 <- as.numeric(trainingData_REST$char_38)
trainingData_REST$people_id <- NULL
trainingData_REST$activity_id <- NULL
##############################################################################

###
#trainingData_REST <- trainingData_REST[sample(1:nrow(trainingData_REST), 5000, replace=FALSE),]
###

testData <- inner_join(activityTest, peopleData, by = "people_id")
testData_ONE <- testData[testData$activity_category == "type 1",]

testData_REST <- testData[testData$activity_category != "type 1",]
levels(testData_REST$char_10.x) <- c(levels(testData_REST$char_10.x), "default")
testData_REST$char_10.x[(testData_REST$char_10.x != "type 1") & 
                                    (testData_REST$char_10.x != "type 2") & 
                                    (testData_REST$char_10.x != "type 23") &
                                    (testData_REST$char_10.x != " ") & 
                                    (testData_REST$char_10.x != "type 61") &
                                    (testData_REST$char_10.x != "type 452") &
                                    (testData_REST$char_10.x != "type 489") &
                                    (testData_REST$char_10.x != "type 52") &
                                    (testData_REST$char_10.x != "type 481") &
                                    (testData_REST$char_10.x != "type 433") &
                                    (testData_REST$char_10.x != "type 8") &
                                    (testData_REST$char_10.x != "type 3") &
                                    (testData_REST$char_10.x != "type 450") &
                                    (testData_REST$char_10.x != "type 649") &
                                    (testData_REST$char_10.x != "type 899") &
                                    (testData_REST$char_10.x != "type 464") &
                                    (testData_REST$char_10.x != "type 400") ] <- "default" #| 
testData_REST$char_10.x <- factor(testData_REST$char_10.x)


# freeing storage
remove(list = c("peopleData","activityTrain","activityTest", "trainingData", "testData"))



###########################################################################################################
# prepare holdout sample
###########################################################################################################

inTraining_ONE <- createDataPartition(trainingData_ONE$outcome, p = 0.5, list = TRUE)
inTraining_REST <- createDataPartition(trainingData_REST$outcome, p = 0.10, list = TRUE)

inVal_ONE <- list((1:nrow(trainingData_ONE)))
inVal_REST <- list((1:nrow(trainingData_REST)))

inVal_ONE <- setdiff(inVal_ONE, inTraining_ONE)
inVal_REST <- setdiff(inVal_REST, inTraining_REST)

#inVal_ONE <- createDataPartition(trainingData_ONE$outcome, p = 0.5, list = FALSE, returnTrain = FALSE)
#inVal_REST <- createDataPartition(trainingData_REST$outcome, p = 0.05, list = TRUE, returnTrain = TRUE)

#trainingData_ONE <- trainingData_ONE[inTraining_ONE,]
#trainingData_REST <- trainingData_REST[inTraining_REST,]

#validation_ONE <- trainingData_ONE[-inTraining_ONE,]
#validation_REST <- trainingData_REST[-inTraining_REST,]

###########################################################################################################
# modeling
###########################################################################################################
ctrl <- trainControl(method = "cv", 
                     ## The method doesn't really matter
                     ## since we are defining the resamples
                     index= inTraining_ONE, 
                     indexOut = inVal_ONE,
                     #savePredictions = TRUE,
                     #number = 3,
                     #repeats = 1,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary, #multiClassSummary
                     verboseIter = TRUE) 

grid <-  expand.grid(eta = c(0.1, 0.2, 0.3, 0.4, 0.5),
                        nrounds = (10:20)*50,
                        colsample_bytree = c(0.5, 0.6, 0.7, 0.8, 0.9, 1),
                        max_depth = (10:20),
                        gamma = 0,
                        min_child_weight = 1)

#trainingData_ONE <- trainingData_ONE[sample(1:nrow(trainingData_ONE), 10000, replace=FALSE),]
system("say Komm budi komm!")
system("say FC Bayern, Stern des Südens!")
system("say Du Penner!")

model_ONE <- train(outcome ~ .,
                    data = trainingData_ONE,
                    method = "xgbTree",
                    metric = "ROC",
                    #tuneLength = 10,
                    #verbose = TRUE,
                    tuneGrid = grid,
                    trControl = ctrl)

system("say Fertsch!")

#trainingData_REST_small <- trainingData_REST[sample(1:nrow(trainingData_REST), 50000, replace=FALSE),]

ctrl <- trainControl(method = "cv", 
                     ## The method doesn't really matter
                     ## since we are defining the resamples
                     index= inTraining_REST, 
                     indexOut = inVal_REST,
                     #savePredictions = TRUE,
                     #number = 3,
                     #repeats = 1,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary, #multiClassSummary
                     verboseIter = TRUE) 

model_REST <- train(outcome ~ ., 
               data = trainingData_REST,
               method = "xgbTree", 
               metric = "ROC", #logLoss 
               #tuneLength = 10,
               tuneGrid = grid,
               #verbose = TRUE,
               trControl = ctrl)

system("say 10 Minuten Peti!")

#############################stopCluster(cl = NULL)

# predictions

results_ONE <- predict(model_ONE, testData_ONE, type = "prob" )[,2]
results_REST <- predict(model_REST, testData_REST, type = "prob" )[,2]

res_ONE_df <- as.data.frame(testData_ONE$activity_id)
res_ONE_df <- cbind(res_ONE_df, results_ONE)
colnames(res_ONE_df) <- c("activity_id","outcome")

res_REST_df <- as.data.frame(testData_REST$activity_id)
res_REST_df <- cbind(res_REST_df, results_REST)
colnames(res_REST_df) <- c("activity_id","outcome")

results <- rbind(res_ONE_df,res_REST_df)
results <- results[order(results[,1]),]

write.csv(results, file = "submission_AWS_ROC_attempt_char10_12levels.csv", row.names = FALSE)



# model_ONE
# 0.1796612 logloss
# parameter : The final values used for the model were nrounds = 350, max_depth = 10, eta = 0.3, gamma = 0, colsample_bytree = 0.6 and min_child_weight = 1. 

# model_REST
# 0.1738139 logloss
# parameter : The final values used for the model were nrounds = 450, max_depth = 10, eta = 0.3, gamma = 0, colsample_bytree = 0.8 and min_child_weight = 1. 

# comment: max-depth bei beiden 10



