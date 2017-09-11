# author: Robert Haase


# load packages
library("caret")
library("plyr")
library("dplyr")  
#library("foreach")
library("DataCombine")
library("data.table")

# showing training log



# set seed for random number generator
set.seed(1188)

# enable multicore/parallel computation and potentially show training log in console

#library("doMC")
#registerDoMC(cores = detectCores(all.tests = TRUE))
library(doSNOW)
cl <- makeCluster(3, outfile="")
registerDoSNOW(cl)

# load data
setwd("/home/rstudio/Dropbox/Archiv")

peopleData <- read.csv('people.csv')
activityTrain <- read.csv('act_train.csv')
activityTest <- read.csv('act_test.csv')

# special variable char 10 for REST
char10 <- activityTrain$char_10
char10 <- as.data.frame(table(char10))
char10 <- char10[order(-char10[,2]),]
char10_sub_1x <- char10[which(char10[,2] == 1),1]

trainingData <- inner_join(activityTrain, peopleData, by = "people_id")
testData <- inner_join(activityTest, peopleData, by = "people_id")
testData$outcome <- NA

fullData <- rbind(trainingData, testData)

fullData_ONE <- fullData[fullData$activity_category == "type 1",]
fullData_REST <- fullData[fullData$activity_category != "type 1",]

##############################################################################
# Type 1
##############################################################################
fullData_ONE <- fullData_ONE[, -c(4,14)]
#columnIndexes <- nearZeroVar(fullData_ONE, saveMetrics = TRUE)
#fullData_ONE <- fullData_ONE[,which(columnIndexes$zeroVar == FALSE)]

fullData_ONE$date.x <- NULL
fullData_ONE$date.y <- NULL
fullData_ONE$group_1 <- NULL
fullData_ONE$char_10.x <- NULL
fullData_ONE$outcome <- as.factor(fullData_ONE$outcome)
levels(fullData_ONE$outcome) <- c("False","True")
fullData_ONE$char_38 <- as.numeric(fullData_ONE$char_38)
fullData_ONE$people_id <- NULL
fullData_ONE$activity_id <- NULL

##############################################################################
# REST
##############################################################################
fullData_REST <- fullData_REST[, -c(5:13)]
#columnIndexes <- nearZeroVar(fullData_REST, saveMetrics = TRUE)
#fullData_REST <- fullData_REST[,which(columnIndexes$zeroVar == FALSE)]

fullData_REST$date.x <- NULL
fullData_REST$date.y <- NULL
fullData_REST$group_1 <- NULL
#fullData_REST$char_10.x <- NULL
# all levels of char10 with frequency > 10000
levels(fullData_REST$char_10.x) <- c(levels(fullData_REST$char_10.x), c("default", "unique"))
fullData_REST$char_10.x[(fullData_REST$char_10.x != "type 1") & 
                                (fullData_REST$char_10.x != "type 2") & 
                                (fullData_REST$char_10.x != "type 23") &
                                (fullData_REST$char_10.x != " ") & 
                                (fullData_REST$char_10.x != "type 61") &
                                (fullData_REST$char_10.x != "type 452") &
                                (fullData_REST$char_10.x != "type 489") &
                                (fullData_REST$char_10.x != "type 52") &
                                (fullData_REST$char_10.x != "type 481") &
                                (fullData_REST$char_10.x != "type 433") &
                                (fullData_REST$char_10.x != "type 8") &
                                (fullData_REST$char_10.x != "type 3") &
                                (fullData_REST$char_10.x != "type 450") &
                                (fullData_REST$char_10.x != "type 649") &
                                (fullData_REST$char_10.x != "type 899") &
                                (fullData_REST$char_10.x != "type 464") &
                                (fullData_REST$char_10.x != "type 400") & 
                                (!is.element(fullData_REST$char_10.x, char10_sub_1x))] <- "default" #| 
fullData_REST$char_10.x[(is.element(fullData_REST$char_10.x, char10_sub_1x))] <- "unique"
fullData_REST$char_10.x <- factor(fullData_REST$char_10.x)

fullData_REST$outcome <- as.factor(fullData_REST$outcome)
levels(fullData_REST$outcome) <- c("False","True")
fullData_REST$char_38 <- as.numeric(fullData_REST$char_38)
fullData_REST$people_id <- NULL
fullData_REST$activity_id <- NULL

###########################################################################################################
# matrix modeling
###########################################################################################################

fullData_ONE[20:47] <- sapply(fullData_ONE[20:47], as.logical)
cols <- sapply(fullData_ONE, is.logical)
fullData_ONE[,cols] <- sapply(fullData_ONE[,cols], as.numeric)

dummies_ONE <- dummyVars(outcome ~ ., data = fullData_ONE)
dummies_ONE <- predict(dummies_ONE, newdata = fullData_ONE)
dummies_ONE <- as.data.frame(dummies_ONE)

# PCA
trans <- preProcess(dummies_ONE, method=c("YeoJohnson","center", "scale","pca", "nzv")) #"BoxCox" 
dummies_ONE <-  data.frame(trans = predict(trans, dummies_ONE))

# remerge with outcome variable
fullData_ONE <- cbind(fullData_ONE$outcome, dummies_ONE)
colnames(fullData_ONE)[1] <- "outcome"

### REST ###

fullData_REST[13:40] <- sapply(fullData_REST[13:40], as.logical)
cols <- sapply(fullData_REST, is.logical)
fullData_REST[,cols] <- sapply(fullData_REST[,cols], as.numeric)

dummies_REST <- dummyVars(outcome ~ ., data = fullData_REST)
dummies_REST <- predict(dummies_REST, newdata = fullData_REST)
dummies_REST <- as.data.frame(dummies_REST)

# PCA
trans <- preProcess(dummies_REST, method=c("YeoJohnson","center", "scale","pca", "nzv")) #"BoxCox"
dummies_REST <-  data.frame(trans = predict(trans, dummies_REST))

# remerge with outcome variable
fullData_REST<- cbind(fullData_REST$outcome, dummies_REST)
colnames(fullData_REST)[1] <- "outcome"

###########################################################################################################
# split into training and test data
###########################################################################################################

# ONE
trainingData_ONE <- fullData_ONE[!is.na(fullData_ONE$outcome),]
testData_ONE <- fullData_ONE[is.na(fullData_ONE$outcome),]
# REST
trainingData_REST <- fullData_REST[!is.na(fullData_REST$outcome),]
testData_REST <- fullData_REST[is.na(fullData_REST$outcome),]


# freeing storage
remove(list = c("peopleData","activityTrain","activityTest", "trainingData", "testData", "fullData", "fullData_ONE", "fullData_REST"))



###########################################################################################################
# prepare holdout sample
###########################################################################################################

inTraining_ONE <- createDataPartition(trainingData_ONE$outcome, p = 0.1, list = TRUE)
inTraining_REST <- createDataPartition(trainingData_REST$outcome, p = 0.01, list = TRUE)

inVal_ONE <- list((1:nrow(trainingData_ONE)))
inVal_REST <- list((1:nrow(trainingData_REST)))

inVal_ONE <- setdiff(inVal_ONE, inTraining_ONE)
inVal_REST <- setdiff(inVal_REST, inTraining_REST)



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

grid <-  expand.grid(eta = c(0.2),
                     nrounds = c(500,1000),# (10:20)*50,
                     colsample_bytree = c(0.8), #c(0.5, 0.6, 0.7, 0.8, 0.9, 1),
                     max_depth = c(20,30,40,50), #(10:20),
                     gamma = 0,
                     min_child_weight = 1)

#trainingData_ONE <- trainingData_ONE[sample(1:nrow(trainingData_ONE), 10000, replace=FALSE),]
system("say Komm budi komm!")
system("say FC Bayern, Stern des Südens!")
system.time(system("say Du Penner!"))

model_ONE <- train(outcome ~ .,
                   data = trainingData_ONE,
                   method = "xgbTree",
                   metric = "ROC",
                   #tuneLength = 3,
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

# grid <-  expand.grid(eta = c(0.2), #c(0.1, 0.2, 0.3, 0.4, 0.5),
#                      nrounds = 500, #(10:20)*50,
#                      colsample_bytree = 0.8, #c(0.5, 0.6, 0.7, 0.8, 0.9, 1),
#                      max_depth = c(20,25,30), #(10:20),
#                      gamma = 0,
#                      min_child_weight = 1)

model_REST <- train(outcome ~ ., 
                    data = trainingData_REST,
                    method = "xgbTree",
                    metric = "ROC",
                    #tuneLength = 3,
                    #verbose = TRUE,
                    tuneGrid = grid,
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

write.csv(results, file = "submission_all_in.csv", row.names = FALSE)

stopCluster(cl)


