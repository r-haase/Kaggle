training_data_raw  <- read.csv("train.csv")
test_data  <- read.csv("test.csv")

#preprocess highly skewed data by take the log of the features
target <- training_data_raw$target
id <- training_data_raw$id

training_data_raw <- training_data_raw[,c(-1,-95)] +1
training_data_raw <- sapply(training_data_raw, log) 
training_data_raw <- as.data.frame(training_data_raw)
training_data_raw  <- cbind(id, training_data_raw, target)

# same for test data
id <- test_data$id
test_data <- test_data[,-1] +1
test_data <- sapply(test_data, log)
test_data  <- cbind(id, test_data)
test_data <- as.data.frame(test_data)

library("caret")
library("e1071")


# might need to create binary target variables
training_data_raw$target_ONE    <- ifelse(training_data_raw$target == "Class_1", 1,  0)
training_data_raw$target_TWO    <- ifelse(training_data_raw$target == "Class_2", 1,  0)
training_data_raw$target_THREE  <- ifelse(training_data_raw$target == "Class_3", 1,  0)
training_data_raw$target_FOUR   <- ifelse(training_data_raw$target == "Class_4", 1,  0)
training_data_raw$target_FIVE   <- ifelse(training_data_raw$target == "Class_5", 1,  0)
training_data_raw$target_SIX    <- ifelse(training_data_raw$target == "Class_6", 1,  0)
training_data_raw$target_SEVEN  <- ifelse(training_data_raw$target == "Class_7", 1,  0)
training_data_raw$target_EIGHT  <- ifelse(training_data_raw$target == "Class_8", 1,  0)
training_data_raw$target_NINE   <- ifelse(training_data_raw$target == "Class_9", 1,  0)

# create new training data set with equally distributed number of rows and all thats left is for the hold out sample for cv
set.seed(1188)
classOne        <- training_data_raw[training_data_raw$target_ONE == 1,]
classTwo        <- training_data_raw[training_data_raw$target_TWO == 1,]
classThree      <- training_data_raw[training_data_raw$target_THREE == 1,]
classFour       <- training_data_raw[training_data_raw$target_FOUR == 1,]
classFive       <- training_data_raw[training_data_raw$target_FIVE == 1,]
classSix        <- training_data_raw[training_data_raw$target_SIX == 1,]
classSeven      <- training_data_raw[training_data_raw$target_SEVEN == 1,]
classEight      <- training_data_raw[training_data_raw$target_EIGHT == 1,]
classNine       <- training_data_raw[training_data_raw$target_NINE == 1,]

# subsample 
trainingOne     <- classOne[sample(1:nrow(classOne), 1500, replace=FALSE),]
valOne          <- classOne[-sample(1:nrow(classOne), 1500, replace=FALSE),]

trainingTwo     <- classTwo[sample(1:nrow(classTwo), 1500, replace=FALSE),]
valTwo          <- classTwo[-sample(1:nrow(classTwo), 1500, replace=FALSE),]

trainingThree     <- classThree[sample(1:nrow(classThree), 1500, replace=FALSE),]
valThree          <- classThree[-sample(1:nrow(classThree), 1500, replace=FALSE),]

trainingFour     <- classFour[sample(1:nrow(classFour), 1500, replace=FALSE),]
valFour          <- classFour[-sample(1:nrow(classFour), 1500, replace=FALSE),]

trainingFive     <- classFive[sample(1:nrow(classFive), 1500, replace=FALSE),]
valFive          <- classFive[-sample(1:nrow(classFive), 1500, replace=FALSE),]

trainingSix     <- classSix[sample(1:nrow(classSix), 1500, replace=FALSE),]
valSix          <- classSix[-sample(1:nrow(classSix), 1500, replace=FALSE),]

trainingSeven     <- classSeven[sample(1:nrow(classSeven), 1500, replace=FALSE),]
valSeven          <- classSeven[-sample(1:nrow(classSeven), 1500, replace=FALSE),]

trainingEight     <- classEight[sample(1:nrow(classEight), 1500, replace=FALSE),]
valEight          <- classEight[-sample(1:nrow(classEight), 1500, replace=FALSE),]

trainingNine     <- classNine[sample(1:nrow(classNine), 1500, replace=FALSE),]
valNine          <- classNine[-sample(1:nrow(classNine), 1500, replace=FALSE),]

#################################################################################
# merging
#################################################################################
training_data <- NULL
training_data <- rbind(training_data,trainingOne,trainingTwo,trainingThree,trainingFour,trainingFive,trainingSix,trainingSeven,trainingEight,trainingNine)
# shuffle the deck
training_data <- training_data[sample(nrow(training_data)),]

holdout_sample <- NULL
holdout_sample <- rbind(holdout_sample,valOne,valTwo,valThree,valFour,valFive,valSix,valSeven,valEight,valNine)

targets  <- training_data[,96:104]
targets_cv  <- holdout_sample[,96:104]
predictions <- NULL
models <- NULL

cost <- NULL
## Grid search


# setting up the formula for the model training
target_string <- "target_ONE+target_TWO+target_THREE+target_FOUR+target_FIVE+target_SIX+target_SEVEN+target_EIGHT+target_NINE ~ "
xnam <- paste("feat_", 1:93, sep="")
fmla <- as.formula(paste(target_string, paste(xnam, collapse= "+")))


gammas <- c(0.003, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1)


for (i in 1:length(gammas)) {
        print(i)
        
        
        mySVM <- svm(as.matrix(training_data[,2:94]), as.factor(training_data[,95]) , kernel = "radial", scale = TRUE, probability = TRUE, gamma = gammas[i])
        
        # save model
        # models <- rbind(models, c(k, lambdas[i], neurons[j], nnetFit))
        # make predictions on holdout sample
        pred_holdout <- predict(mySVM, holdout_sample[,2:94], probability = TRUE)
        pred_holdout <- attr(pred_holdout, "probabilities")
        predictions <- pred_holdout[,c("Class_1", "Class_2", "Class_3", "Class_4", "Class_5", "Class_6", "Class_7", "Class_8", "Class_9")]
        # calculate and save cost
        cost <- rbind(cost, sum((targets_cv-predictions)^2))
        
        pred <- predict(mySVM, test_data[,2:94], probability = TRUE)
        pred <- attr(pred, "probabilities")
        predictions <- pred[,c("Class_1", "Class_2", "Class_3", "Class_4", "Class_5", "Class_6", "Class_7", "Class_8", "Class_9")]
        #predictions <- cbind(predictions, pred)
}
# add id column
predictions <- cbind(test_data$id,predictions)
# convert matrix to data.frame in order to add names
predictions <- as.data.frame(predictions)
names(predictions) <- c("id","Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")
# convert id to interger for submission reasons
predictions$id <- as.integer(predictions$id)

write.csv(predictions, file = "./resultsMultiClassSVM_Gamma0.02LOGGED.csv", row.names = FALSE)
