training_data_raw  <- read.csv("train.csv")
test_data  <- read.csv("test.csv")


library("nnet")
library("caret")


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
training_data <- rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(training_data,trainingOne),trainingTwo),trainingThree),trainingFour),trainingFive),trainingSix),trainingSeven),trainingEight),trainingNine)
# shuffle the deck
training_data <- training_data[sample(nrow(training_data)),]

holdout_sample <- NULL
holdout_sample <- rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(holdout_sample,valOne),valTwo),valThree),valFour),valFive),valSix),valSeven),valEight),valNine)

targets  <- training_data[,96:104]
targets_cv  <- holdout_sample[,96:104]
predictions <- NULL
models <- NULL

cost <- NULL
## Grid search

lambdas <- c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3)
neurons <- c(90, 80, 70, 65, 60, 55)

# building nine one vs. all models (one for each class)

for (k in 1:9) {
        for (i in 1:length(lambdas)) {
                for (j in 1:length(neurons)) {
                        print(k)
                        print(i)
                        print(j)
                        
                        nnetFit <- pcaNNet(training_data[,2:94], targets[,k], maxit = 500, thresh = .95, size = neurons[j], decay = lambdas[i], MaxNWts = 10000, pc = c("center", "scale"))
                        
                        # save model
                        models <- rbind(models, c(k, lambdas[i], neurons[j], nnetFit))
                        # make predictions on holdout sample
                        pred_holdout <- predict(nnetFit, holdout_sample[,2:94])
                        # calculate and save cost
                        cost <- rbind(cost, c(k, lambdas[i], neurons[j], sum((targets_cv[,k]-pred_holdout)^2)))
                        
                        pred <- predict(nnetFit, test_data[,2:94])
                        predictions <- cbind(predictions, pred)
                }
        }
}
# add id column
#predictions <- cbind(test_data$id,predictions)
# convert matrix to data.frame in order to add names
#predictions <- as.data.frame(predictions)
#names(predictions) <- c("id","Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")
# convert id to interger for submission reasons
#predictions$id <- as.integer(predictions$id)

#write.csv(predictions, file = "./results4.csv", row.names = FALSE)

# back up code

#nnetFit1 <- nnet(training_data[,1:93],training_data$target_ONE, maxit = 2000, size = 10, decay = 1e-4)


#fitControl <- trainControl(## 10-fold CV
#        method = "repeatedcv",
#        number = 10,
## repeated ten times
#        repeats = 1)

# training
#set.seed(825)
#nnetFit1 <- train(training_data[,1:93], training_data$target_ONE,
#                method = "nnet",
#trControl = fitControl,
## This last option is actually one
## for gbm() that passes through
#              verbose = FALSE)
