training_data  <- read.csv("train.csv")
test_data  <- read.csv("test.csv")

library("neuralnet")
library("caret")


# might need to create binary target variables
training_data$target_ONE  <- ifelse(training_data$target == "Class_1", 1,  0)
training_data$target_TWO  <- ifelse(training_data$target == "Class_2", 1,  0)
training_data$target_THREE  <- ifelse(training_data$target == "Class_3", 1,  0)
training_data$target_FOUR  <- ifelse(training_data$target == "Class_4", 1,  0)
training_data$target_FIVE  <- ifelse(training_data$target == "Class_5", 1,  0)
training_data$target_SIX  <- ifelse(training_data$target == "Class_6", 1,  0)
training_data$target_SEVEN  <- ifelse(training_data$target == "Class_7", 1,  0)
training_data$target_EIGHT  <- ifelse(training_data$target == "Class_8", 1,  0)
training_data$target_NINE  <- ifelse(training_data$target == "Class_9", 1,  0)

targets  <- training_data[,96:104]
predictions <- NULL

for (i in 1:9) {

        nnetFit <- pcaNNet(training_data[,2:94], targets[,i], maxit = 10, size = 47, thresh = .95, decay = 1e-4, MaxNWts = 10000)
        
        pred <- predict(nnetFit, test_data[,2:94])
        
        predictions <- cbind(predictions, pred)
}
indexes <- NULL
for (i in 1:nrow(predictions)){
        indexes  <- rbind(indexes, which.max(predictions[i,]))
}

final_pred <- mat.or.vec(nrow(predictions), ncol(predictions))
for (i in 1:nrow(final_pred)){
        final_pred[i,indexes[i]] <- 1 
} 

final_pred <- cbind(test_data$id,final_pred)

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
