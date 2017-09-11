training_data  <- read.csv("train.csv")
test_data  <- read.csv("test.csv")

library("nnet")
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
set.seed(825)
# building nine one vs. all models (one for each class)
for (i in 1:9) {
        
        nnetFit <- pcaNNet(training_data[,2:94], targets[,i], maxit = 2000, thresh = .95, size = 39, , decay = 1e-4, MaxNWts = 10000, pc = c("YeoJohnson","center", "scale"))
        
        pred <- predict(nnetFit, test_data[,2:94])
        
        predictions <- cbind(predictions, pred)
}

# add id column
predictions <- cbind(test_data$id,predictions)
# convert matrix to data.frame in order to add names
predictions <- as.data.frame(predictions)
names(predictions) <- c("id","Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")
# convert id to interger for submission reasons
predictions$id <- as.integer(predictions$id)

write.csv(predictions, file = "./results5.csv", row.names = FALSE)

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
