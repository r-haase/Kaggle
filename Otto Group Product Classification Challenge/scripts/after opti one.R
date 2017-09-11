training_data_raw  <- read.csv("train.csv")
test_data  <- read.csv("test.csv")


library("nnet")
library("caret")


# might need to create binary target variables
training_data_raw$target_ONE  <- ifelse(training_data_raw$target == "Class_1", 1,  0)
training_data_raw$target_TWO  <- ifelse(training_data_raw$target == "Class_2", 1,  0)
training_data_raw$target_THREE  <- ifelse(training_data_raw$target == "Class_3", 1,  0)
training_data_raw$target_FOUR  <- ifelse(training_data_raw$target == "Class_4", 1,  0)
training_data_raw$target_FIVE  <- ifelse(training_data_raw$target == "Class_5", 1,  0)
training_data_raw$target_SIX  <- ifelse(training_data_raw$target == "Class_6", 1,  0)
training_data_raw$target_SEVEN  <- ifelse(training_data_raw$target == "Class_7", 1,  0)
training_data_raw$target_EIGHT  <- ifelse(training_data_raw$target == "Class_8", 1,  0)
training_data_raw$target_NINE  <- ifelse(training_data_raw$target == "Class_9", 1,  0)

# creat hold out sample for cv
set.seed(1188)
holdout_sample <- training_data_raw[sample(1:nrow(training_data), 10000, replace=FALSE),]
training_data <- training_data_raw[-sample(1:nrow(training_data), 10000, replace=FALSE),]



targets  <- training_data[,96:104]
targets_cv  <- holdout_sample[,96:104]
predictions <- NULL
models <- NULL

cost <- NULL
## Grid search

# building nine one vs. all models (one for each class)
        
        # class one
        nnetFit <- pcaNNet(training_data[,2:94], targets[,1], maxit = 2000, thresh = .95, size = 60, decay = 0.1, MaxNWts = 10000, pc = c("center", "scale"))
        pred <- predict(nnetFit, test_data[,2:94])
        predictions <- cbind(predictions, pred)
        
        # class two
        nnetFit <- pcaNNet(training_data[,2:94], targets[,2], maxit = 2000, thresh = .95, size = 60, decay = 0.001, MaxNWts = 10000, pc = c("center", "scale"))
        pred <- predict(nnetFit, test_data[,2:94])
        predictions <- cbind(predictions, pred)

        # class three
        nnetFit <- pcaNNet(training_data[,2:94], targets[,3], maxit = 2000, thresh = .95, size = 60, decay = 0.03, MaxNWts = 10000, pc = c("center", "scale"))
        pred <- predict(nnetFit, test_data[,2:94])
        predictions <- cbind(predictions, pred)

        # class four
        nnetFit <- pcaNNet(training_data[,2:94], targets[,4], maxit = 2000, thresh = .95, size = 60, decay = 0.03, MaxNWts = 10000, pc = c("center", "scale"))
        pred <- predict(nnetFit, test_data[,2:94])
        predictions <- cbind(predictions, pred)

        # class five
        nnetFit <- pcaNNet(training_data[,2:94], targets[,5], maxit = 2000, thresh = .95, size = 60, decay = 0.01, MaxNWts = 10000, pc = c("center", "scale"))
        pred <- predict(nnetFit, test_data[,2:94])
        predictions <- cbind(predictions, pred)

        # class six
        nnetFit <- pcaNNet(training_data[,2:94], targets[,6], maxit = 2000, thresh = .95, size = 60, decay = 0.1, MaxNWts = 10000, pc = c("center", "scale"))
        pred <- predict(nnetFit, test_data[,2:94])
        predictions <- cbind(predictions, pred)

        # class seven
        nnetFit <- pcaNNet(training_data[,2:94], targets[,7], maxit = 2000, thresh = .95, size = 60, decay = 0.03, MaxNWts = 10000, pc = c("center", "scale"))
        pred <- predict(nnetFit, test_data[,2:94])
        predictions <- cbind(predictions, pred)

        # class eight
        nnetFit <- pcaNNet(training_data[,2:94], targets[,8], maxit = 2000, thresh = .95, size = 60, decay = 0.1, MaxNWts = 10000, pc = c("center", "scale"))
        pred <- predict(nnetFit, test_data[,2:94])
        predictions <- cbind(predictions, pred)

        # class one
        nnetFit <- pcaNNet(training_data[,2:94], targets[,9], maxit = 2000, thresh = .95, size = 60, decay = 0.1, MaxNWts = 10000, pc = c("center", "scale"))
        pred <- predict(nnetFit, test_data[,2:94])
        predictions <- cbind(predictions, pred)
 


# add id column
predictions <- cbind(test_data$id,predictions)
# convert matrix to data.frame in order to add names
predictions <- as.data.frame(predictions)
names(predictions) <- c("id","Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")
# convert id to interger for submission reasons
predictions$id <- as.integer(predictions$id)

write.csv(predictions, file = "./resultsOpti2.csv", row.names = FALSE)