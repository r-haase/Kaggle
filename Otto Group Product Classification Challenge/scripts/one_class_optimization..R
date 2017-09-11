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

lambdas <- c(0, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 3, 10)
neurons <- c(60,40,20)

# building nine one vs. all models (one for each class)

for (k in 1:9) {
        for (i in 1:length(lambdas)) {
            for (j in 1:length(neurons)) {
                print(k)
                print(i)
                print(j)
                    
                nnetFit <- pcaNNet(training_data[,2:94], targets[,k], maxit = 1000, thresh = .95, size = neurons[j], decay = lambdas[i], MaxNWts = 10000, pc = c("center", "scale"))
                
                # save model
                models <- rbind(models, k, lambdas[i], neurons[j], nnetFit)
                # make predictions on holdout sample
                pred_holdout <- predict(nnetFit, holdout_sample[,2:94])
                # calculate and save cost
                cost <- rbind(cost, k, lambdas[i], neurons[j], sum((targets_cv[,k]-pred_holdout)^2))
                
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
