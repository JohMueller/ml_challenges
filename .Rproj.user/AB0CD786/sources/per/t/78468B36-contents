library(readr)
library(caret)
library(ggplot2)
library(readxl)

# Load and clean the data
data_blood <- read_csv("~/projects/ml_challenges/data_blood.csv")

names(data_blood) <- c("ID", "months_since_last", "number_donations",
                 "total_volume", "months_since_first", "made_donation")

data_blood[2:5] <- sapply(data_blood[2:5], as.numeric)
data_blood$made_donation <- as.factor(data_blood$made_donation)
data_blood$ID <- NULL

# Preprocessing the data

# pair-wise plots of all 4 attributes, dots colored by class
featurePlot(x=data_blood[,1:4], 
            y=data_blood[,5], 
            plot="pairs" 
            #auto.key=list(columns=3)
            )


#### Create datasets for ML challenges

  data_blood$ID <- as.character(data_blood$ID)
  blood_split <- createDataPartition(data_blood$made_donation, p=0.85, list=FALSE)
  blood_train <- data_blood[blood_split,]
  blood_test <- data_blood[-blood_split,]
  
  # training file
  write.csv(blood_train, file = "blood_train.csv")
  
  # test file with solutions
  blood_eval <- blood_test[,c("ID", "made_donation")]
  write.csv(blood_eval, file = "blood_solution.csv", row.names = F)
  
  # test file with features
  blood_eval_feat <- blood_test[,-6]
  write.csv(blood_eval_feat, file = "blood_test.csv", row.names = F)


# split into training and test data
Index <- createDataPartition(blood_train$made_donation, p=0.80, list=FALSE)
data_train <- blood_train[Index,]
data_test <- blood_train[-Index,]


# train the model

fit.glm <- train(as.factor(as.character(made_donation)) ~ months_since_last + number_donations + months_since_first,
                 data=data_train, method="glm") # , metric="Accuracy"

summary(fit.glm)
predict(fit.glm)

# check accuracy on test data
predict(fit.glm, newdata = data_test) == data_test$made_donation
sum(predict(fit.glm, newdata = blood_eval_feat) == blood_eval$made_donation)/nrow(blood_eval)

### write submission file

predicted_probabilities <- predict(fit.glm, newdata = blood_eval_feat[,2:5], type = "prob")[2]
test_submission <- as.data.frame(cbind(blood_eval_feat$ID, predicted_probabilities))
names(test_submission) <- c("ID", "prob")
write.csv(test_submission, file = "blood_test_submission.csv",row.names = F)



