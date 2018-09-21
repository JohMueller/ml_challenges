### Data Challenge 1: Predicting whether preace agreements will fail or not

library(readxl)
data_peace <- read_excel("~/projects/ml_challenges/data_peace.xls")

names(data_peace)

# subset data
data_peace <- data_peace[, c("ended","cease", "Intarmy", "DDR", "Withd",
                       "Mil_prov", "pp", "Intgov", "Intciv","Elections",
                       "Interrim", "Natalks", "Shagov", "Pol_prov", "Aut",                       
                       "Fed", "Ind" ,"Ref", "Shaloc",
                       "Regdev", "Cul", "Demarcation","Locgov",
                       "Terr_prov",  "Amn", "pris", "Recon",
                       "Return", "Justice_prov")]

#### Create datasets for ML challenges

data_peace$Id <- paste0(1:nrow(data_peace))
peace_split <- createDataPartition(data_peace$ended, p=0.85, list=FALSE)
peace_train <- data_peace[peace_split,]
peace_test <- data_peace[-peace_split,]

# training file
write.csv(peace_train, file = "peace_train.csv")

# test file with solutions
peace_eval <- peace_test[,c("Id", "ended")]
write.csv(peace_eval, file = "peace_eval.csv", row.names = F)

# test file with features
peace_eval_feat <- peace_test[,-1]
write.csv(peace_eval_feat, file = "peace_eval_features.csv", row.names = F)

# test solution

library(caret)
library(ggplot2)


Index <- createDataPartition(peace_train$ended, p=0.90, list=FALSE)
data_train <- peace_train[Index,-30] # delete ID var
data_test <- peace_train[-Index,-30] # delete ID var

# train the model

fit.glm <- train(as.factor(ended) ~ .,
                 data=data_train, method="glm", metric="Accuracy")

summary(fit.glm)
sum(predict(fit.glm) == data_train$ended) / nrow(data_train)

# check accuracy on test data
sum(predict(fit.glm, newdata = data_test) == data_test$ended)/nrow(data_test)

# Predict test submission
predicted_probabilities <- predict(fit.glm, newdata = peace_eval_feat[,1:28], type = "prob")[2]

test_submission <- as.data.frame(cbind(peace_eval_feat$Id,
                                       predicted_probabilities))
names(test_submission) <- c("Id", "prob")

write.csv(test_submission, file = "test_submission.csv",row.names = F)


