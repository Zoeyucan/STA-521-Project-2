library(tidyverse)
library(caret)
#read cv models
lr_model_block = read_rds("cache/model_lr_model_block.rds")
lr_model_horizon = read_rds("cache/model_lr_model_horizon.rds")
rf_model_block = read_rds("cache/model_rf_model_block.rds")
rf_model_horizon = read_rds("cache/model_rf_model_horizon.rds")
qda_model_block = read_rds("cache/model_qda_model_block.rds")
qda_model_horizon = read_rds("cache/model_qda_model_horizon.rds")
lda_model_block = read_rds("cache/model_lda_model_block.rds")
lda_model_horizon = read_rds("cache/model_lda_model_horizon.rds")
#find_best_parameter----------------------------------------------------------
#logistic regression  tune_param = c(0,0.0005, seq(0.001,0.01,0.001))
#random forest tune_param = seq(1, 8, by=1)
find_best_parameter = function(model, tune_param) {
  accuracy_mean = colMeans(model$accuracy)
  best_parameter = tune_param[which.max(accuracy_mean)]
  return(best_parameter)
}

#best params
params_lr = list(block = find_best_parameter(lr_model_block,c(0,0.0005, seq(0.001,0.01,0.001))),
                 horizon = find_best_parameter(lr_model_horizon,c(0,0.0005, seq(0.001,0.01,0.001))))
params_rf = list(block = find_best_parameter(rf_model_block,seq(1, 8, by=1)),
                 horizon = find_best_parameter(rf_model_horizon,seq(1, 8, by=1)))

#read training data
train_block = read_rds("cache/02_train_block_2_3.rds")
train_horizon = read_rds("cache/02_train_block_1_10.rds")

#training model
model_train = function(trainData, label_col = "expert_label", classifier="qda",best_params) {
  set.seed(1)
  caretctrl = trainControl(method = "none")
  # Training using Caret Wrapper
  if (classifier == "glmnet") {
    return(
      train(
        expert_label ~ .,
        data = trainData,
        method = classifier,
        family = "binomial",
        preProcess = c("center", "scale"),
        tuneLength = 1,
        tuneGrid = data.frame(alpha = 1, lambda = best_params),
        trControl = caretctrl
      )
    )
  } else if (classifier == "rf") {
    return(
      train(
        expert_label ~ .,
        data = trainData,
        method = classifier,
        tuneLength = 1,
        tuneGrid = data.frame(mtry = tp),
        trControl = caretctrl
      )
    )
  } else if (classifier == "svm") {
    return(
      train(
        expert_label ~ .,
        data = trainData,
        method = "svmLinear",
        tuneLength = 1,
        tuneGrid = data.frame(C = tp),
        preProcess = c("center", "scale"),
        trControl = caretctrl
      )
    )
  } else {
    # for lda and qda, which do not have hyper parameter
    return(
      train(
        expert_label ~ .,
        data = trainData,
        method = classifier,
        preProcess = c("center", "scale"),
        tuneLength = 1,
        trControl = caretctrl
      )
    )
  }
}

