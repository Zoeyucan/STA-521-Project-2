library(tidyverse)
library(caret)
library(pROC)
#read cv models
lr_model_block = read_rds("cache/model_lr_model_block.rds")
lr_model_horizon = read_rds("cache/model_lr_model_horizon.rds")
rf_model_block = read_rds("cache/model_rf_model_block.rds")
rf_model_horizon = read_rds("cache/model_rf_model_horizon.rds")
dt_model_block = read_rds("cache/model_dt_model_block.rds")
dt_model_horizon = read_rds("cache/model_dt_model_horizon.rds")
qda_model_block = read_rds("cache/model_qda_model_block.rds")
qda_model_horizon = read_rds("cache/model_qda_model_horizon.rds")
lda_model_block = read_rds("cache/model_lda_model_block.rds")
lda_model_horizon = read_rds("cache/model_lda_model_horizon.rds")
#find_best_parameter----------------------------------------------------------
#logistic regression  tune_param = c(0,0.0005, seq(0.001,0.01,0.001))
#random forest tune_param = seq(1, 8, by=1)
#decision tree tune_param =c(0,0.0005, seq(0.001,0.03,0.001))
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
params_dt = list(block = find_best_parameter(rf_model_block,c(0,0.0005, seq(0.001,0.03,0.001))),
                 horizon = find_best_parameter(rf_model_horizon,c(0,0.0005, seq(0.001,0.03,0.001))))

#read training data
train_block = read_rds("cache/02_train_block_2_3.rds")
train_horizon = read_rds("cache/02_train_block_1_10.rds")
#training model function------------------------------------------------------------------
model_train = function(trainData, label_col = "expert_label", classifier="qda",best_params) {
  set.seed(1)
  caretctrl = trainControl(method = "none")
  # Training using Caret Wrapper
  trainData=trainData%>%
    mutate('log(SD)' = log(SD))%>%
    select(-x_coordinate,-y_coordinate,-class,-id,-SD)%>%
    mutate(across(expert_label, factor))
  colnames(trainData) <- make.names(colnames(trainData))
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
        tuneGrid = data.frame(mtry = best_params),
        trControl = caretctrl
      )
    )
  } else if (classifier == "rpart") {
    return(
      cvModel = train(
        expert_label ~ .,
        data = trainData,
        method = classifier,
        tuneLength = 1,
        tuneGrid = data.frame(cp = best_params),
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
#training model------------------------------------------------------------------
#block
model_lr_block = model_train(trainData = train_block,classifier = 'glmnet',best_params = params_lr$block)
model_rf_block = model_train(trainData = train_block,classifier = 'rf',best_params = params_rf$block)
model_dt_block = model_train(trainData = train_block,classifier = 'rpart',best_params = params_dt$block)
model_qda_block = model_train(trainData = train_block,classifier = 'qda')
model_lda_block = model_train(trainData = train_block,classifier = 'lda')

#horizon
model_lr_horizon = model_train(trainData = train_horizon,classifier = 'glmnet',best_params = params_lr$horizon)
model_rf_horizon = model_train(trainData = train_horizon,classifier = 'rf',best_params = params_rf$horizon)
model_dt_horizon = model_train(trainData = train_horizon,classifier = 'rpart',best_params = params_dt$horizon)
model_qda_horizon = model_train(trainData = train_horizon,classifier = 'qda')
model_lda_horizon = model_train(trainData = train_horizon,classifier = 'lda')

#accuracy

#read test data
test = read_rds("cache/02_test.rds")%>%
  mutate('log(SD)' = log(SD))%>%
  select(-x_coordinate,-y_coordinate,-class,-id,-SD)%>%
  mutate(across(expert_label, factor))
colnames(test) <- make.names(colnames(test))

#Get metrics
get_metrics = function(testData,model){
  y_pred_prob = predict(model, newdata = testData, type ="prob")
  roc_obj=roc(testData[["expert_label"]] ~ y_pred_prob[,"1"], smoothed=TRUE, plot=FALSE)
  #get the "best" point
  x_best = coords(roc_obj, "b", ret="fpr", best.method="closest.toplef")[[1]]
  y_best = coords(roc_obj, "b", ret="tpr", best.method="closest.toplef")[[1]]

  # Loss computation
  thres = as.numeric(coords(roc_obj, "best", "threshold")['threshold'])
  y_pred <- as.factor(ifelse(y_pred_prob[,"1"] > thres, "1", "-1"))

  testData = data.frame(obs = as.factor(c(testData[, "expert_label"])), pred =as.factor( c(y_pred)))
  #accuracy,precision,recall,f1,auc
  acc = confusionMatrix(data = testData$pred, reference = testData$obs, mode = "prec_recall")$overall[["Accuracy"]]
  pre = confusionMatrix(data = testData$pred, reference = testData$obs, mode = "prec_recall")$byClass[["Precision"]]
  rec = confusionMatrix(data = testData$pred, reference = testData$obs, mode = "prec_recall")$byClass[["Recall"]]
  f1 = confusionMatrix(data = testData$pred, reference = testData$obs, mode = "prec_recall")$byClass[["F1"]]
  auc = round(auc(roc_obj)[1], 3)
  list(roc = roc_obj, accuracy = acc, precision = pre, recall = rec, f1score = f1, AUC = auc,x_best = x_best, y_best = y_best,threshold = thres )
}
#block
metric_lr_block = get_metrics(test,model_lr_block) %>%
  write_rds("cache/metric_lr_block.rds")

metric_rf_block = get_metrics(test,model_rf_block) %>%
  write_rds("cache/metric_rf_block.rds")

metric_dt_block = get_metrics(test,model_dt_block) %>%
  write_rds("cache/metric_dt_block.rds")

metric_qda_block = get_metrics(test,model_qda_block) %>%
  write_rds("cache/metric_qda_block.rds")

metric_lda_block = get_metrics(test,model_lda_block) %>%
  write_rds("cache/metric_lda_block.rds")

#horizon
metric_lr_horizon = get_metrics(test,model_lr_horizon) %>%
  write_rds("cache/metric_lr_horizon.rds")

metric_rf_horizon = get_metrics(test,model_rf_horizon) %>%
  write_rds("cache/metric_rf_horizon.rds")

metric_dt_horizon  = get_metrics(test,model_dt_horizon) %>%
  write_rds("cache/metric_dt_horizon.rds")

metric_qda_horizon = get_metrics(test,model_qda_horizon) %>%
  write_rds("cache/metric_qda_horizon.rds")

metric_lda_horizon = get_metrics(test,model_lda_horizon) %>%
  write_rds("cache/metric_lda_horizon.rds")



