library(tidyverse)
library(caret)
metric_rf_block = read_rds("cache/metric_rf_block.rds")
metric_rf_horizon = read_rds("cache/metric_rf_horizon.rds")

#feature importance of random forest
rfimp_block = varImp(model_rf_block, scale = FALSE)
rfimp_horizon = varImp(model_rf_horizon, scale = FALSE)

rfimp_block <- data.frame(cbind(variable = rownames(rfimp_block$importance), score = rfimp_block$importance[,1]))
rfimp_block$score <- as.double(rfimp_block$score)

rfimp_block[order(rfimp_block$score,decreasing = TRUE),]
rfimp_block$set = "Block"

rfimp_horizon <- data.frame(cbind(variable = rownames(rfimp_horizon$importance), score = rfimp_horizon$importance[,1]))
rfimp_horizon$score <- as.double(rfimp_horizon$score)

rfimp_horizon[order(rfimp_horizon$score,decreasing = TRUE),]
rfimp_horizon$set = "Horizon"

df_roc_imp = bind_rows(rfimp_block,rfimp_horizon,)


varimp = ggplot(df_roc_imp, aes(x=reorder(variable, score), y=score,fill=set)) +
  #  geom_point() +
  geom_bar(stat = 'identity', position = position_dodge(0.5),width = 0.5)+
  scale_fill_manual(values=c('black','gray'))+
  #geom_segment(aes(x=variable,xend=variable,y=0,yend=score,color = set,alpha = 0.7,linewidth = 5)) +
  ylab("Variable Importance") +
  xlab("Variable Name") +
  coord_flip()+  theme_bw()
ggsave(
  "graphs/rf_varimp_image.png",
  varimp,
  width = 15,
  height = 12,
  units = "cm"
)

#confusion matrix------------------------------------------------------------
#models
model_rf_block = read_rds("cache/model_rf_block.rds")
model_rf_horizon = read_rds("cache/model_rf_horizon.rds")
#read test data
test = read_rds("cache/02_test.rds")%>%
  mutate('log(SD)' = log(SD))%>%
  select(-x_coordinate,-y_coordinate,-class,-id,-SD)%>%
  mutate(across(expert_label, factor))
colnames(test) <- make.names(colnames(test))
#block
y_pred_prob = predict(model_rf_block, newdata = test, type ="prob")
roc_obj=roc(test[["expert_label"]] ~ y_pred_prob[,"1"], smoothed=TRUE, plot=FALSE)
# Loss computation
thres = as.numeric(coords(roc_obj, "best", "threshold")['threshold'])
y_pred <- as.factor(ifelse(y_pred_prob[,"1"] > thres, "1", "-1"))

test_set = data.frame(obs = as.factor(c(test[, "expert_label"])), pred =as.factor( c(y_pred)))
block_confusion = confusionMatrix(data = test_set$pred, reference = test_set$obs, mode = "prec_recall")
#horizon
y_pred_prob = predict(model_rf_horizon, newdata = test, type ="prob")
roc_obj=roc(test[["expert_label"]] ~ y_pred_prob[,"1"], smoothed=TRUE, plot=FALSE)
# Loss computation
thres = as.numeric(coords(roc_obj, "best", "threshold")['threshold'])
y_pred <- as.factor(ifelse(y_pred_prob[,"1"] > thres, "1", "-1"))

test_set = data.frame(obs = as.factor(c(test[, "expert_label"])), pred =as.factor( c(y_pred)))
horizon_confusion = confusionMatrix(data = test_set$pred, reference = test_set$obs, mode = "prec_recall")

#test robustness
set.seed(520)
img1$imgIndicator = 1:nrow(img1)
max(img1$imgIndicator)
img2$imgIndicator = (nrow(img1) + 1):(nrow(img1)+nrow(img2))
min(img2$imgIndicator)
img3$imgIndicator = (nrow(img1)+nrow(img2) + 1):(nrow(img1)+nrow(img2)+nrow(img3))
block_split = block_wise_split(list(img1, img2, img3))
block_train = block_split$train_blocks
block_test = block_split$test_blocks
block_train_set = data.frame(rbindlist(block_train)[,-c("x", "y")])
block_test_set = data.frame(rbindlist(block_test)[,-c("x", "y")])
vertical_split = vert_wise_split(list(img1, img2, img3))
vert_train = vertical_split$train_blocks
vert_test = vertical_split$test_blocks
vert_train_set = data.frame(rbindlist(vert_train)[,-c("x", "y")])
vert_test_set = data.frame(rbindlist(vert_test)[,-c("x", "y")])
dele = c("imgIndicator", "prediction_indicator", "block_prediction_indicator", "vert_prediction_indicator")
p = sort(seq(0, 0.5, 0.05), decreasing = T)
p
p_acc_block = c()
p_acc_vert = c()
for (i in 1:length(p)) {
  p_i = p[i]
  print(p_i)
  size_perturb = as.integer(nrow(block_train_set)*p_i)
  perturb_index_block = sample(1:nrow(block_train_set), size_perturb)
  perturbed_block = block_train_set
  perturbed_block[perturb_index_block, "label"] =
    ifelse(block_train_set[perturb_index_block,"label"] == "Cloud", "Not Cloud", "Cloud")
  size_perturb = as.integer(nrow(vert_train_set)*p_i)
  perturb_index_vert = sample(1:nrow(vert_train_set), size_perturb)
  perturbed_vert = vert_train_set
  perturbed_vert[perturb_index_vert, "label"] =
    ifelse(vert_train_set[perturb_index_vert,"label"] == "Cloud", "Not Cloud", "Cloud")
  blockFinal = train(
    form = as.factor(label) ~.,
    data = perturbed_block[, !(names(perturbed_block) %in% dele)],
    method = "rf",
    tuneGrid=data.frame(mtry=1),
    tuneLength = 1,
    trControl = trainControl(method="none")
  )
  vertFinal = train(
    form = as.factor(label)~.,
    data = perturbed_vert[,!(names(perturbed_vert) %in% dele)],
    method = "rf",
    tuneGrid=data.frame(mtry=1),
    tuneLength = 1,
    trControl = trainControl(method="none")
  )
  pred_block_prob = predict(blockFinal, newdata=block_test_set[,!(names(block_test_set) %in% dele)], type="prob")
  y_pred_block <- as.character(ifelse(pred_block_prob[,"Cloud"] > block_threshold, "Cloud", "Not Cloud"))
  pred_vert_prob = predict(vertFinal, newdata=vert_test_set[,!(names(block_test_set) %in% dele)], type="prob")
  y_pred_vert <- as.character(ifelse(pred_vert_prob[,"Cloud"] > vert_threshold, "Cloud", "Not Cloud"))
  a_block = accuracy(y_pred_block, block_test_set[,"label"])
  a_vert = accuracy(y_pred_vert, vert_test_set[,"label"])
  print(a_block)
  print(a_vert)
  p_acc_block = c(p_acc_block, a_block)
  p_acc_vert = c(p_acc_vert, a_vert)
}
