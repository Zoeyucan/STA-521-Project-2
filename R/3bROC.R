#block
metric_lr_block = read_rds("cache/metric_lr_block.rds")

metric_rf_block = read_rds("cache/metric_rf_block.rds")

metric_dt_block = read_rds("cache/metric_dt_block.rds")

metric_qda_block = read_rds("cache/metric_qda_block.rds")

metric_lda_block = read_rds("cache/metric_lda_block.rds")

#horizon
metric_lr_horizon = read_rds("cache/metric_lr_horizon.rds")

metric_rf_horizon = read_rds("cache/metric_rf_horizon.rds")

metric_dt_horizon  = read_rds("cache/metric_dt_horizon.rds")

metric_qda_horizon = read_rds("cache/metric_qda_horizon.rds")

metric_lda_horizon = read_rds("cache/metric_lda_horizon.rds")



##ROC
roc.list_block <- list("logistic regression" = metric_lr_block$roc,
                       "random forest" = metric_rf_block$roc,
                       "decision tree" = metric_dt_block$roc,
                       "QDA" = metric_qda_block$roc,
                       "LDA" = metric_lda_block$roc
)
cut_off_block = data.frame(x = c(metric_lr_block$x_best,
                                 metric_rf_block$x_best,
                                 metric_dt_block$x_best,
                                 metric_qda_block$x_best,
                                 metric_lda_block$x_best),
                           y = c(metric_lr_block$y_best,
                                 metric_rf_block$y_best,
                                 metric_dt_block$y_best,
                                 metric_qda_block$y_best,
                                 metric_lda_block$y_best)
)
roc.list_horizon <- list("Logistic Regression" = metric_lr_horizon$roc,
                         "Random Forest" = metric_rf_horizon$roc,
                         "Decision Tree" = metric_dt_horizon$roc,
                         "QDA" = metric_qda_horizon$roc,
                         "LDA" = metric_lda_horizon$roc
)
cut_off_horizon = data.frame(x = c(metric_lr_horizon$x_best,
                                   metric_rf_horizon$x_best,
                                   metric_dt_horizon$x_best,
                                   metric_qda_horizon$x_best,
                                   metric_lda_horizon$x_best),
                             y = c(metric_lr_horizon$y_best,
                                   metric_rf_horizon$y_best,
                                   metric_dt_horizon$y_best,
                                   metric_qda_horizon$y_best,
                                   metric_lda_horizon$y_best)
)
block_ROC = ggroc(roc.list_block, aes= "color",legacy.axes = TRUE)+
  geom_point(data = cut_off_block,aes(x=x,y=y),color = "black") +
  scale_colour_brewer(palette="Paired",name = 'Model Type')+
  theme_bw()+
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")
horizon_ROC = ggroc(roc.list_horizon, aes= "color",legacy.axes = TRUE)+
  geom_point(data = cut_off_horizon,aes(x=x,y=y),color = "black") +
  scale_colour_brewer(palette="Paired",name = 'Model Type')+
  theme_bw()+
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")

ggsave(
  "graphs/block_ROC.png",
  block_ROC,
  width = 18,
  height = 15,
  units = "cm"
)
ggsave(
  "graphs/horizon_ROC.png",
  horizon_ROC,
  width = 18,
  height = 15,
  units = "cm"
)
