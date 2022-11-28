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

metric.list_block = list(metric_lr_block,

                         metric_rf_block,

                         metric_dt_block,

                         metric_qda_block,

                         metric_lda_block
)
#print all metrics
for(i in metric.list_block){
  print(paste0("acc: ",round(i$accuracy,3),
               " precision: ",round(i$precision,3),
               " recall: ",round(i$recall,3),
               " f1: ",round(i$f1score,3),
               " AUC: ",round(i$AUC,3),
               " threshold",round(i$threshold,3)
               ))
}
metric.list_horizon = list(metric_lr_horizon,

                         metric_rf_horizon,

                         metric_dt_horizon,

                         metric_qda_horizon,

                         metric_lda_horizon
)
#print all metrics
for(i in metric.list_horizon){
  print(paste0("acc: ",round(i$accuracy,3),
               " precision: ",round(i$precision,3),
               " recall: ",round(i$recall,3),
               " f1: ",round(i$f1score,3),
               " AUC: ",round(i$AUC,3),
               " threshold",round(i$threshold,3)
  ))
}
