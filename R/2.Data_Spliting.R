library(tidyverse)
library(splitTools)
library(caret)
library(mlbench)
#part a--------------------------------------------------------------

# splitting testing and training data
image = read_rds("cache/image.rds")
#make this example reproducible
set.seed(1)
image$id = 1:nrow(image)
#Use 70% of dataset as training set and remaining 30% as testing set
train = image %>% dplyr::sample_frac(0.7)
test_block  = dplyr::anti_join(image, train, by = 'id')

#find the coordinate of each block
find_blocks = function(x, y, cols = 2,rows = 3) {
  x_min = min(x)
  x_max = max(x)
  x_step = (x_max-x_min)/cols

  y_min = min(y)
  y_max = max(y)
  y_step = (y_max-y_min)/rows

  block = tibble()

  for (i in seq_len(rows)) {
    # make top and bottom
    top = floor(y_min + (i-1) * y_step)
    bot = floor(y_max - (rows-i) * y_step )
    if (i != rows)
      bot = bot - 1

    for (j in seq_len(cols)){
      # make left and right
      left = floor(x_min + (j-1) * x_step)
      right = floor(x_max - (cols-j) * x_step )
      if (j != cols)
        right = right - 1

      block %>%
        bind_rows(data.frame(top, bot, left, right)) -> block
    }
  }
  names(block) = c("top", "bottom", "left", "right")
  return(block)
}


split_blocks = function(block, cols = 2, rows = 3, train_num_blocks = 5, val_num_blocks = 1) {
  set.seed(1)
  train_index = sample(seq_len(cols*rows),train_num_blocks)
  set.seed(11)
  val_index = sample(setdiff(seq_len(cols*rows),train_index),val_num_blocks)

  train_blocks = block[train_index,]
  val_blocks = block[val_index,]
  return(list(train = train_blocks, val = val_blocks))
}

split_data = function(data, block) {
  final_data = tibble()
  for (i in seq_len(nrow(block))) {
    coords = block[i,]

    data %>%
      filter(x_coordinate >= coords$left, x_coordinate <= coords$right,
             y_coordinate >= coords$top, y_coordinate <= coords$bottom) -> filtered_data

    final_data = rbind(final_data, filtered_data)
  }

  return(final_data)
}

split_blocks_main = function(df, cols = 2, rows = 3, train_num_blocks, val_num_blocks){
  blocks = find_blocks(df$x_coordinate, df$y_coordinate)
  block_indices = split_blocks(blocks, cols, rows,
                                train_num_blocks, val_num_blocks)

  val = split_data(df, block_indices$val)
  train = split_data(df, block_indices$train)
  return(list(val = val, train = train))
}

image_dfs_1 = split_blocks_main(train, cols = 2, rows = 3, train_num_blocks = 5, val_num_blocks = 1)
image_dfs_2 = split_blocks_main(train, cols = 1, rows = 10, train_num_blocks = 8, val_num_blocks = 2)
#method1
train_block_2_3 = image_dfs_1$train
val_block_2_3 = image_dfs_1$val
#method2
train_block_1_10 = image_dfs_2$train
val_block_1_10 = image_dfs_2$val

#part b------------------------------------------------------------------

#method1:block 2*3
train_block_2_3 %>%
  filter(expert_label != 0) %>%
  summarise(acc = mean(expert_label == -1))

val_block_2_3%>%
  filter(expert_label != 0) %>%
  summarise(acc = mean(expert_label == -1))

#method2: block 1*10
# Split data into partitions
train_block_1_10 %>%
  filter(expert_label != 0) %>%
  summarise(acc = mean(expert_label == -1))

val_block_1_10%>%
  filter(expert_label != 0) %>%
  summarise(acc = mean(expert_label == -1))

#part c------------------------------------------------------------------
#k features by importance using the caret r packageR
# ensure results are repeatable
set.seed(1)
#block
x_train_block <- train_block_2_3%>%
  filter(expert_label != 0)%>%
  dplyr::select(NDAI:Rad_AN)%>%
  mutate("log(SD)" = log(SD))


y_train_block <-train_block_2_3%>%
  filter(expert_label != 0)%>%
  dplyr::select(expert_label)

#ROC
roc_imp_block <- filterVarImp(x = x_train_block, y = y_train_block$expert_label)
roc_imp_block <- data.frame(cbind(variable = rownames(roc_imp_block), score = roc_imp_block[,1]))
roc_imp_block$score <- as.double(roc_imp_block$score)

roc_imp_block[order(roc_imp_block$score,decreasing = TRUE),]
roc_imp_block$set = "Block"

#horizontal block
x_train_block_h <- train_block_1_10%>%
  filter(expert_label != 0)%>%
  dplyr::select(NDAI:Rad_AN)%>%
  mutate("log(SD)" = log(SD))


y_train_block_h <-train_block_1_10%>%
  filter(expert_label != 0)%>%
  dplyr::select(expert_label)


#ROC
roc_imp_block_h <- filterVarImp(x = x_train_block_h, y = y_train_block_h$expert_label)
roc_imp_block_h <- data.frame(cbind(variable = rownames(roc_imp_block_h), score = roc_imp_block_h[,1]))
roc_imp_block_h$score <- as.double(roc_imp_block_h$score)

roc_imp_block_h[order(roc_imp_block_h$score,decreasing = TRUE),]
roc_imp_block_h$set = "Horizontal Block"

df_roc_imp = bind_rows(roc_imp_block,roc_imp_block_h,)


varimp = ggplot(df_roc_imp, aes(x=reorder(variable, score), y=score,fill=set)) +
#  geom_point() +
  geom_bar(stat = 'identity', position = position_dodge(0.5),width = 0.5)+
  scale_fill_manual(values=c('black','gray'))+
  #geom_segment(aes(x=variable,xend=variable,y=0,yend=score,color = set,alpha = 0.7,linewidth = 5)) +
  ylab("Variable Importance") +
  xlab("Variable Name") +
  coord_flip()+  theme_bw()
ggsave(
  "graphs/varimp_image.png",
  varimp,
  width = 15,
  height = 12,
  units = "cm"
)

train = train%>%
  filter(expert_label != 0)%>%
  write_rds("cache/02_train.rds")
train_block_2_3 %>%
  filter(expert_label != 0) %>%
  write_rds("cache/02_train_block_2_3.rds")
val_block_2_3 %>%
  filter(expert_label != 0) %>%
  write_rds("cache/02_val_block_2_3.rds")
train_block_1_10 %>%
  filter(expert_label != 0) %>%
  write_rds("cache/02_train_block_1_10.rds")
val_block_1_10 %>%
  filter(expert_label != 0) %>%
  write_rds("cache/02_val_block_1_10.rds")
test_block %>%
  filter(expert_label != 0) %>%
  write_rds("cache/02_test.rds")
