library(tidyverse)

#part a--------------------------------------------------------------

# splitting testing and training data
image = read_rds("cache/image.rds")
#make this example reproducible
set.seed(1)
image$id <- 1:nrow(image)
#Use 70% of dataset as training set and remaining 30% as testing set
train <- image %>% dplyr::sample_frac(0.7)
test_18  <- dplyr::anti_join(image, train, by = 'id')

#use block cross validation
y_length = (max(image$y_coordinate)-min(image$y_coordinate)+1)/5
x_length = (max(image$x_coordinate)-min(image$x_coordinate)+1)/2

#find the coordinate of each block
find_blocks <- function(x, y, cols = 2,rows = 3) {
  x_min <- min(x)
  x_max <- max(x)
  x_step <- (x_max-x_min)/cols

  y_min <- min(y)
  y_max <- max(y)
  y_step <- (y_max-y_min)/rows

  block <- tibble()

  for (i in seq_len(rows)) {
    # make top and bottom
    top <- floor(y_min + (i-1) * y_step)
    bot <- floor(y_max - (rows-i) * y_step )
    if (i != rows)
      bot <- bot - 1

    for (j in seq_len(cols)){
      # make left and right
      left <- floor(x_min + (j-1) * x_step)
      right <- floor(x_max - (cols-j) * x_step )
      if (j != cols)
        right <- right - 1

      block %>%
        bind_rows(data.frame(top, bot, left, right)) -> block
    }
  }
  names(block) <- c("top", "bottom", "left", "right")
  return(block)
}



split_data = function(data, block,cols = 2,rows = 3) {
  final_data <- tibble()
  for (i in seq_len(cols*rows)) {
    coords <- block[i,]
    data %>%
      filter(x_coordinate >= coords$left, x_coordinate <= coords$right,
             y_coordinate >= coords$top, y_coordinate <= coords$bottom) -> filtered_data
    filtered_data$block = i
    final_data = rbind(final_data, filtered_data)
  }

  return(final_data)
}
## column = 2, row = 3
block = find_blocks(image$x_coordinate,image$y_coordinate)
splited_data_18 =split_data(train,block)
splited_data_18 = splited_data_18%>% filter(expert_label == 1|expert_label == -1)

#part b---------------------------------
test_18 %>%
  filter(expert_label != 0) %>%
  summarise(acc = mean(expert_label == -1))

splited_data_18%>%
  filter(expert_label != 0) %>%
  summarise(acc = mean(expert_label == -1))


library(splitTools)


# Split data into partitions
set.seed(3451)
inds <- partition(image$expert_label, p = c(train = 0.6, valid = 0.2, test = 0.2))
str(inds)

#stratified data spliting
train <- image[inds$train, ]
valid <- image[inds$valid, ]
test <- image[inds$test, ]
test %>%
  filter(expert_label != 0) %>%
  summarise(acc = mean(expert_label == -1))
valid %>%
  filter(expert_label != 0) %>%
  summarise(acc = mean(expert_label == -1))
