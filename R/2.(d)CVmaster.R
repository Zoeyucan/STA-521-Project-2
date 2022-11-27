library(tidyverse)
library(tidymodels)
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

split_data = function(data,cols = 2,rows = 3) {
  block = find_blocks(data$x_coordinate,data$y_coordinate,cols ,rows )
  final_data = tibble()
  for (i in seq_len(nrow(block))) {
    coords = block[i,]
    data %>%
      filter(x_coordinate >= coords$left, x_coordinate <= coords$right,
             y_coordinate >= coords$top, y_coordinate <= coords$bottom) -> filtered_data
    filtered_data$block = i
    final_data = rbind(final_data, filtered_data)

  }
  return(final_data)
}

train <- read_rds("cache/2_train_block.rds")

table(split_data(train,2,3)$block)

##
make_cvsplits <- function(.dt, .method = "block", .columns = 2, .rows = 3, .k) {
  cv_att <- list(v = 0, strata = FALSE, repeats = 1)

  if (.method == "block") {
    out <- vector(mode = "list", length = .columns * .rows)
    names <- vector(mode = "character", length = .columns * .rows)
    blocks <- split_data(.dt, .columns, .rows)

    for (i in 1:(.columns * .rows)) {
      val_set <- which(blocks == i)
      train_set <- (1:NROW(.dt))[-val_set]
      out[[i]] <- rsample::make_splits(
        list(analysis = train_set, assessment = val_set),
        data = .dt
      )
      names[[i]] <- paste0("Fold", i)
    }
    cv_att$v <- .columns * .rows
  }

  rset_obj <- new_rset(
    splits = out,
    ids = names,
    subclass = c("vfold_cv", "rset"),
    attrib = cv_att
  )

  return(rset_obj)
}
b = make_cvsplits(train, .method = "block", .columns = 2, .rows = 3, .k = 5)

train = train%>%
  mutate(across(expert_label, factor))

dt_rec <- recipe(expert_label ~ ., data = train) %>%
  step_rm(x_coordinate, y_coordinate, class) %>%
  step_scale(all_predictors())

logreg_mod <- logistic_reg(mode = "classification", engine = "glm")

CVmaster <- function(.train, .classifier, .recipe, .method, .columns,
                     .rows, .k, .metrics = metric_set(accuracy)) {
  fit <- workflow() %>%
    add_recipe(.recipe) %>%
    add_model(.classifier) %>%
    fit_resamples(
      make_cvsplits(.train, .method, .columns, .rows, .k),
      metrics = .metrics
    )
  return(select(bind_rows(fit$.metrics), -.config))
}
##test
CVmaster(
  train,
  "rf_mod",
  dt_rec,
  .method = "block",
  .columns = 3,
  .rows = 3,
  .k = 10,
  .metrics = metric_set(accuracy, mn_log_loss, kap, yardstick::spec, sens)
)
