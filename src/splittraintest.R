set.seed(1984)
# split data
seeds <- countries1 %>% 
  group_by(country.code) %>% 
  filter(row_number() == 1)

plants <- countries1 %>% 
  group_by(country.code) %>% 
  filter(row_number() != 1)

isComplete <- which(complete.cases(plants))
idx <- sample(isComplete, replace = F, 0.2 * nrow(plants))

training <- plants[-idx,] %>% 
  rbind(seeds)
testing <- plants[idx,]

set.seed(1984)
split_train_test <- function(data) {
  # split data
  seeds <- data %>% 
    group_by(country.code) %>% 
    filter(row_number() == 1)
  
  plants <- data %>% 
    group_by(country.code) %>% 
    filter(row_number() != 1)
  
  isComplete <- which(complete.cases(plants))
  idx <- sample(isComplete, replace = F, 0.2 * nrow(plants))
  
  training <- plants[-idx,] %>% 
    rbind(seeds)
  testing <- plants[idx,]
}


ntrain <- nrow(training)
combine <- rbind(training, testing)
combine.mtrx <- model.matrix(pov ~ ., data = combine)
train.mtrx <- combine.mtrx[1:100,]
test.mtrx <- combine.mtrx[-ntrain,]

train.x <- train.mtrx[,-1]
train.y <- training$pov

test.x <- test.mtrx[,-1]
text.y <- test$pov