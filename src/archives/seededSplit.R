set.seed(1984)
# split data
seeds <- countries1 %>% 
  group_by(country.code) %>% 
  filter(row_number() == 1)

plants <- countries1 %>% 
  group_by(country.code) %>% 
  filter(row_number() != 1)

# blank for no imputation, cart for variable requiring imputation
meth <- c(rep("", 4), "cart", "", rep("cart", 16))
names(meth) <- colnames(countries1)
meth

isComplete <- which(complete.cases(plants))
idx <- sample(isComplete, replace = F, 0.2 * nrow(plants))

countries.train.4 <- plants[-idx,] %>% 
  rbind(seeds)
countries.test.4 <- plants[idx,]

countries1.imputed <- mice(countries.train.4, m = 3, maxit = 20, method = meth)