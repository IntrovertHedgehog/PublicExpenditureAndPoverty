mpi6 <- mpiDiff2(6)
edu3 <- eduNYear2(1)
health3 <- healthNYear2(1)
mili3 <- miliNYear2(3)

fin.df <- mpi6 %>% 
  inner_join(edu3, by = c('economy', 'country.code', 'year')) %>% 
  inner_join(health3, by = c('economy', 'country.code', 'year')) %>% 
  inner_join(mili3, by = c('economy', 'country.code', 'year'))

lrm <- lm(mpi.diff ~ log(mean.edu.expenditure), data = fin.df)
summary(lrm)

lrm <- lm(mpi.diff ~ sqrt(mean.health.expenditure) + mean.health.expenditure, data = fin.df)
summary(lrm)

lrm <- lm(mpi.diff ~ sqrt(mean.mili.expenditure), data = fin.df)
summary(lrm)

lrm <- lm(mpi.diff ~ mean.edu.expenditure + log(mean.edu.expenditure), data = fin.df)
summary(lrm)

ggplot(fin.df, aes(-0.40588 * log(mean.edu.expenditure)^2 + 6.26488 * log(mean.edu.expenditure), mpi.diff)) +
  geom_point() +
  geom_smooth(method = 'lm')

car::vif(lrm)

plot(lrm, which = 1)

# getRSquared <- function(mpiYear, eduYear, healthYear) {
#   mpi <- mpiDiff2(mpiYear)
#   edu <- eduNYear2(eduYear)
#   health <- healthNYear2(healthYear)
#   
#   fin.df <- mpi %>% 
#     inner_join(edu, by = c('economy', 'country.code', 'year'), suffix = c('.mpi', '.edu')) %>% 
#     inner_join(health3, by = c('economy', 'country.code', 'year'), suffix = c('', '.health'))
#   
#   lrm <- lm(mpi.diff ~ mean.health.expenditure + log(mean.edu.expenditure), data = fin.df)
#   
#   summary(lrm)$adj.r.squared
# }
# 
# assessment <- data.frame(mpi = numeric(), edu = numeric(), health = numeric(), r.sqr = numeric())
# 
# 
# m <- 6
# for (e in 1:4) {
#   for (h in 1:4) {
#     rq <- getRSquared(m, e, h)
#     print(rq)
#     assessment[nrow(assessment)+1, ] = c(m, e, h, rq)
#   }
# }
