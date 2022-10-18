library(dplyr)
library(tidy)
library(tibble)

#custom plus operator to ignore NAs
`%+%` <- function(x, y) {
  mapply(sum, x, y, MoreArgs = list(na.rm = TRUE))
}

non.NA <- function(...)

#import + clean data
edu.expense <- read.csv('data/edu.percent.gdp.csv')
edu.expense <- as_tibble(edu.expense) %>% 
  dplyr::select(economy, country.code, edu.2016 = X2012, edu.2017 = X2013, edu.2018 = X2014)

edu.expense <- edu.expense %>% mutate(no.avail = 3 - rowSums(is.na(.)), mean.edu = (edu.2016 %+% edu.2017 %+% edu.2018) / no.avail) %>% 
  filter(!is.nan(mean.edu)) %>% 
  dplyr::select(economy, country.code, mean.edu)

# MPI with edu col
dat <- fin.df %>%
  dplyr::select(country.code, dep.mpi.diff, dep.mpi.diff.trans) %>% 
  inner_join(edu.expense %>% 
               dplyr::select(country.code, mean.edu), by = 'country.code') %>% 
  inner_join(gdp %>% 
               dplyr::select(country.code, mean.gdp), by = 'country.code')

# rm.usa <- dat[-13,]
# below.5e10 <- dat %>% 
#  filter(mean.gdp < 1e12)

below.5e10 <- dat
lrm <- lm(dep.mpi.diff.trans ~ log(I(mean.edu * mean.gdp)), data = below.5e10)
summary(lrm)
plot(log(below.5e10$mean.gdp * below.5e10$mean.edu), below.5e10$dep.mpi.diff.trans)
abline(lrm)
plot(lrm, which=1)
hist(resid(lrm))
