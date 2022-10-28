# gdp
gdp <- read.csv('data/gdppc.csv')
gdp <- gdp %>% 
  select(economy, country.code, X2007:X2021) %>% 
  pivot_longer(cols = X2007:X2021, names_to = 'year', values_to = 'gdp.per.capita') %>% 
  mutate(year = as.numeric(substring(year, 2)))

# edu
edu.expense <- read.csv('data/education.csv')
edu.expense <- edu.expense %>% 
  select(economy, country.code, X2007:X2021) %>% 
  pivot_longer(cols = X2007:X2021, names_to = 'year', values_to = 'edu.percent') %>% 
  mutate(year = as.numeric(substring(year, 2)))

# healthcare
health.expense <- read.csv('data/healthcare.csv')
health.expense <- health.expense %>% 
  select(economy, country.code, X2007:X2021) %>% 
  pivot_longer(cols = X2007:X2021, names_to = 'year', values_to = 'health.percent') %>% 
  mutate(year = as.numeric(substring(year, 2)))

# military
mili.expense <- read.csv('data/military.csv')
mili.expense <- mili.expense %>% 
  select(economy, country.code, X2007:X2021) %>% 
  pivot_longer(cols = X2007:X2021, names_to = 'year', values_to = 'mili.percent') %>% 
  mutate(year = as.numeric(substring(year, 2)))

# join data
df.expenditure <- gdp %>% 
  inner_join(edu.expense, by = c('economy', 'country.code', 'year')) %>%
  inner_join(health.expense, by = c('economy', 'country.code', 'year')) %>%
  inner_join(mili.expense, by = c('economy', 'country.code', 'year')) %>%
  mutate(edu.expenditure = edu.percent * gdp.per.capita / 100,
         health.expenditure = health.percent * gdp.per.capita / 100, 
         mili.expenditure = mili.percent * gdp.per.capita / 100) 


# functions
eduNYear <- function(y1, y2) {
  df.expenditure %>% 
    filter(year >= y1 & year <= y2) %>%
    filter(!is.na(edu.expenditure)) %>% 
    group_by(economy, country.code) %>% 
    summarise(mean.edu.expenditure = mean(edu.expenditure, rm.na = TRUE), mean.gdp.per.capita = mean(gdp.per.capita, rm.na = T)) %>% 
    mutate(year = y1, distance.edu = y2 - y1)
}

eduNYear2 <- function(noYear) {
  y.min = min(df.expenditure$year)
  y.max = max(df.expenditure$year) - noYear
  
  if (y.min < y.max){
    list <- lapply(y.min:y.max, \(start.year) {
      eduNYear(start.year, start.year + noYear)
    })
  }
  
  Reduce(rbind, list)
}

# health


healthNYear <- function(y1, y2) {
  df.expenditure %>% 
    filter(year >= y1 & year <= y2) %>%
    filter(!is.na(health.expenditure)) %>% 
    group_by(economy, country.code) %>% 
    summarise(mean.health.expenditure = mean(health.expenditure, rm.na = TRUE)) %>% 
    mutate(year = y1, distance.health = y2 - y1)
}

healthNYear2 <- function(noYear) {
  y.min = min(df.expenditure$year)
  y.max = max(df.expenditure$year) - noYear
  
  if (y.min < y.max){
    list <- lapply(y.min:y.max, \(start.year) {
      healthNYear(start.year, start.year + noYear)
    })
  }
  
  Reduce(rbind, list)
}

#mili
miliNYear <- function(y1, y2) {
  df.expenditure %>% 
    filter(year >= y1 & year <= y2) %>%
    filter(!is.na(mili.expenditure)) %>% 
    group_by(economy, country.code) %>% 
    summarise(mean.mili.expenditure = mean(mili.expenditure, rm.na = TRUE)) %>% 
    mutate(year = y1, distance.mili = y2 - y1)
}

miliNYear2 <- function(noYear) {
  y.min = min(df.expenditure$year)
  y.max = max(df.expenditure$year) - noYear
  
  if (y.min < y.max){
    list <- lapply(y.min:y.max, \(start.year) {
      miliNYear(start.year, start.year + noYear)
    })
  }
  
  Reduce(rbind, list)
}