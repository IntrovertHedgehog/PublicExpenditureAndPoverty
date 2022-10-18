gdp <- read.csv('data/gdp.csv')

gdp <- as.tibble(gdp) %>% 
  dplyr::select(economy, country.code, gdp.2016 = X2012, gdp.2017 = X2013, gdp.2018 = X2014)

gdp <- gdp %>% mutate(no.avail = 3 - rowSums(is.na(.)), mean.gdp = (gdp.2016 %+% gdp.2017 %+% gdp.2018) / no.avail) %>% 
  filter(!is.nan(mean.gdp)) %>% 
  dplyr::select(economy, country.code, mean.gdp)

