library(dplyr)
library(tidyverse)
library(tidyr)
library(tibble)

options(dplyr.summarise.inform = FALSE)
# edu

eduNYear <- function(y1, y2) {
  df.expenditure %>% 
    filter(year >= y1 & year <= y2) %>%
    filter(!is.na(edu.expenditure)) %>% 
    group_by(economy, country.code) %>% 
    summarise(mean.edu.expenditure = mean(edu.expenditure, rm.na = TRUE), mean.gdp.per.capita = mean(gdp.per.capita, rm.na = T)) %>% 
    mutate(year = y1, distance = y2 - y1)
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

