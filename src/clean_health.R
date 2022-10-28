healthNYear <- function(y1, y2) {
  df.expenditure %>% 
    filter(year >= y1 & year <= y2) %>%
    filter(!is.na(health.expenditure)) %>% 
    group_by(economy, country.code) %>% 
    summarise(mean.health.expenditure = mean(health.expenditure, rm.na = TRUE)) %>% 
    mutate(year = y1, distance = y2 - y1)
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