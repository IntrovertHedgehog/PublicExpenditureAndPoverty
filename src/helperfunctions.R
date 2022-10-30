library(readr)
library(tidyr)
library(dplyr)

importWDI <- function(filepath, value_name) {
  df <- read_csv(filepath, skip = 4) 
  
  colnames(df) <- tolower(gsub(" ", ".", colnames(df)))
  
  df <- df %>% 
    pivot_longer(5:ncol(.), names_to = "year", values_to = "value") %>% 
    filter(!is.null(value) & !is.na(value)) %>% 
    mutate(country.code = factor(country.code), 
           country.name = factor(country.name),
           year = as.numeric(year)) %>% 
    select(country.code, country.name, year, value)
  
  colnames(df)[4] <- value_name
  
  df
}

importRegionClass <- function(filepath) {
  df <- read_csv(filepath, skip = 4) 
  
  colnames(df) <- tolower(gsub(" ", ".", colnames(df)))
  
  df %>% mutate(country.name = factor(country.name),
                region = factor(region)) %>% 
    select(country.name, reg = region)
}

importIncomeClass <- function(filepath) {
  df <- read_csv(filepath, skip = 4) 
  
  colnames(df) <- tolower(gsub(" ", ".", colnames(df)))
  
  df %>% 
    pivot_longer(3:ncol(.), names_to = "year", values_to = "income") %>% 
    filter(!is.null(income) & !is.na(income)) %>% 
    mutate(country.code = factor(country.code), 
           country.name = factor(country.name), 
           year = as.numeric(year), 
           income = factor(income)) %>% 
    select(country.code, country.name, year, income)
}

