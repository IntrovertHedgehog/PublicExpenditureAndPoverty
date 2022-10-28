library(e1071)
library(MASS)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tibble)
library(conflicted)
library(knitr)

# everytime `select` is called, it will prefer dplyr function instead of MASS
conflict_prefer(name = "select", winner = "dplyr")
conflict_prefer(name = "filter", winner = "dplyr")

df.povert <- tibble(read.csv('data/mpi.14.19.csv'))

getYear <- function(yr) {
  df.povert %>% 
    filter(year == yr)
}

mpiDiff <- function(y1, y2) {
  df.y1 <- getYear(y1)
  df.y2 <- getYear(y2)
  
  fin.df <- df.y1 %>%
    inner_join(df.y2, by = c("country.code", "economy")) %>% 
    mutate(mpi.diff = dep.mpi.y - dep.mpi.x,
           distance.mpi = year.y - year.x,
           mpi.diff.percent = mpi.diff/dep.mpi.x*100,
           edu.att.diff = edu.attainment.y - edu.attainment.x,
           edu.enr.diff = edu.enrollment.y - edu.enrollment.x,
           mon.diff = dep.monetary.y - dep.monetary.x) %>% 
    select(economy, country.code, year = year.x, distance.mpi, mpi.diff, edu.att.diff, edu.enr.diff, mpi.diff.percent, mon.diff)
  
  fin.df
}

mpiDiff2 <- function(distance) {
  y.min = 2012
  y.max = 2022 - distance
  
  if (y.min <= y.max) {
    list <- lapply(y.min:y.max, \(start.year){
      mpiDiff(start.year, start.year + distance)
    })
  }
  
  Reduce(rbind, list)
}

