library(e1071)
library(MASS)
library(ggplot2)
library(dplyr)

df.povert <- read.csv('data/mpi.14.19.csv')

str(df.povert)

no.avail <- df.povert %>%
  select(country.code, economy, year) %>% 
  group_by(year) %>% 
  summarise(no.avail = n())

hist(df.povert$year, breaks = 2011:2021)

getYear <- function(yr) {
    df.povert %>% 
      select(country.code, economy, year, dep.monetary, dep.mpi) %>% 
      filter(year == yr)
}

seg <- lapply(2012:2021, getYear)
names(seg) <- 2012:2021

noOverlap <- function(s1, s2) {
  s1 %>%
    inner_join(s2, by = "country.code") %>% 
    filter(year.x != year.y) %>% 
    summarise(count = n()) %>% 
    pull()
}

cross <- diag(10)
rownames(cross) <- 2012:2021
colnames(cross) <- 2012:2021

for (i in 1:10) {
  cross[i,] = sapply(seg[1:10], noOverlap, s2 = seg[[i]])
}

cross

max.cr <- max(cross)

df.cross <- as.data.frame.table(cross)

df.cross %>% 
  filter(Freq == max.cr)

fin.df <- seg[["2012"]] %>%
  inner_join(seg[["2018"]], by = "country.code") %>% 
  filter(year.x != year.y) %>% 
  dplyr::select(country.code,
         economy = economy.x,
         dep.mpi.2016 = dep.mpi.x,
         dep.mpi.2018 = dep.mpi.y) %>% 
  mutate(dep.mpi.diff = dep.mpi.2018 - dep.mpi.2016)

fin.df %>% 
  summarise(pos = sum(dep.mpi.diff > 0),
            neg = sum(dep.mpi.diff < 0),
            unchg = sum(dep.mpi.diff == 0))

summary(fin.df)

hist(fin.df$dep.mpi.diff, breaks = 30)

# transform
y <- fin.df$dep.mpi.diff
y <- y - min(y-0.01)

skewness(y)
shapiro.test(y)

b <- boxcox(lm(y~1), lambda = seq(-3, 3, 1/1000))
lambda <- b$x[which.max(b$y)]

newy <- (y ^ lambda - 1) / lambda

skewness(newy)

shapiro.test(newy)

ggplot(mapping = aes(x = newy)) +
  geom_density()

# add transformed columns
fin.df$dep.mpi.diff.trans <- newy
