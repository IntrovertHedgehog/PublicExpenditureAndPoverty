library(corrplot)
library(PerformanceAnalytics)
library(Hmisc)
library(ggplot2)

# Import and combine
setwd("data")

poverty.headcount <- importWDI("poverty.headcount.215dollar.csv", "pov")
mpi <- importWDI("mpi.csv", "mpi")
education.expenditure.total <- importWDI("total.education.expenditure.csv", "edu.total")
education.expenditure.primary <- importWDI("primary.education.expenditure.csv", "edu.pri")
education.expenditure.secondary <- importWDI("secondary.education.expenditure.csv", "edu.sec")
education.expenditure.tertiary <- importWDI("tertiary.education.expenditure.csv", "edu.ter")
health.expenditure <- importWDI("health.expenditure.csv", "hlth")
military.expenditure <- importWDI("military.expenditure.csv", "mil")
fdi <- importWDI("fdi.csv", "fdi")
labour.force.participation <- importWDI("labour.force.participation.csv", "lbr.part")
unemployment.rate <- importWDI("unemployment.csv", "unemp")
population.growth <- importWDI("population.growth.csv", "pop.gwth.total")
rural.population.growth <- importWDI("rural.population.growth.csv", "pop.gwth.rural")
urban.population.growth <- importWDI("urban.population.growth.csv", "pop.gwth.urban")
gdp.deflator <- importWDI("gdp.deflator.csv", "gdp.dflt")
gender.equality <- importWDI("gender.equality.csv", "gdr.eql")
gross.capital.formation <- importWDI("gross.capital.formation.csv", "gcf")
trade <- importWDI("trade.csv", "trade")
region.class <- importRegionClass("region.class.csv")
income.class <- importIncomeClass("income.class.csv")

setwd("..")

countries <- income.class %>% 
  full_join(region.class, by = "country.name") %>% 
  right_join(poverty.headcount, by = c("country.name", "country.code", "year")) %>%
  full_join(mpi, by = c("country.name", "country.code", "year")) %>%
  left_join(education.expenditure.total, by = c("country.name", "country.code", "year")) %>%
  left_join(education.expenditure.primary, by = c("country.name", "country.code", "year")) %>%
  left_join(education.expenditure.secondary, by = c("country.name", "country.code", "year")) %>%
  left_join(education.expenditure.tertiary, by = c("country.name", "country.code", "year")) %>%
  left_join(health.expenditure, by = c("country.name", "country.code", "year")) %>%
  left_join(military.expenditure, by = c("country.name", "country.code", "year")) %>%
  left_join(fdi, by = c("country.name", "country.code", "year")) %>%
  left_join(labour.force.participation, by = c("country.name", "country.code", "year")) %>%
  left_join(unemployment.rate, by = c("country.name", "country.code", "year")) %>%
  left_join(population.growth, by = c("country.name", "country.code", "year")) %>%
  left_join(rural.population.growth, by = c("country.name", "country.code", "year")) %>%
  left_join(urban.population.growth, by = c("country.name", "country.code", "year")) %>%
  left_join(gdp.deflator, by = c("country.name", "country.code", "year")) %>%
  left_join(gender.equality, by = c("country.name", "country.code", "year")) %>%
  left_join(gross.capital.formation, by = c("country.name", "country.code", "year")) %>%
  left_join(trade, by = c("country.name", "country.code", "year"))

cor_matrix <- cor(countries[, 6:23], use = "complete.obs")
print(cor_matrix, 2)
corrplot(cor_matrix, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
chart.Correlation(countries[,6:23], histogram=TRUE, pch=19)

res2 <- rcorr(as.matrix(countries[,6:23]))
res2
corrplot(res2[[1]], type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)

n <- nrow(countries)
missings <- colMeans(is.na(countries))
df <- data.frame(columns = factor(names(missings)), missing.prcn = missings)
df
ggplot(df, aes(x = columns, y = missings, fill = missings < 0.35)) +
  geom_bar(stat = "identity")

IV <- (df %>% filter(missing.prcn < 0.4))$columns

dataset <- countries %>% select(all_of(IV))
sum(complete.cases(dataset))

lrm <- lm(pov ~ ., dataset)
summary(lrm)
plot(lrm)

