library(fpp3)
library(gridExtra)
library(tidyquant)
library(forecast)
library(GGally)

setwd("homeworks/Homework1/data")
getwd() #to see the directory you're currently in

tute1 <- readr::read_csv("tute1.csv")
#tute1 <- readr::read_csv("~/Downloads/tute1.csv")
tute_ts <- as_tsibble(tute1, index = Quarter)
View(tute1)

#turning tute1 into a tsibble
mytimeseries <- tute1 |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(index = Quarter)

yearquarter(tute1$Quarter)

mutate(tute1, Quarter = yearquarter(Quarter))

pivot_longer(mytimeseries, -Quarter)

mytimeseries |>
  pivot_longer(-Quarter) |>
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")

pl1 <- autoplot(mytimeseries, .vars = Sales) +
  labs(x = "Quarter")

pl2 <- autoplot(mytimeseries, .vars = AdBudget) +
  labs(x = "Quarter")

pl3 <- autoplot(mytimeseries, .vars = GDP) +
  labs(x = "Quarter")

grid.arrange(pl1,pl2,pl3,nrow=3)

#or

mytimeseries |>
  pivot_longer(-Quarter) |>
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_point() +
  #geom_smooth(method = "lm") + #add regression line, if interested
  facet_grid(name ~ ., scales = "free_y")

GGally::ggpairs(tute_ts, columns = 2:4)

long_ts <- mytimeseries |>
  pivot_longer(-Quarter)

p0 <- ggplot(mytimeseries, aes(x = Sales, y = AdBudget)) +
  geom_point()
p1 <- ggplot(mytimeseries, aes(x = Sales, y = GDP)) +
  geom_point()
p2 <- ggplot(mytimeseries, aes(x = GDP, y = AdBudget)) +
  geom_point()

library(gridExtra)

grid.arrange(p0,p1,p2, nrow = 3)
