#install.packages("TSA")
#install.packages("readr")
#install.packages("tsibbledata")
#install.packages("pacman")
library(tsibbledata)
library(TSA)
library(ggplot2)
library(fpp3)
library(gridExtra)
library(tidyquant)
library(forecast)
library(readr)
library(latex2exp)
library(pacman)
p_load(tsibbledata, TSA, ggplot2, fpp3)

#setwd("Homework2")

# Exercise 1 ---------------------------------------------------------------------
data(hours) #extracts hours dataset from TSA package
hours <- as_tsibble(hours)

#computes autocorrelation along time lags (lag = 1 month)
auto_corr <- ACF(hours, y = value)
#create correlogram
autoplot(auto_corr) #what is possible to see is a significant positive correlation up to lag 5

# Exercise 2 --------------------------------------------------------------

id_number <- 254978 #CHANGE THIS WITH YOUR MATRICULATION NUMBER

set.seed(id_number) #set seed for reproducibility (sample depends on pseudo-random generation and this ensures we get always the same sample)
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

guer <- features(myseries, Turnover, features = guerrero) #use guerrero function to determine a suitable lambda for the box-cox transformation
lambda <- guer$lambda_guerrero #assign the computed lambda to a variable with the same name (lambda)
myseries$Turnover_transformed <- box_cox(myseries$Turnover, lambda = lambda) #add the transformed turnover column to the original dataset and assign the name "Turnover_transformed"

hist(myseries$Turnover_transformed, main = "Transformed Data") #visualize the improvements in normality with this plot...
hist(myseries$Turnover, main = "Original Data") #... compared to this more skewed distribution

qqnorm(y = myseries$Turnover_transformed) #quantile-quantile plot to show the normality of the data (closer to the diagonal is better)
qqnorm(y = myseries$Turnover)

autoplot(myseries, box_cox(Turnover, lambda)) + labs(y = "",
                                                      title =TeX(paste0("Transformed turnover with $\\lambda$ = ",
                                                                        round(lambda,2))))

#it is concluded that a box-cox transformation with lambda = -0.20... improves the fit to normality of the data as shown by the quantile-quantile plot


# Exercise 3 --------------------------------------------------------------
library(fma)
labour <- fma::labour

dates <- seq.Date(from = as.Date("1978-02-01"), to = as.Date("1995-08-01"), by = "month")

labour <- as.data.frame(labour)
labour$date <- dates
names(labour) <- c("labor_force", "month_year")

labour <- labour %>%
  mutate(month_year = yearmonth(month_year))

labour_ts <- as_tsibble(labour, index = month_year)

autoplot(labour_ts, .vars = labor_force) #time plot

## correlogram
autocorr <- ACF(labour_ts, labor_force)
autoplot(autocorr) #huge autocorrelation across months

## seasonality plot
gg_season(labour_ts, labor_force) + 
  labs(y = "Labor Force",
       title = "Seasonal plot: Labor Force in AUS") 

#from the plot we see there is seasonality in the data as every year around December the count tends to increase

gg_subseries(labour_ts, labor_force) +
  labs(y = "Labor Force",
       title = "Subseries plot: Labor Force in AUS")

#the mean (blue bar) in the plots shows how on average there are more people working in the month of december
## further check, computes mean for every month
as_tibble(labour_ts) %>%
  group_by(month(month_year)) %>%
  summarize(mean_dec = mean(labor_force))

hist(labour_ts$labor_force) #the distribution appears to be bimodal, therefore a smoothing through box-cox would make sense
qqnorm(y = labour_ts$labor_force)

lambda <- features(labour_ts, .var = labor_force, features = guerrero)$lambda_guerrero
transformed_labforce <- box_cox(labour_ts$labor_force, lambda)

labour_ts$transformed_labforce <- transformed_labforce

hist(labour_ts$transformed_labforce) #however the distribution remains highly bimodal and normality does not seem to be improved by much

