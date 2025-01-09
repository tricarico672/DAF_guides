library(fpp3)

# ex1 ---------------------------------------------------------------------

data(AirPassengers)

df <- as_tsibble(AirPassengers)

autoplot(df)

fit <- model(df, TSLM(value~trend()))
fore <- forecast(fit, h = 12)

augment(fit) %>%
  autoplot(.fitted)

autoplot(df, value) + 
  autolayer(fore)

#a) the method is not appropriate as we are not taking into account the seasonality that is present in the time series. A better model would probably be the Seasonal Naive

fit2 <- model(df, snaive = SNAIVE(value))
fore2 <- forecast(fit2, h = 12)

autoplot(df, value) + 
  autolayer(fore2, level = NULL)

autoplot(df, value) +
  autolayer(augment(fit2), .fitted, color = "blue") +
  labs(title = "Actual vs fitted values",
       subtitle = "Fitted values in blue") +
  theme(plot.subtitle = element_text(color = "blue", hjust = .5),
        plot.title = element_text(hjust = .5, size = 20))

#b)
report(fit)
#we see that the estimated trend is pointing upward (positive coefficient estimate) and we also see that the trend is a significant predictor of the number of monthly passengers
#however through the 5-number summary of the residuals we see that those do not seem to be close to have a mean of 0 as the median is -5.757. This violates one of the assumption in the linear model


# ex2 ---------------------------------------------------------------------



# ex3 ---------------------------------------------------------------------

#The seasonal component in the additive classical decomposition is computed following these steps:
#1. given a seasonal period (m = 4 for quarterly data, m = 12 for monthly data, m = 7 for daily data etc.) decide whether you need to estimate the trend using a 2 x m-MA (when m is even) or a simple m-MA .
#2. once this trend has been estimated, we can now compute the detrended series. For each observation, you subtract the estimated trend (computed in step 1) from the same period.
#3. now we can simply average the observation belonging to the same season with those belonging to the same season but in different years. This estimates the seasonality for each month and the estimates are repeated for each year.
#4. Once we compute the seasonal component, we can also get the seasonally adjusted time series simply by subtracting the estimated seasonal component to the original time series

#Below you can see an applied example of the theoretical steps outlined above
dcmp <- components(model(df, classical_decomposition(formula = value, type = "additive"))) #example from ex1

filtered_dcmp <- dcmp %>%
  filter(year(index) < 1950) %>%
  summarise(sum(seasonal))

round(sum(filtered_dcmp$`sum(seasonal)`)) #practically zero, shows how the estimated effect in 12 months should never form bias in the decomposition, therefore the sum of all the estimated seasonal components in a year (12 months) sums up to 0!


# ex4 ---------------------------------------------------------------------
library(slider)

setwd("homeworks/Mock Exam")

series4 <- read.table("series4.csv", quote="\"", comment.char="")

names(series4) <- "series"

#we see that we are missing a time index here, so we need to create one before we convert it into a tsibble
date <- seq.Date(from = as.Date("1981-01-01"), to = as.Date("2020-01-01"), by = "year")

series4_cleaned <- series4 |>
  mutate(date = year(date))

mutate(series4, date = year(date))

#convert into a tsibble
series4_ts <- as_tsibble(series4_cleaned, index = date)

#use autoplot
autoplot(series4_ts, series)
#use slider::slide_dbl() to compute the 5-MA to estimate the trend

series4_ts2 <- series4_ts %>%
  mutate(`5-MA` = slider::slide_dbl(series, mean, .before = 2, .after = 2, .complete = T))

autoplot(series4_ts2, series) +
  geom_line(aes(y = `5-MA`), color = "orange")

