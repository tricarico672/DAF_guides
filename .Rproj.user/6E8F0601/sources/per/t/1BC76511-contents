# you can look up https://r4ds.hadley.nz/
# for more information about R for data science

rm(list=ls())
# install fpp3 if not already installed; also install tidyquant, readr
# and ISLR2, which will be necessary later on
library(fpp3)
library(ggplot2)
Sys.setlocale(locale = "English") # needed if your local language is not English

library(ISLR2)

ggplot(Carseats, aes(x = Price, y = Sales))

typeof(Carseats)

ggplot(Carseats, aes(x = Price)) +
  geom_histogram()

# better to change the format to tibble?

Carseats_tib <- as_tibble(Carseats)

typeof(Carseats_tib)

ggplot(Carseats_tib, aes(x = Price, y = Sales)) +
  geom_point()

ggplot(Carseats_tib, aes(x = Price, y = Sales, color = Population)) +
  geom_point()

# two regression lines

ggplot(Carseats_tib, aes(x = Price, y = Sales, color = US)) +
  geom_point() + 
  geom_smooth(method = "lm")

# one regression line

ggplot(Carseats_tib, aes(x = Price, y = Sales)) +
  geom_point(aes(color = US)) + 
  geom_smooth(method = "lm")

# Use different symbols...

ggplot(Carseats_tib, aes(x = Price, y = Sales)) +
  geom_point(aes(color = US, shape = US)) + geom_smooth(method = "lm")

# ... and add titles and axes labels

ggplot(Carseats_tib, aes(x = Price, y = Sales)) +
  geom_point(aes(color = US, shape = US)) + 
  geom_smooth(method = "lm") +
  labs(title = "Sales vs. Price of Carseats",
    subtitle = "US and non-US stores",
    x = "Price ($)", y = "Sales (thousands)",
    color = "US", shape = "US")

#################################################################
# plotting the normal density
#################################################################

mu <- 0 # set value of mu
sigmaLow <- 1 # set value of standard deviation
x0 <- 1 # point where the density is evaluated
y0 <- dnorm(x0,mu,sigmaLow) # compute normal density with parameters mu and sigma
x <- seq(-6,6,by=0.01) # now evaluate the density at a sequence of points
y <- dnorm(x,mu,sigmaLow)
ntib <- tibble(x=x,Low=y) # create a tibble

pdensLow <- ggplot(ntib, aes(x=x,y=Low)) + 
  geom_line() + 
  xlab('x') + 
  ylab('density') # "produce" the plot object...

pdensLow # ...and print it

sigmaHigh <- 1.5 # now increase the value of the standard deviation
y1 <- dnorm(x,mu,sigmaHigh)
ntib$High <- y1 # add a column to the tibble
pdensHigh <- ggplot(ntib, aes(x=x,y=High)) + geom_line() + xlab('x') +
  ylab('density')
print(pdensHigh)

# a graph with two curves

ntibLong = pivot_longer(ntib,c(Low,High),
          names_to = 'VolatilityLevel', values_to = 'Density')

pdens2 <- ggplot(ntibLong, aes(x = x, y = Density, color = VolatilityLevel)) +
  geom_line()

print(pdens2)

# monthly sales for a souvenir shop at a beach resort town in 
# Queensland, Australia, January 1987-December 1993 

souvenir <- scan("rawdata\\souvenir.txt")

# creating a tibble (see also exampleTibble.R)

dates <- seq(as_date("1987-01-01"),as_date("1993-12-01"),
      by="1 month")
z <- tibble('Month' = dates, 'Observation' = souvenir)

# converting to a tsibble

z1 <- mutate(z, Month = yearmonth(Month)) 
souvts <- as_tsibble(z1, index = Month)
autoplot(souvts,Observation) + xlab('Time') + ylab('Sales')

# downloading stock market prices from Yahoo Finance

library(tidyquant)
ENIdata <- tq_get("ENI.MI",get="stock.prices",
                  from="2022-09-15",to="2024-09-15")
ENIdata2 <- tq_get("ENI.MI", get="dividends",from="2022-09-15",to="2024-09-15")

closeP <- select(ENIdata,"date","close")

ENItsib <- as_tsibble(closeP, index = date)

autoplot(ENItsib,close) + xlab('time')

#autoplot(ENIdata,close) + xlab('time') ERROR!!!!

# wide vs long tsibbles
# 1: wide to long

us_change_long <- pivot_longer(us_change,
            c(Consumption, Income), names_to="Series")

# 2: long to wide

arr_wide <- pivot_wider(aus_arrivals, values_from=Arrivals, 
                        names_from=Origin)

# computing and plotting returns

ENIret = mutate(ENItsib,close,ret=log(close)-log(lag(close)))
autoplot(ENIret,ret) + xlab('Time') # time plot

# histogram

hist3 <- ggplot(ENIret, aes(ret)) +
geom_histogram(bins=20,color="black", fill="white") + xlab("ENI")
print(hist3) # histogram

p2 <- ggplot(ENIret, aes(x = ret)) +
      geom_histogram(aes(y=after_stat(density)),bins = 20,
      color="black", fill="white")
p2 <- p2 + stat_function(fun = dnorm, color = "red", n = 1000,
args = list(mean = mean(ENIret$ret,na.rm=TRUE),
            sd = sd(ENIret$ret,na.rm=TRUE)))
print(p2) # normalized histogram and normal density

