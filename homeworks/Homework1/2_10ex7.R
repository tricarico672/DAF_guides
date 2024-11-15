set.seed(12345678)
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

autoplot(myseries,.vars = Turnover)
gg_season(myseries, y = Turnover)
gg_subseries(myseries, y = Turnover)
gg_lag(myseries, y = Turnover, lags = c(3,6,9,12)) #at lag 12 it is possible to see how close December is to the perfect 
#correlation line (gray & dashed) but also all the other months
?gg_lag
ACF(myseries,y = Turnover) |> autoplot() #extending above dashed lines implies a statistically significant correlation
#this also allows to see the seasonality in the data as we see spikes every 12 (months)
?ACF
