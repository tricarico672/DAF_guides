library(fpp3)
library(tsapp)

df <- as_tsibble(SALES)

autoplot(df)
gg_subseries(df)
gg_season(df)
autoplot(ACF(df, value))


# Ex2 ---------------------------------------------------------------------
library(tswge)
df <- data.frame(freight)
dates <- seq.Date(from = as.Date("2010-01-01"), to = as.Date("2019-12-01"), by = "month")

df_ts <- df %>%
  mutate(date = yearmonth(dates)) %>%
  as_tsibble(index = date)

autoplot(df_ts)

fit <- df_ts %>%
  model(
    "additive" = ETS(freight ~ error("A") + trend("A") + season("A")),
    "multiplicative" = ETS(freight ~ error("M") + trend("A") + season("M"))
  )

forecast(fit, h = 1)

cv <- stretch_tsibble(df_ts, .step = 1, .init = 50)

cv %>%
  model(
    "additive" = ETS(freight ~ error("A") + trend("A") + season("A")),
    "multiplicative" = ETS(freight ~ error("M") + trend("A") + season("M"))
  ) %>%
  forecast(h = 1) %>%
  accuracy(df_ts) %>%
  dplyr::select(.model, RMSE)


