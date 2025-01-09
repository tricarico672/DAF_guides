setwd("~/Desktop/UniTrento/Tutorato/Software R (English)/DAF/homeworks/Homework4")

retail <- read_excel("retail.xlsx")

library(fpp3)

library(readxl)
retail <- read_excel("retail.xlsx", col_types = c("date", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text"))

df_retail <- retail %>%
  select(...1, "Turnover ;  New South Wales ;  Supermarket and grocery stores ;") %>%
  mutate(month = yearmonth(...1)) %>%
  select(-...1)

names(df_retail) <- c("series", "month")

df_cleaned <- df_retail[2:nrow(df_retail),]

is_tsibble(df_cleaned)

df_cleaned <- as_tsibble(df_cleaned, index = month)

is_tsibble(df_cleaned)

df_cleaned <- df_cleaned %>%
  mutate(series = as.numeric(series))

## Data Visualization

autoplot(df_cleaned, series)

cv <- stretch_tsibble(df_cleaned, .init = 200, .step = 4)

fit_hwm <- cv %>%
  model(HWM = ETS(series ~ error("M") + trend("A") + season("M")))

fit_hwd <- cv %>%
  model(HWD = ETS(series ~ error("M") + trend("Ad") + season("M")))

fore_hwm <- forecast(fit_hwm, h = 1)
fore_hwd <- forecast(fit_hwd, h = 1)

accuracy(fore_hwm, df_cleaned)$RMSE
accuracy(fore_hwd, df_cleaned)$RMSE

#I would prefer the damped model since it has the lowest RMSE

augment(fit_hwm) %>%
  select(.innov, month) %>%
  features(.innov, ljung_box, lag = 12)

fit_hwd_data <- df_cleaned %>%
  model(HWD = ETS(series ~ error("M") + trend("Ad") + season("M")))
  
gg_tsresiduals(fit_hwd_data)

#no, the residuals do not look like white noise since there is a strong correlation at different time lags, hence we cannot assume the residuals to come from a white noise process
#this was also visible from the Ljung-Box test on the CV

train_data <- filter_index(df_cleaned, "1982 Apr" ~ "2010 Dec")
test_data <- filter_index(df_cleaned, "2011 Jan" ~ "2013 Dec")

fit2 <- train_data %>%
  model(
    HWM = ETS(series ~ error("M") + trend("A") + season("M")),
    seasonal_naive = SNAIVE(series)
  )

fore2 <- forecast(fit2, new_data = test_data)
accuracy(fore2, df_cleaned)
