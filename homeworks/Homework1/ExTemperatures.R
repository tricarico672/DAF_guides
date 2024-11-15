#seq function
seq(0,90,2)

#what is a date in R?
as.integer(dates) #they're just numbers! Specifically, the number of days since 1970-01-01 (Unix Epoch Date)

#since they are numbers we are able to use seq to create a sequence that goes from a start to an end
dates <- seq(as.Date("1981-01-01"), as.Date("2023-12-31"), 1)

years <- year(dates)
unique(years)
month(dates)
days(dates)
yearmonth(dates) 