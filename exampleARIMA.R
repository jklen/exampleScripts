
library(forecast)

df <- data.table::fread('1varTimeSeries.csv')
print(class(df$Date))

df$Date <- as.Date(df$Date)

fit <- auto.arima(df$Value)
forecast <- forecast(fit, h = 20) # h - number of periods to forecast

forecastDF <- data.frame(Date = 1:nrow(as.data.frame(forecast)),
                         Actual = 'N')
forecastDF <- cbind(forecastDF, as.data.frame(forecast))
forecastDF$Date <- as.Date(forecastDF$Date, origin = max(df$Date))

df$Actual <- 'Y'
names(forecastDF)[3] <- 'Value'
forecastDF <- forecastDF[, c('Date', 'Value', 'Actual', 'Lo 80', 'Hi 80', 'Lo 95', 'Hi 95')]

outputDF <- merge(df, forecastDF, all = T)
outputDF