install.packages("quantmod")
library(quantmod)
library(tseries)
library(forecast)
library(ggplot2)

getSymbols("AAPL", src = "yahoo", from = "2020-01-01", to = Sys.Date())
chartSeries(AAPL) 

price_adj <- Ad(AAPL)
plot(price_adj)

ret_log <- diff(log(price_adj))[-1]
plot(ret_log, main = "Log return AAPL")

adf.test(na.omit(ret_log))

log_price <- log(price_adj)
mod_arima <- auto.arima(log_price)
summary(mod_arima)

fc <- forecast(mod_arima, h = 30)
plot(fc)

price_forecast <- exp(fc$mean)
plot(price_forecast, main = "Forecast AAPL")

checkresiduals(mod_arima)

#plot
ultima_data <- index(prezzi_adj)[NROW(prezzi_adj)]
dates_forecast <- seq(from = ultima_data + 1, by = "days", length.out = 30)
dates_forecast <- dates_forecast[weekdays(dates_forecast) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]
dates_forecast <- dates_forecast[1:length(fc$mean)]

# dataframe for plot
df_fc <- data.frame(
  Dates = dates_forecast,
  Forecast = as.numeric(exp(fc$mean)),
  Lower = as.numeric(exp(fc$lower[,2])),  # CI 95%
  Upper = as.numeric(exp(fc$upper[,2]))
)

# Grafico
ggplot(df_fc, aes(x = Dates, y = Forecast)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "lightblue", alpha = 0.4) +
  labs(title = "Forecast AAPL ", x = "Date", y = "Predicted price")
