packages <- c("quantmod", "forecast", "tseries", "ggplot2", "lubridate")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# Download from Yahoo
getSymbols("AAPL", from = "2010-01-01", auto.assign = TRUE)

#dataset
price <- Ad(AAPL)  # ADJ close
price_mon <- to.monthly(prezzi, indexAt = "lastof", OHLC = FALSE)
log_price <- log(price_mon)

# split in test and train set
n <- length(log_price)
h <- 12  # forecast 12 month
train <- log_price[1:(n - h)]
test <- log_price[(n - h + 1):n]

# ARIMA
mod_arima <- auto.arima(train)
summary(mod_arima)
checkresiduals(mod_arima)

# forecast
fc <- forecast(mod_arima, h = h)

# Validation
accuracy(fc, test)

# dataframe for plot
n_test <- length(test)
forecast_values <- exp(fc$mean)
forecast_lower <- exp(fc$lower[,2])  # 95% confidenza
forecast_upper <- exp(fc$upper[,2])
actual_values <- as.numeric(exp(test))

# correct dates
last_train_date <- index(train)[length(train)]
future_dates <- seq(from = last_train_date %m+% months(1), by = "1 month", length.out = n_test)

df_plot <- data.frame(
  Dates = future_dates,
  forecast = forecast_values,
  Lower = forecast_lower,
  Upper = forecast_upper,
  actual = actual_values
)

# plot
library(ggplot2)

ggplot(df_plot, aes(x = Dates)) +
  geom_line(aes(y = forecast), color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = Reale), color = "darkgreen", linetype = "dashed", size = 1.2) +
  labs(title = "Forecast AAPL with ARIMA",
       subtitle = "Blu = Forecast, Dotted = Actual, Area = IC 95%",
       x = "Date", y = "Price ($)") +
  theme_minimal()
