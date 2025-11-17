install.packages("quantmod")
library(quantmod)
library(tseries)
library(forecast)
library(ggplot2)

getSymbols("AAPL", src = "yahoo", from = "2020-01-01", to = Sys.Date())
chartSeries(AAPL) 

prezzi_adj <- Ad(AAPL)
plot(prezzi_adj)

rend_log <- diff(log(prezzi_adj))[-1]
plot(rend_log, main = "Rendimenti logaritmici AAPL")

adf.test(na.omit(rend_log))

log_prezzi <- log(prezzi_adj)
mod_arima <- auto.arima(log_prezzi)
summary(mod_arima)

fc <- forecast(mod_arima, h = 30)
plot(fc)

prezzi_forecast <- exp(fc$mean)
plot(prezzi_forecast, main = "Previsione AAPL (scala originale)")

checkresiduals(mod_arima)

#per grafico
ultima_data <- index(prezzi_adj)[NROW(prezzi_adj)]
date_forecast <- seq(from = ultima_data + 1, by = "days", length.out = 30)

# Rimuove sabato e domenica (Yahoo fornisce dati borsistici, no weekend)
date_forecast <- date_forecast[weekdays(date_forecast) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]

# Adatta a lunghezza forecast
date_forecast <- date_forecast[1:length(fc$mean)]

# Costruisci il dataframe per il plot
df_fc <- data.frame(
  Data = date_forecast,
  Previsto = as.numeric(exp(fc$mean)),
  Lower = as.numeric(exp(fc$lower[,2])),  # intervallo 95%
  Upper = as.numeric(exp(fc$upper[,2]))
)

# Grafico
ggplot(df_fc, aes(x = Data, y = Previsto)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "lightblue", alpha = 0.4) +
  labs(title = "Previsione AAPL (scala originale)", x = "Data", y = "Prezzo previsto")
