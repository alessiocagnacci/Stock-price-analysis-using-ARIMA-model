# --- STEP 0: Pacchetti necessari ---
packages <- c("quantmod", "forecast", "tseries", "ggplot2", "lubridate")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# --- STEP 1: Scarica dati storici da Yahoo Finance ---
getSymbols("AAPL", from = "2010-01-01", auto.assign = TRUE)
prezzi <- Ad(AAPL)  # Chiusura aggiustata

# --- STEP 2: Prepara la serie logaritmica mensile ---
prezzi_mens <- to.monthly(prezzi, indexAt = "lastof", OHLC = FALSE)
log_prezzi <- log(prezzi_mens)

# --- STEP 3: Dividi in training e test set ---
n <- length(log_prezzi)
h <- 12  # Previsione a 12 mesi
train <- log_prezzi[1:(n - h)]
test <- log_prezzi[(n - h + 1):n]

# --- STEP 4: Costruisci modello ARIMA ---
mod_arima <- auto.arima(train)
summary(mod_arima)
checkresiduals(mod_arima)

# --- STEP 5: Previsione ---
fc <- forecast(mod_arima, h = h)

# --- STEP 6: Valutazione delle performance ---
accuracy(fc, test)

# STEP 7: Crea dataframe per il grafico
n_test <- length(test)
forecast_values <- exp(fc$mean)
forecast_lower <- exp(fc$lower[,2])  # 95% confidenza
forecast_upper <- exp(fc$upper[,2])
actual_values <- as.numeric(exp(test))

# Genera le date corrette
last_train_date <- index(train)[length(train)]
future_dates <- seq(from = last_train_date %m+% months(1), by = "1 month", length.out = n_test)

# Costruisci dataframe completo
df_plot <- data.frame(
  Data = future_dates,
  Previsione = forecast_values,
  Lower = forecast_lower,
  Upper = forecast_upper,
  Reale = actual_values
)

# STEP 8: Grafico
library(ggplot2)

ggplot(df_plot, aes(x = Data)) +
  geom_line(aes(y = Previsione), color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = Reale), color = "darkgreen", linetype = "dashed", size = 1.2) +
  labs(title = "Previsione AAPL con ARIMA (scala originale)",
       subtitle = "Linea blu = previsione, tratteggiata = valore reale, area = IC 95%",
       x = "Data", y = "Prezzo ($)") +
  theme_minimal()