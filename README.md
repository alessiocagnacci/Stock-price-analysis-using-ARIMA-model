# Stock price analysis using ARIMA model
*I used ARIMA models to forecast the value of a stock, using historical time series data retrieved from Yahoo Finance.*

During my Master’s degree in Mathematics, I took a time series analysis course with R, so I decided to apply the skills I learned to a field of personal interest.
I selected a stock—in this case AAPL (Apple)—and analyzed its historical time series data, both daily and monthly.

The goal was to build an ARIMA model capable of producing an interval of possible future prices at a given date.

I used the tseries and forecast packages for the time series analysis, and quantmod to import the data.

By modifying the initial ticker, the model can be applied to any other stock.
