# Libraries
library(prophet)
library(tidyverse)
library(crypto2)

# Get Data from CoinMarketCap
coin_list <- crypto_list(only_active = TRUE)
coin_list <- coin_list[coin_list$name == "Avalanche", ] # change to desired coin name

crypto_hist <- crypto_history(coin_list = coin_list,
                              convert = "CAD", # change to USD or other currency if desired
                              limit = NULL,
                              sleep = 1)

# Select Date and Time from Dataset & ignore the rest
crypto_hist <- select(crypto_hist,timestamp, close)

# Rename columns to "ds" and "y" in order to use Prophet library
crypto_hist <- rename(crypto_hist, ds = timestamp, y = close)

# Fix date data
crypto_hist$ds <- as.Date(crypto_hist$ds)

# Plot data
qplot(ds, y, data = crypto_hist)

# Log transform the data & plot again to compare
crypto_hist$y <- log(crypto_hist$y)
qplot(ds, y, data = crypto_hist)

# Generate model using Prophet library
prophet_model <- prophet(crypto_hist)
future_model <- make_future_dataframe(prophet_model, periods = 365)
tail(future_model)

# Forecast
forecast1 <- predict(prophet_model, future_model)
tail(forecast1[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

# Plot Forecast
dyplot.prophet(prophet_model, forecast1)
prophet_plot_components(prophet_model, forecast1)

# Model Performance
pred <- forecast1$yhat[1:453] #adjust this range based on dataset of crypto_hist
actual <- prophet_model$history$y
plot(actual, pred)
abline(lm(pred~actual), col = 'red')

# RSquared
summary(lm(pred~actual))

# Cross Validation
x <- cross_validation(prophet_model, horizon = 32,365, units = 'days')
performance_metrics(x, rolling_window = 0.1)
plot_cross_validation_metric(x, metric = 'rmse', rolling_window = 0.1)
