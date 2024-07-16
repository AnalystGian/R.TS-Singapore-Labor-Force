#https://www.gapminder.org/data/
#https://docs.google.com/spreadsheets/d/1frieoKODnD9sX3VCZy5c3QAjBXMY-vN7k_I9gR-gcU8/pub

# Singapore data
"70.19999695	71.09999847	71.69999695	72.30000305	73.09999847	72.90000153	74.40000153	75.40000153	76	76.90000153	77.40000153	78.19999695	78.90000153	78.69999695	79	78	80	79.80000305	80.30000305	80.5	80.69999695	81.09999847	81.5	81.90000153	82.30000305	82.69999695	83.19999695	83.5"

# Import with Scan
singapore = scan()
singapore

# Convert to time series
singapore = ts(singapore, start = 1980)

plot(singapore, ylab = "Labor Force Participation Rate 25-54")

library(forecast)
# Exponential smoothing with holt
holttrend = holt(singapore, h = 5)
summary(holttrend)
plot(holttrend)

# Phi auto generated
plot(holt(singapore, h = 15, damped = T))
# To see the generated value for Phi
summary(holt(singapore, h = 15, damped = T))

# Manual setting of Phi
plot(holt(singapore, h = 15, damped = T, phi = 0.8))

# Arima auto generated
singapore.arima = auto.arima(singapore)

summary(singapore.arima)

plot(forecast(singapore.arima, h = 5))

# Exact calculation of Arima parameters
auto.arima(singapore, stepwise = F, approximation = F)

# Overview plot - models
holttrend = holt(singapore, h = 10)
holtdamped = holt(singapore, h = 10, damped = T)
arimafore = forecast(auto.arima(singapore), h = 10)
library(ggplot2)

# 3 Forecast Lines as Comparison
autoplot(singapore) +
  forecast::autolayer(holttrend$mean, series = "Holt Linear Trend") +
  forecast::autolayer(holtdamped$mean, series = "Holt Damped Trend") +
  forecast::autolayer(arimafore$mean, series = "ARIMA") +
  xlab("year") + ylab("Labour Force Participation Rate Age 25-54") + 
  guides(colour=guide_legend(title="Forecast Method")) + theme(legend.position = c(0.8, 0.2)) +
  ggtitle("Singapore") + theme(plot.title=element_text(family="Times", hjust = 0.5, color = "blue",
                                                      face="bold", size=15))
# Exercise - Dataset vs Model 
# Models
holttrend = holt(singapore, h = 10)
holtdamped = holt(singapore, h = 10, damped = T)
arimafore = forecast(auto.arima(singapore), h = 10)

autoplot(singapore) + geom_line(size=2) +
  forecast::autolayer(holttrend$fitted, series = "Holt Linear Trend", size = 1.1) +
  forecast::autolayer(holtdamped$fitted, series = "Holt Damped Trend", size = 1.1) +
  forecast::autolayer(arimafore$fitted, series = "ARIMA", size = 1.1) +
  xlab("year") + ylab("Labour Force Participation Rate Age 25-54") + 
  guides(colour=guide_legend(title="Forecast Method")) + theme(legend.position = c(0.8, 0.2)) +
  ggtitle("Singapore") + theme(plot.title=element_text(family="Times", hjust = 0.5, 
                                                      color = "blue", face="bold", size=15))

# Calculate error metrics for training set
arima_accuracy <- accuracy(arimafore)
holt_linear_accuracy <- accuracy(holttrend)
holt_damped_accuracy <- accuracy(holtdamped)

arima_accuracy
holt_linear_accuracy
holt_damped_accuracy