library(dplyr)
library(lubridate)
library(xts)
library(ggplot2)
library(mgcv)
library(forecast)
library(RODBC)
library(gsubfn)
library(patchwork)
library(openxlsx)
library(tidyverse)
library(stats4)
library(tseries)
library(astsa)

source("process_data.R")
source("visualize_data.R")
source("vasicek.R")
source("linear_trend_seasonality.R")
source("poly_trend_seasonality.R")
source("gen_add_model.R")
source("all_models.R")

wb <- "F:/Pivdennyy/stat.xlsb"
con2 <- odbcConnectExcel2007(wb)
all_daily_data <- sqlFetch(con2, "По денно всі")
cor_daily_data <- sqlFetch(con2, "По денно КОР")
all_dates_data <- sqlFetch(con2, "Звітні дати всі")
cor_dates_data <- sqlFetch(con2, "Звітні дати КОР")

all_daily_data <- process_daily_data(all_daily_data)
cor_daily_data <- process_daily_data(cor_daily_data)
all_dates_data <- process_daily_data(all_dates_data)
cor_dates_data <- process_daily_data(cor_dates_data)

# visualize_data(all_daily_data, "По денно всі",
#                c("По денно всі", "Дата", "Підсумок"),
#                c(0.87, 0.07))
# visualize_data(cor_daily_data, "По денно КОР",
#                c("По денно КОР", "Дата", "Підсумок"),
#                c(0.87, 0.07))
# visualize_data(all_dates_data, "Звітні дати всі",
#                c("Звітні дати всі", "Дата", "Підсумок"),
#                c(0.87, 0.07))
# visualize_data(cor_dates_data, "Звітні дати КОР",
#                c("Звітні дати КОР", "Дата", "Підсумок"),
#                c(0.87, 0.07))

last_date <- as.POSIXct('2025-12-31')
future_daily_dates <- data.frame("date" = seq(all_daily_data$date[nrow(all_daily_data)], 
                                              last_date, 
                                              by = 'days'))
future_monthly_dates <- data.frame("date" = seq(all_daily_data$date[nrow(all_daily_data)], 
                                                last_date, 
                                                by = 'months'))

set.seed(411)
all_daily <- all_models(all_daily_data, future_daily_dates, 
                        week, title = "по денно всі")
cor_daily <- all_models(cor_daily_data, future_daily_dates, 
                        week, title = "по денно КОР")
all_dates <- all_models(all_dates_data, future_monthly_dates,
                        month, title = "звітні дати всі")
cor_dates <- all_models(cor_dates_data, future_monthly_dates,
                        month, title = "звітні дати КОР")

OUT <- createWorkbook()

addWorksheet(OUT, "По денно всі")
addWorksheet(OUT, "По денно КОР")
addWorksheet(OUT, "Звітні дати всі")
addWorksheet(OUT, "Звітні дати КОР")

writeData(OUT, sheet = "По денно всі", x = all_daily[[1]])
writeData(OUT, sheet = "По денно КОР", x = cor_daily[[1]])
writeData(OUT, sheet = "Звітні дати всі", x = all_dates[[1]])
writeData(OUT, sheet = "Звітні дати КОР", x = cor_dates[[1]])

# saveWorkbook(OUT, "F:/Pivdennyy/result_models_future.xlsx")

# print(all_daily[[2]])
# print(cor_daily[[2]])
# print(all_dates[[2]])
# print(cor_dates[[2]])

# data <- cor_dates_data$total
# 
# ts_data <- ts(log(data))
# 
# # ETS---------------------------------------------------------------------------
# ts_data <- ts(all_daily_data$total, frequency = 1)
# 
# # Построение модели ETS
# ets_model <- ets(ts_data)
# 
# # Прогноз на 12 периодов вперед
# ets_forecast <- forecast(ets_model, h = 365)
# 
# # Вывод прогноза
# summary(ets_model)
# plot(ets_forecast)
# 
# # ARIMA-------------------------------------------------------------------------
# par(mfrow=c(1, 2))
# acf(ts_data, main="ACF of AR(2)")
# acf(ts_data, type="partial", main="PACF of AR(2)")
# 
# acf(diff(ts_data), main='ACF')
# acf(diff(ts_data), type='partial', main='PACF')
# 
# acf(diff(diff(ts_data)), main='ACF')
# acf(diff(diff(ts_data)), type='partial', main='PACF')
# par(mfrow=c(1, 1))
# 
# aic_min <- 1e10
# for (p in 0:13) {
#   for (d in 0:3) {
#     for (q in 0:2) {
#       try(
#         {
#           model <- arima(ts_data, order=c(p, d, q))
#           print(paste(p, d, q, 'RSS =', sum(resid(model)^2),
#                       'AIC =', model$aic))
#           if (model$aic < aic_min) {
#             aic_min <- model$aic
#             best_par <- c(p, d, q)
#           }
#         },
#         silent = TRUE
#       )
#     }
#   }
# }
# print(best_par)
# 
# best.model <- arima(ts_data, order=c(0,2,1))
# plot.ts(best.model$residuals, main='Residuals of ARIMA-model')
# Box.test(best.model$residuals, lag=log(length(best.model$residuals)),
#          type="Ljung-Box")
# 
# forecast_model <- forecast(best.model, h = 90)
# 
# plot(forecast_model)
# 
# for (p in 0:1) {
#   for (P in 0:1) {
#     for (q in 0:1) {
#       for (Q in 0:1) {
#         model <- arima(ts_data,
#                        order=c(p, 1, q),
#                        seasonal=list(order=c(P, 1, Q), period=3))
#         b <- Box.test(resid(model), lag=log(length(ts_data)), type="Ljung-Box")
#         print(paste(p, 1, q, P, 1, Q, 4,
#                     'RSS =', round(sum(resid(model)^2), 3),
#                     'AIC =', round(model$aic, 3),
#                     'p-value =', round(b$p.value, 3)))
#       }
#     }
#   }
# }
# 
# best.model <- arima(ts_data, order=c(1,1,0),
#                     seasonal=list(order=c(0,1,1), period=3))
# plot.ts(best.model$residuals, main='Residuals of ARIMA-model')
# Box.test(best.model$residuals, lag=log(length(best.model$residuals)),
#          type="Ljung-Box")
# 
# forecast_model <- forecast(best.model, h = 90)
# 
# plot(forecast_model)
# 
# auto.model <- auto.arima(ts_data, d=2, max.p=13)
# plot.ts(auto.model$residuals, main='Residuals of ARIMA-model')
# Box.test(auto.model$residuals, lag=log(length(auto.model$residuals)),
#          type="Ljung-Box")
# 
# forecast_model <- forecast(auto.model, h = 90)
# 
# plot(forecast_model)
