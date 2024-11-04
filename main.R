library(dplyr)
library(lubridate)
library(xts)
library(ggplot2)
library(mgcv)
library(forecast)
library(RODBC)

source("process_data.R")
source("visualize_data.R")
source("vasicek.R")
source("linear_trend_seasonality.R")
source("poly_trend_seasonality.R")
source("gen_add_model.R")
source("ets_model.R")

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

visualize_data(all_daily_data, "По денно всі", 
               c("По денно всі", "Дата", "Підсумок"),
               c(0.87, 0.07))
visualize_data(cor_daily_data, "По денно КОР", 
               c("По денно КОР", "Дата", "Підсумок"),
               c(0.87, 0.07))
visualize_data(all_dates_data, "Звітні дати всі", 
               c("Звітні дати всі", "Дата", "Підсумок"),
               c(0.87, 0.07))
visualize_data(cor_dates_data, "Звітні дати КОР", 
               c("Звітні дати КОР", "Дата", "Підсумок"),
               c(0.87, 0.07))

# Модель Васічека---------------------------------------------------------------------------------
VasicekModel(all_daily_data, daily_test_data, 
             title = "Модель Васічека (щоденно)", legend_pos = c(0.16, 0.88))
VasicekModel(monthly_data, monthly_test_data, 
             title = "Модель Васічека (щомісячно)", legend_pos = c(0.16, 0.88))

# Лінійна модель з трендом та сезонністю----------------------------------------------------------
linear_trend_seasonality(daily_data, daily_test_data, week, 
                         title = "Лінійна регресія з трендом та сезонністю (щоденно)", 
                         legend_pos = c(0.2, 0.85))
linear_trend_seasonality(monthly_data, monthly_test_data, month, 
                         title = "Лінійна регресія з трендом та сезонністю (щомісячно)", 
                         legend_pos = c(0.17, 0.14))

# Поліноміальна модель з трендом та сезонністю----------------------------------------------------
poly_trend_seasonality(daily_data, daily_test_data, week, 
                       title = "Поліноміальна регресія з трендом та сезонністю (щоденно)", 
                       legend_pos = c(0.2, 0.85))
poly_trend_seasonality(monthly_data, monthly_test_data, month, 
                         title = "Поліноміальна регресія з трендом та сезонністю (щомісячно)", 
                         legend_pos = c(0.17, 0.85))

# General Additive Model (GAM)--------------------------------------------------------------------
gen_add_model(daily_data, daily_test_data, week,
              title = "GAM модель (щоденно)",
              legend_pos = c(0.2, 0.85))
gen_add_model(monthly_data, monthly_test_data, month,
              title = "GAM модель (щомісячно)",
              legend_pos = c(0.17, 0.85))

# # ETS модель--------------------------------------------------------------------------------------
# ets_model(daily_data, daily_test_data, 365)
# ets_model(monthly_data, monthly_test_data, 12)

weekly_data <- daily_data %>%
  group_by(Date = floor_date(Date, 'week')) %>%
  summarise(number_sold = sum(number_sold))
weekly_data <- weekly_data[2:(nrow(weekly_data) - 1),]

ts_data_weekly <- ts(weekly_data$number_sold, start = c(2016, 1), frequency = 7)
ts_data_monthly <- ts(monthly_data$number_sold, start = c(2016, 1), frequency = 12)
sarima_model <- auto.arima(ts_data_monthly, seasonal = TRUE)
sarima_model <- auto.arima(ts_data_weekly, seasonal = TRUE)

print(sarima_model)

# Прогноз на будущее (например, на 52 недели вперед)
forecast_horizon <- 26  # Прогноз на 1 год
sarima_forecast <- forecast(sarima_model, h = forecast_horizon)

# Отображение прогноза
plot(sarima_forecast)
