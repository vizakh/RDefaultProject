library(dplyr)
library(lubridate)
library(xts)
library(ggplot2)
library(mgcv)
library(forecast)

source("process_data.R")
source("visualize_data.R")
source("vasicek.R")
source("linear_trend_seasonality.R")
source("poly_trend_seasonality.R")
source("gen_add_model.R")
source("ets_model.R")

data <- read.table('F:/Pivdennyy/train.csv', header = TRUE, sep = ',')
data_limit_dates <- c(as.POSIXct("2016-01-01"), as.POSIXct("2019-01-01"))
daily_data <- process_daily_data(data, data_limit_dates)
monthly_data <- process_monthly_data(data, data_limit_dates)

test_data <- read.table('F:/Pivdennyy/test.csv', header = TRUE, sep = ',')
daily_test_data <- process_daily_data(test_data)
monthly_test_data <- process_monthly_data(test_data)

head(daily_data)
head(daily_test_data)

head(monthly_data)
head(monthly_test_data)

visualize_data(daily_data, "Вихідні дані (щоденні)", 
               c("Вихідні дані", "Дата", "Продажі"),
               c(0.84, 0.07))

visualize_data(monthly_data, "Вихідні дані (щомісячні)", 
               c("Вихідні дані (щомісячні)", "Дата", "Продажі"),
               c(0.17, 0.07))

# Модель Васічека---------------------------------------------------------------------------------
VasicekModel(daily_data, daily_test_data, 
             title = "Модель Васічека (щоденно)", legend_pos = c(0.16, 0.88))
VasicekModel(monthly_data, monthly_test_data, 
             title = "Модель Васічека (щомісячно)", legend_pos = c(0.16, 0.88))

# Лінійна модель з трендом та сезонністю----------------------------------------------------------
linear_trend_seasonality(daily_data, daily_test_data, week, 
                         title = "Лінійна регресія з трендом та сезонністю (щоденно)", 
                         legend_pos = c(0.2, 0.85))
linear_trend_seasonality(monthly_data, monthly_test_data, quarter, 
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
# ets_model(daily_data, 50, title = "ETS модель (щоденно)", legend_pos = c(0.2, 0.85))

