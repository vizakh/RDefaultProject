library(dplyr)
library(lubridate)
library(xts)
library(ggplot2)

source("process_data.R")
source("linear_trend_seasonality.R")
source("visualize_data.R")

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

visualize_data(daily_data, "Вихідні дані", 
               c("Вихідні дані", "Дата", "Продажі"),
               c(0.89, 0.07))

visualize_data(monthly_data, "Вихідні дані (щомісячні)", 
               c("Вихідні дані (щомісячні)", "Дата", "Продажі"),
               c(0.17, 0.07))

linear_trend_seasonality(daily_data, daily_test_data, week, 
                         title = "Лінійна регресія з трендом та сезонністю (щоденно)", 
                         legend_pos = c(0.2, 0.85))

linear_trend_seasonality(monthly_data, monthly_test_data, quarter, 
                         title = "Лінійна регресія з трендом та сезонністю (щомісячно)", 
                         legend_pos = c(0.17, 0.14))
