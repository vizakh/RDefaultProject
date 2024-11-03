library(dplyr)
library(lubridate)
library(xts)
library(ggplot2)

source("process_data.R")
source("linear_trend_seasonality.R")

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

ggplot(daily_data, aes(x = Date)) +
  geom_line(aes(y = number_sold, color = "Вихідні дані")) +
  labs(title = "Вихідні дані",
       x = "Дата",
       y = "Продажі") +
  scale_color_manual(values = c("Вихідні дані" = "black")) +
  theme(legend.position = c(0.89, 0.05),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank())

ggplot(monthly_data, aes(x = Date)) +
  geom_line(aes(y = number_sold, color = "Вихідні дані")) +
  labs(title = "Вихідні дані (щомісячні)",
       x = "Дата",
       y = "Продажі") +
  scale_color_manual(values = c("Вихідні дані" = "black")) +
  theme(legend.position = c(0.89, 0.05),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank())

linear_trend_seasonality(daily_data, daily_test_data, week, 
                         legend_pos = c(0.2, 0.85))

linear_trend_seasonality(monthly_data, monthly_test_data, quarter, 
                         legend_pos = c(0.17, 0.89))
