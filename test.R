library(magrittr)
library(dplyr)
library(lubridate)
library(ggplot2)

data <- read.table('F:/Pivdennyy/train.csv', header = TRUE, sep = ',',
                   colClasses = c("character", "NULL", "NULL", "numeric"))
print(data)

data$Date <- ymd(data$Date)
str(data)

data <- data %>%
  group_by(Date) %>%
  summarise(number_sold = sum(number_sold))

ggplot(data, aes(x = Date, y = number_sold)) +
  geom_line() + scale_x_date(date_labels = "%m-%Y")
print(data)

data = data[!(data$Date < as.POSIXct("2016-01-01")),]
ggplot(data, aes(x = Date, y = number_sold)) +
  geom_line() + scale_x_date(date_labels = "%m-%Y")

# ------------------------------------------------------------------------------
data$time_index <- 1:nrow(data)
print(data)
# ------------------------------------------------------------------------------
# Линейная регрессия по тренду
linear_trend_model <- lm(number_sold ~ time_index, data = data)
summary(linear_trend_model)

# Прогноз на будущее
forecast_interval <- 30 * 12
future_time_index <- (nrow(data) + 1):(nrow(data) + forecast_interval)  # Прогноз на 12 месяцев вперёд
forecast_trend <- predict(linear_trend_model, newdata = data.frame(time_index = future_time_index))

# График прогноза
plot(data$time_index, data$number_sold, 
     type = "l", main = "Линейная регрессия по тренду", xlab = "Время", ylab = "Дефолты",
     xlim =c(0, max(data$time_index) + forecast_interval))
lines(future_time_index, forecast_trend, col = "blue", lty = 2)
# ------------------------------------------------------------------------------
data$month_factor <- as.factor(format(data$Date, "%m"))  # Месяц как фактор для сезонности
print(data)

# Линейная регрессия с трендом и сезонностью
linear_seasonal_model <- lm(number_sold ~ time_index + month_factor, data = data)
summary(linear_seasonal_model)

# Прогноз на будущее
future_time_index <- (nrow(data) + 1):(nrow(data) + forecast_interval)  # Прогнозируем на 12 месяцев вперёд
future_month_factor <- as.factor(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))
future_data <- data.frame(time_index = future_time_index)
future_data$month_factor <- future_month_factor
print(future_data)
str(future_data)
str(data)

# Построение прогноза
forecast_seasonal <- predict(linear_seasonal_model, newdata = future_data)

# График прогноза
plot(data$time_index, data$number_sold, 
     type = "l", main = "Линейная регрессия с трендом и сезонностью", xlab = "Время", ylab = "Дефолты",
     xlim =c(0, max(data$time_index) + forecast_interval))
lines(future_time_index, forecast_seasonal, col = "red", lty = 2)
