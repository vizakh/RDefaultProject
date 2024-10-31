library(magrittr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)

data <- read.table('F:/Pivdennyy/train.csv', header = TRUE, sep = ',',
                   colClasses = c("character", "NULL", "NULL", "numeric"))
print(data)

# Обробка даних та будування графіків-------------------------------------------
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

# Лінійна регресія з трендом----------------------------------------------------
data$time_index <- 1:nrow(data)
print(data)

linear_trend_model <- lm(number_sold ~ time_index, data = data)
summary(linear_trend_model)

# Прогноз на майбутнє
forecast_interval <- 30 * 12 + 5
future_time_index <- (nrow(data) + 1):(nrow(data) + forecast_interval)  # Прогнозуємо на 12 місяців наперед
forecast_trend <- predict(linear_trend_model, newdata = data.frame(time_index = future_time_index))

# Графік прогнозу
plot(data$time_index, data$number_sold, 
     type = "l", main = "LM with trend", xlab = "Time", ylab = "Defaults",
     xlim =c(0, max(data$time_index) + forecast_interval))
grid()
lines(future_time_index, forecast_trend, col = "blue", lty = 2)

# Лінійна регресія з трендом та сезонністю--------------------------------------
data$month_factor <- as.factor(as.numeric(format(data$Date, "%V")))  # Місяць як фактор для сезонності
print(data)

# Лінійна регресія з трендом та сезонністю
linear_seasonal_model <- lm(number_sold ~ time_index + month_factor, data = data)
summary(linear_seasonal_model)

# Прогноз на майбутнє
future_time_index <- (nrow(data) + 1):(nrow(data) + forecast_interval)  # Прогнозуємо на 12 місяців наперед

future_dates <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by="days")
future_month_factor <- as.factor(as.numeric(format(future_dates, "%V")))

future_data <- data.frame(time_index = future_time_index)
future_data$month_factor <- future_month_factor
print(future_data)

# Прогноз з довірчими інтервалами
forecast_seasonal <- predict(linear_seasonal_model, newdata = future_data, 
                             interval = "confidence", level = 0.95)

# Графік даних та прогнозів з інтервалами
plot(data$time_index, data$number_sold, type = "l", main = "LM with trend and seasonality", xlab = "Time", ylab = "Defaults", xlim = c(1, max(future_time_index)))
grid()
lines(future_time_index, forecast_seasonal[, "fit"], col = "red", lty = 1)         # Середнє значення прогнозу
lines(future_time_index, forecast_seasonal[, "lwr"], col = "blue", lty = 2)        # Нижня границя інтервалу
lines(future_time_index, forecast_seasonal[, "upr"], col = "blue", lty = 2)        # Верхня границя інтервалу

# Легенда до графіку
legend("bottomright", legend = c("Middle forecast", "Confidence interval (95%)"),
       col = c("red", "blue"), lty = c(1, 2), bty = "n")

# Модель ETS--------------------------------------------------------------------
# Временной ряд данных о дефолтах
data_ts <- ts(data$number_sold, frequency = 12) # Предполагаем месячную сезонность

# Построение модели ETS
ets_model <- ets(data_ts, model="MAA")
forecast_ets <- forecast(ets_model, h = 100) # Прогноз на 12 периодов

# График прогноза
plot(forecast_ets)

# # Модель на основі сезонних індексів--------------------------------------------
# # Разложение временного ряда
# decomposed <- decompose(data_ts)
# 
# # Построение прогноза с использованием сезонных индексов
# seasonally_adjusted <- data_ts - decomposed$seasonal
# linear_model <- tslm(seasonally_adjusted ~ trend) # Прогноз с учетом тренда
# forecast_linear <- forecast(linear_model, h = 54)
# adjusted_forecast <- forecast_linear$mean + decomposed$seasonal[1:54] # Учёт сезонности
# 
# # График прогноза
# plot(adjusted_forecast)
