library(magrittr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)

data <- read.table('F:/Pivdennyy/train.csv', header = TRUE, sep = ',',
                   colClasses = c("character", "NULL", "NULL", "numeric"))
head(data)

# Обробка даних та будування графіків-------------------------------------------
data$Date <- ymd(data$Date)
str(data)

data <- data %>%
  group_by(Date) %>%
  summarise(number_sold = sum(number_sold))

ggplot(data, aes(x = Date, y = number_sold)) +
  geom_line() + scale_x_date(date_labels = "%m-%Y")
head(data)

data = data[!(data$Date < as.POSIXct("2016-01-01")),]
ggplot(data, aes(x = Date, y = number_sold)) +
  geom_line() + scale_x_date(date_labels = "%m-%Y")

monthly_data <- data %>%
  mutate(month = ym(format(Date, "%Y-%m"))) %>%
  group_by(month) %>%
  summarize(number_sold = sum(number_sold))
head(monthly_data)

ggplot(monthly_data, aes(x = month, y = number_sold)) +
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
data$week_factor <- as.factor(as.numeric(format(data$Date, "%V")))  # Тиждень як фактор для сезонності
data$month_factor <- as.factor(as.numeric(format(data$Date, "%m"))) # Місяць як фактор для сезонності
print(data)

# Лінійна регресія з трендом та сезонністю
linear_seasonal_model <- lm(number_sold ~ time_index + week_factor, data = data)
summary(linear_seasonal_model)

# Прогноз на майбутнє
future_time_index <- (nrow(data) + 1):(nrow(data) + forecast_interval)  # Прогнозуємо на 12 місяців наперед

future_dates <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by="days")
future_week_factor <- as.factor(as.numeric(format(future_dates, "%V")))

future_data <- data.frame(time_index = future_time_index)
future_data$week_factor <- future_week_factor
print(future_data)

# Прогноз з довірчими інтервалами
forecast_seasonal <- predict(linear_seasonal_model, newdata = future_data, 
                             interval = "confidence", level = 0.95)

# Графік даних та прогнозів з інтервалами
plot(data$time_index, data$number_sold, type = "l", main = "LM with trend and seasonality", 
     xlab = "Time", ylab = "Defaults", xlim = c(1, max(future_time_index)),
     ylim = c(min(data$number_sold) - 300, max(data$number_sold)) + 300)
grid()
lines(future_time_index, forecast_seasonal[, "fit"], col = "red", lty = 1)  # Середнє значення прогнозу
lines(future_time_index, forecast_seasonal[, "lwr"], col = "blue", lty = 2) # Нижня границя інтервалу
lines(future_time_index, forecast_seasonal[, "upr"], col = "blue", lty = 2) # Верхня границя інтервалу

# Легенда до графіку
legend("bottomright", legend = c("Middle forecast", "Confidence interval (95%)"),
       col = c("red", "blue"), lty = c(1, 2), bty = "n")

# Модель з поліноміальним трендом та сезонним фактором--------------------------
data$time_squared <- data$time_index^2

poly_model <- lm(number_sold ~ time_index + time_squared + week_factor, data = data)
summary(poly_model)

# Прогноз
future_data <- data.frame(
  time_index = (nrow(data) + 1):(nrow(data) + forecast_interval),
  time_squared = ((nrow(data) + 1):(nrow(data) + forecast_interval))^2,
  week_factor = as.factor(as.numeric(format(future_dates, "%V")))
)

forecast_poly <- predict(poly_model, newdata = future_data, 
                         interval = "confidence", level = 0.95)

plot(data$time_index, data$number_sold, type = "l", main = "Polynomial model", 
     xlab = "Time", ylab = "Defaults", xlim = c(1, max(future_time_index)),
     ylim = c(min(data$number_sold) - 300, max(data$number_sold)) + 300)
grid()
lines(future_time_index, forecast_poly[, "fit"], col = "red", lty = 1)  # Середнє значення прогнозу
lines(future_time_index, forecast_poly[, "lwr"], col = "blue", lty = 2) # Нижня границя інтервалу
lines(future_time_index, forecast_poly[, "upr"], col = "blue", lty = 2) # Верхня границя інтервалу

# Легенда до графіку
legend("bottomright", legend = c("Middle forecast", "Confidence interval (95%)"),
       col = c("red", "blue"), lty = c(1, 2), bty = "n")

#-------------------------------------------------------------------------------
