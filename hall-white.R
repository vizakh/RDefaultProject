# Функция для вычисления θ(t) для текущей кривой доходности
theta <- function(t) {
  # Здесь можно использовать аналитическое решение для θ(t) или подгонять
  # под конкретную текущую кривую доходности
  return(r0 + 100000 * t * t)
}

# Функция для генерации пути процентных ставок по модели Халла-Уайта
hull_white_simulation <- function(a, sigma, r0, T, dt) {
  n_steps <- T / dt
  r <- numeric(n_steps + 1)
  r[1] <- r0
  
  for (i in 2:(n_steps + 1)) {
    t <- (i - 1) * dt
    drift <- (theta(t) - a * r[i - 1]) * dt
    diffusion <- sigma * sqrt(dt) * rnorm(1)
    r[i] <- r[i - 1] + drift + diffusion
  }
  
  return(r)
}

# Подготовка данных
n <- nrow(cor_dates_data)
delta_r <- diff(cor_dates_data$total)  # изменения процентных ставок
r_lag <- head(cor_dates_data$total, -1)  # лагированные значения ставок
dt <- 1 / 12  # например, если данные ежемесячные

# Линейная регрессия для оценки параметра a
reg <- lm(delta_r ~ r_lag)
a_hat <- -coef(reg)[2]  # оценка параметра скорости возврата

# Оценка волатильности σ
sigma_hat <- sd(residuals(reg)) / sqrt(dt)

# Вывод параметров
cat("Оценка параметра a:", a_hat, "\n")
cat("Оценка волатильности sigma:", sigma_hat, "\n")

# Установленные параметры
a <- a_hat
sigma <- sigma_hat
r0 <- tail(cor_dates_data$total, 1)     # начальное значение для прогноза — последнее значение временного ряда
T <- length(future_monthly_dates) / 12  # горизонт прогноза, например, 1 год
dt <- 1 / 12                            # шаг прогнозирования, ежемесячно

# Генерация прогноза процентных ставок
set.seed(411)
forecast_path <- hull_white_simulation(a, sigma, r0, T, dt)

# Построение графика прогноза
forecast_df <- data.frame(future_monthly_dates, total = forecast_path)
ggplot() +
  geom_line(data = forecast_df, aes(x = date, y = total), color = "blue") +
  geom_line(data = cor_dates_data, aes(x = date, y = total), color = "black") +
  labs(title = "Прогноз процентной ставки по модели Халла-Уайта",
       x = "Время", y = "Процентная ставка") +
  theme_minimal()
