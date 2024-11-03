linear_trend_seasonality <- function(data, test_data, seasonality, 
                                     title, legend_pos) {
  data$time_index <- 1:nrow(data)
  data$seasonality <- seasonality(data$Date)
  
  linear_seasonal_model <- lm(number_sold ~ time_index + factor(seasonality), 
                              data = data)
  summary(linear_seasonal_model)
  
  test_data$time_index <- (nrow(data) + 1):(nrow(data) + nrow(test_data))
  test_data$seasonality <- seasonality(test_data$Date)
  
  forecast_seasonal <- predict(linear_seasonal_model, newdata = test_data, 
                               interval = "confidence", level = 0.95)
  test_data$fit <- forecast_seasonal[, "fit"]
  test_data$lwr <- forecast_seasonal[, "lwr"]
  test_data$upr <- forecast_seasonal[, "upr"]
  
  ggplot() +
    geom_line(data = data, aes(x = Date, y = number_sold, color = "Вихідні дані")) +
    geom_line(data = test_data, aes(x = Date, y = number_sold, color = "Тестові дані")) +
    geom_line(data = test_data, aes(x = Date, y = fit, color = "Середнє значення")) +
    geom_line(data = test_data, aes(x = Date, y = upr, color = "Довірчий інтервал (95%)")) +
    geom_line(data = test_data, aes(x = Date, y = lwr, color = "Довірчий інтервал (95%)")) +
    labs(title = title,
         x = "Дата",
         y = "Продажі") +
    scale_color_manual(values = c("Вихідні дані" = "black", "Тестові дані" = "grey",
                                  "Середнє значення" = "red", "Довірчий інтервал (95%)" = "blue")) +
    theme(legend.position = legend_pos,  # Перемещение легенды внутрь графика
          legend.background = element_rect(fill = "white", color = "black"),
          legend.title = element_blank())
  
}