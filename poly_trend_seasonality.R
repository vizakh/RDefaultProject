poly_trend_seasonality <- function(data, test_data, seasonality,
                                   title, legend_pos) {
  data$time_index <- 1:nrow(data)
  data$seasonality <- seasonality(data$date)
  data$time_squared <- data$time_index^2
  
  poly_model <- lm(total ~ time_index + time_squared + factor(seasonality), 
                   data = data)
  print(summary(poly_model))
  
  test_data$time_index <- (nrow(data) + 1):(nrow(data) + nrow(test_data))
  test_data$time_squared <-  test_data$time_index^2
  test_data$seasonality <- seasonality(test_data$date)
  
  forecast_poly <- predict(poly_model, newdata = test_data, 
                           interval = "confidence", level = 0.95)
  test_data$fit <- forecast_poly[, "fit"]
  test_data$lwr <- forecast_poly[, "lwr"]
  test_data$upr <- forecast_poly[, "upr"]
  
  result_plot <- ggplot() +
    geom_line(data = data, aes(x = date, y = total, color = "Вихідні дані")) +
    geom_line(data = test_data, aes(x = date, y = fit, color = "Середнє значення")) +
    geom_line(data = test_data, aes(x = date, y = upr, color = "Довірчий інтервал (95%)")) +
    geom_line(data = test_data, aes(x = date, y = lwr, color = "Довірчий інтервал (95%)")) +
    labs(title = title,
         x = "Дата",
         y = "Підсумок") +
    scale_color_manual(values = c("Вихідні дані" = "black", "Тестові дані" = "grey",
                                  "Середнє значення" = "red", "Довірчий інтервал (95%)" = "blue")) +
    theme(legend.position = legend_pos,
          legend.background = element_rect(fill = "white", color = "black"),
          legend.title = element_blank())
  return(list(test_data$fit, result_plot, test_data$date))
}