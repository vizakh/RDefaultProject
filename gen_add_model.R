gen_add_model <- function(data, test_data, seasonality, title, legend_pos) {
  data$time_index <- 1:nrow(data)
  data$seasonality <- seasonality(data$date)
  
  gam_model <- gam(total ~ s(time_index) + factor(seasonality), data = data)
  print(summary(gam_model))
  
  test_data$time_index <- (nrow(data) + 1):(nrow(data) + nrow(test_data))
  test_data$seasonality <- seasonality(test_data$date)
  
  forecast_gam <- predict(gam_model, newdata = test_data, se.fit = TRUE)
  
  upr <- forecast_gam$fit + (2 * forecast_gam$se.fit)
  lwr <- forecast_gam$fit - (2 * forecast_gam$se.fit)
  
  test_data$fit <- forecast_gam$fit
  test_data$lwr <- lwr
  test_data$upr <- upr
  
  ggplot() +
    geom_line(data = data, aes(x = date, y = total, color = "Вихідні дані")) +
    geom_line(data = test_data, aes(x = date, y = total, color = "Тестові дані")) +
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
}