# Функція для побудови GAM-моделі з урахуванням сезонності.
gen_add_model <- function(data, test_data, future_data, seasonality, title, legend_pos) {
  # Формуємо стовбці з індексом часу від 1 до загальної кількості дат та з 
  # сезонністю. 
  data$time_index <- 1:nrow(data)
  data$seasonality <- seasonality(data$date)
  
  # Будуємо модель на основі часового індексу та сезонності у якості фактору.
  gam_model <- gam(total ~ s(time_index) + factor(seasonality), data = data)
  
  # Виводимо у консоль інформацію щодо моделі.
  print(summary(gam_model))
  
  # Формуємо часовий індекс та сезонність для тестових даних для коректної 
  # роботи функції прогнозування.
  test_data$time_index <- (nrow(data) + 1):(nrow(data) + nrow(test_data))
  test_data$seasonality <- seasonality(test_data$date)
  
  # Генеруємо прогноз на усі тестові дати, а також довірчі інтервали.
  forecast_gam <- predict(gam_model, newdata = test_data, se.fit = TRUE)
  
  # Обчислення довірчих інтервалів з урахуванням стандартного відхилення.
  upr <- forecast_gam$fit + (2 * forecast_gam$se.fit)
  lwr <- forecast_gam$fit - (2 * forecast_gam$se.fit)

  # Записуємо результати у набір тестових даних:
  # fit - згенерований прогноз;
  # upr - верхня межа довірчого інтервалу;
  # lwr - нижня межа довірчого інтервалу.  
  test_data$fit <- forecast_gam$fit
  test_data$lwr <- lwr
  test_data$upr <- upr
  
  # Формуємо результуючий графік з даними із train-test (існуючі дані), а також
  # графік прогнозу та довірчих інтервалів.
  result_plot <- ggplot() +
    geom_line(data = data, aes(x = date, y = total, color = "Вихідні дані")) +
    geom_line(data = test_data[test_data$date < min(future_data$date),], 
              aes(x = date, y = total, color = "Тестові дані")) +
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
  
  # Повертаємо список зі значень прогнозу у вигляді таблиці, результуючого 
  # графіку та набору дат, на які робиться прогноз.
  return(list(test_data$fit, result_plot, test_data$date))
}