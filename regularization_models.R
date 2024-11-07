# Функція для побудови моделей Ridge та Lasso з регуляризацією.
ridge_lasso <- function(data, test_data, future_data, seasonality, 
                        title, legend_pos) {
  # Записуємо у змінну кількість прогнозованих періодів.
  forecast_periods <- nrow(test_data)
  
  # Визначаємо змінні для побудови моделей
  n <- nrow(data)
  x <- 1:nrow(data)
  y <- data$total
  X <- as.matrix(poly(x, degree = 2, raw = TRUE))
  
  # Будуємо моделі (параметр alpha відповідає за те, яка модель формується)
  cv_ridge <- cv.glmnet(X, y, alpha = 0) # Ridge
  cv_lasso <- cv.glmnet(X, y, alpha = 1) # Lasso
  
  # Виводимо у консоль інформацію щодо моделей.
  print(cv_ridge)
  print(cv_lasso)
  
  # Формуємо зміннім для прогнозування.
  future_x <- seq(n + 1, n + forecast_periods)
  X_future <- as.matrix(poly(future_x, degree = 2, raw = TRUE))
  
  # Робимо прогноз.
  ridge_pred_future <- predict(cv_ridge, s = cv_ridge$lambda.min, newx = X_future)
  lasso_pred_future <- predict(cv_lasso, s = cv_lasso$lambda.min, newx = X_future)
  
  # Записуэмо прогноз у набір тестових даних.
  test_data$ridge <- ridge_pred_future[,1]
  test_data$lasso <- lasso_pred_future[,1]
  
  # Формуємо результуючий графік з даними із train-test (існуючі дані), а також
  # графіки прогнозу.
  result_plot <- ggplot() +
    geom_line(data = data, aes(x = date, y = total, color = "Вихідні дані")) +
    geom_line(data = test_data[test_data$date < min(future_data$date),], 
              aes(x = date, y = total, color = "Тестові дані")) +
    geom_line(data = test_data, aes(x = date, y = ridge, color = "Ridge прогноз")) +
    geom_line(data = test_data, aes(x = date, y = lasso, color = "Lasso прогноз")) +
    labs(title = title,
         x = "Дата",
         y = "Підсумок") +
    scale_color_manual(values = c("Вихідні дані" = "black", "Тестові дані" = "grey",
                                  "Ridge прогноз" = "red", "Lasso прогноз" = "blue")) +
    theme(legend.position = legend_pos,
          legend.background = element_rect(fill = "white", color = "black"),
          legend.title = element_blank())
  
  # Повертаємо список зі значень прогнозу Ridge у вигляді таблиці, результуючого 
  # графіку, набору дат, на які робиться прогноз, та прогнозу Lasso.
  return(list(test_data$ridge, result_plot, test_data$date, test_data$lasso))
}