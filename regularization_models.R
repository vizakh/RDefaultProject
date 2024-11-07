ridge_lasso <- function(data, test_data, future_data, seasonality, 
                        title, legend_pos) {
  forecast_periods <- nrow(test_data)
  
  n <- nrow(data)
  x <- 1:nrow(data)
  y <- data$total
  
  X <- as.matrix(poly(x, degree = 2, raw = TRUE))
  
  cv_ridge <- cv.glmnet(X, y, alpha = 0)
  cv_lasso <- cv.glmnet(X, y, alpha = 1)
  
  print(cv_ridge)
  print(cv_lasso)
  
  future_x <- seq(n + 1, n + forecast_periods)
  X_future <- as.matrix(poly(future_x, degree = 2, raw = TRUE))
  
  ridge_pred_future <- predict(cv_ridge, s = cv_ridge$lambda.min, newx = X_future)
  lasso_pred_future <- predict(cv_lasso, s = cv_lasso$lambda.min, newx = X_future)
  
  test_data$ridge <- ridge_pred_future[,1]
  test_data$lasso <- lasso_pred_future[,1]
  
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
  
  return(list(test_data$ridge, result_plot, test_data$date, test_data$lasso))
}