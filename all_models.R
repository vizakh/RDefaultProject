# Функція для побудови та прогнозування з використанням кожної моделі
# для окремого набору даних. Аргументами є:
# data - набір даних train;
# test_data - набір даних test;
# future_data - майбутні дані з датами;
# seasonality - сезонність для побудови моделей (week для щоденних даних, 
#               month для щомісячних);
# title - назва набору даних.
# 
# Повертається: список з даних для запису в Excel у вигляді таблиці та 
#               комбінований графік з графіками усіх моделей.
all_models <- function(data, test_data, future_data, seasonality, title) {
  
  # Далі працюють усі необхідні моделі та записуються їхні результати в 
  # окремі змінні. Функція кожної моделі приймає train, test та майбутні дати,
  # а також сезонність (за вийнятком Васічека), назву графіку та положення 
  # легенди на графіку.
  data_vasicek <- VasicekModel(data, test_data, future_data,
                               title = glue::glue("Модель Васічека ({title})"), 
                               legend_pos = c(0.2, 0.85))
  data_linear <- linear_trend_seasonality(data, test_data, future_data, seasonality, 
                                          title = glue::glue("Лінійна регресія з трендом та сезонністю ({title})"),
                                          legend_pos = c(0.2, 0.85))
  # data_poly <- poly_trend_seasonality(data, test_data, future_data, seasonality, 
  #                                     title = glue::glue("Поліноміальна регресія з трендом та сезонністю ({title})"), 
  #                                     legend_pos = c(0.2, 0.85))
  data_regularization <- ridge_lasso(data, test_data, future_data, seasonality,
                                     title = glue::glue("Ridge та Lasso регресії ({title})"),
                                     legend_pos = c(0.2, 0.85))
  data_gam <- gen_add_model(data, test_data, future_data, seasonality, 
                            title = glue::glue("GAM модель ({title})"), 
                            legend_pos = c(0.2, 0.85))
  
  # Формуємо таблицю з датами та прогнозами усіх моделей
  numerical_results <- data.frame("Дата" = data_vasicek[[3]],
                                  "Васічек" = data_vasicek[[1]], 
                                  "Лінійна" = data_linear[[1]],
                                  "Ridge" = data_regularization[[1]],
                                  "Lasso" = data_regularization[[4]],
                                  "GAM" = data_gam[[1]])
  
  # Формуємо комбінований графік прогнозів
  data_combined_plot <- data_vasicek[[2]] + data_linear[[2]] + data_regularization[[2]] + data_gam[[2]]
  
  # Повертаємо список
  return(list(numerical_results, data_combined_plot))
}
