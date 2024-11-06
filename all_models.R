all_models <- function(data, test_data, future_data, seasonality, title) {
  data_vasicek <- VasicekModel(data, test_data, future_data,
                               title = glue::glue("Модель Васічека ({title})"), 
                               legend_pos = c(0.2, 0.85))
  data_linear <- linear_trend_seasonality(data, test_data, future_data, seasonality, 
                                          title = glue::glue("Лінійна регресія з трендом та сезонністю ({title})"),
                                          legend_pos = c(0.2, 0.85))
  data_poly <- poly_trend_seasonality(data, test_data, future_data, seasonality, 
                                      title = glue::glue("Поліноміальна регресія з трендом та сезонністю ({title})"), 
                                      legend_pos = c(0.2, 0.85))
  data_gam <- gen_add_model(data, test_data, future_data, seasonality, 
                            title = glue::glue("GAM модель ({title})"), 
                            legend_pos = c(0.2, 0.85))
  
  numerical_results <- data.frame("Дата" = data_vasicek[[3]],
                                  "Васічек" = data_vasicek[[1]], 
                                  "Лінійна" = data_linear[[1]],
                                  "Поліноміальна" = data_poly[[1]], 
                                  "GAM" = data_gam[[1]])
  
  data_plots <- list(data_vasicek[[2]], data_linear[[2]], 
                     data_poly[[2]], data_gam[[2]])
  data_combined_plot <- data_vasicek[[2]] + data_linear[[2]] + data_poly[[2]] + data_gam[[2]]
  
  return(list(numerical_results, data_combined_plot))
}