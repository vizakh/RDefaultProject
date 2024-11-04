ets_model <- function(data, test_data, frequency) {
  ts_data <- ts(data$number_sold, frequency = frequency)
  
  ets_model <- ets(ts_data)
  print(summary(ets_model))
  
  ets_forecast <- forecast(ets_model, h = nrow(test_data))
  
  print(ets_forecast)
  plot(ets_forecast)
}