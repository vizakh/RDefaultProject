ets_model <- function(data, h, title, legend_pos) {
  model_ets <- ets(data, model = "MAA")
  
  forecast_ets <- forecast(model_ets, h = h)
  str(forecast_ets)
  
  plot(forecast_ets)
}