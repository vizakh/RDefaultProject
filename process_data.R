process_data <- function(data, col_values_index, col_values_name) {
  data <- data[, c(1, col_values_index)]
  data <- data %>% 
    rename(
      date = "Дата, за яку сформані залишки",
      total = col_values_name
    )
  return(data)
}

create_train_test <- function(data, fraction) {
  border <- round(nrow(data) * fraction)
  train <- data[1:border,]
  test <- data[(border + 1):nrow(data),]
  return(list(train, test))
}