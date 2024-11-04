process_daily_data <- function(data) {
  data <- data[, c(1, 4)]
  data <- data %>% 
    rename(
      date = "Дата, за яку сформані залишки",
      total = "Загальний підсумок"
    )
  return(data)
}

create_train_test <- function(data, fraction = 0.9) {
  border <- round(nrow(data) * 0.9)
  train <- data[1:border,]
  test <- data[(border + 1):nrow(data),]
  return(list(train, test))
}

# process_monthly_data <- function(data) {
#   data <- data[, c(1, 4)]
#   data <- data %>% 
#     rename(
#       date = "Дата, за яку сформані залишки",
#       total = "Загальний підсумок"
#     ) %>%
#     group_by(date = floor_date(date, 'month'))
#   return(data)
# }