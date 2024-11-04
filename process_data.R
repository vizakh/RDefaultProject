process_daily_data <- function(data) {
  data <- data[, c(1, 4)]
  data <- data %>% 
    rename(
      date = "Дата, за яку сформані залишки",
      total = "Загальний підсумок"
    )
  return(data)
}

process_monthly_data <- function(data) {
  data <- data[, c(1, 4)]
  data <- data %>% 
    rename(
      date = "Дата, за яку сформані залишки",
      total = "Загальний підсумок"
    ) %>%
    group_by(date = floor_date(date, 'month'))
  return(data)
}