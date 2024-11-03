process_daily_data <- function(data, limit_dates) {
  data <- data[, -c(2, 3)]
  
  data <- data %>%
    mutate(Date = ymd(Date)) %>%
    group_by(Date) %>%
    summarise(number_sold = sum(number_sold))
  
  if (missing(limit_dates)) {
    limit_dates <- c(min(data$Date), max(data$Date))
  }
  data <- data[(data$Date >= limit_dates[1] & data$Date <= limit_dates[2]),]
  
  return(data)
}

process_monthly_data <- function(data, limit_dates) {
  data <- data[, -c(2, 3)]
  
  data <- data %>%
    mutate(Date = ymd(Date)) %>%
    group_by(Date = floor_date(Date, 'month')) %>%
    summarise(number_sold = sum(number_sold))
  
  if (missing(limit_dates)) {
    limit_dates <- c(min(data$Date), max(data$Date))
  }
  data <- data[(data$Date >= limit_dates[1] & data$Date <= limit_dates[2]),]
  
  return(data)
}