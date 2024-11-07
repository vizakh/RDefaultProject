# Функція для вибірки стовбців даних з видаленням усіх інших. Приймає набір
# даних, індекс стовбця, який потрібно залишити у наборі та назву даного 
# стовбця.
process_data <- function(data, col_values_index, col_values_name) {
  
  # Залишаемо тільки стовбці з датами та введений у якості аргументу функції.
  data <- data[, c(1, col_values_index)]
  
  # За допомогою бібліотеки dplyr змінюємо назви стовбців.
  data <- data %>% 
    rename(
      date = "Дата, за яку сформані залишки",
      total = col_values_name
    )
  
  # Повертаємо змінений набір даних.
  return(data)
}

# Функція для розбиття набору даних на train/test компоненти. У якості аргмуентів
# приймає набір даних та параметр відносного розбиття даних.
create_train_test <- function(data, fraction) {
  
  # Знаходимо граничний запис у наборі, що буде останнім записом у train.
  border <- round(nrow(data) * fraction)
  
  # Створюємо train та test набори.
  train <- data[1:border,]
  test <- data[(border + 1):nrow(data),]
  
  # Повертаємо список з наборів train та test.
  return(list(train, test))
}

# Функція для нормалізації даних від 0 до 1.
normalize_data <- function(data) {
  
  # Створюємо процес нормалізації для змінної 'total' з набору даних,
  # використовуючи метод нормалізації по діапазону значень.
  process <- preProcess(as.data.frame(data$total), method=c("range"))
  
  # Застосовуємо процес нормалізації даних. Функція predict використовує 
  # створений процес для трансформації даних.
  norm_scale <- predict(process, as.data.frame(data$total))
  
  # Замінюємо значення змінної 'total' у наборі даних.
  data$total <- norm_scale$`data$total`
  
  # Повертаємо змінений набір.
  return(data)
}