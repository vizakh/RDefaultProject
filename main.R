# Підключаємо необхідні бібліотеки.
library(dplyr)
library(lubridate)
library(xts)
library(ggplot2)
library(mgcv)
library(forecast)
library(RODBC)
library(gsubfn)
library(patchwork)
library(openxlsx)
library(caret)
library(glmnet)

# Підключаємо необхідні файли для доступу до написаних у них функцій:
source("process_data.R")             # Файл з функціями для обробки даних
source("visualize_data.R")           # Файл з функцією для візуалізації загальних даних
source("vasicek.R")                  # Файл з функціями для роботи з моделлю Васічека
source("linear_trend_seasonality.R") # Файл з функцією для моделі лінійної регресії
source("regularization_models.R")    # Файл з функцією для моделей з регулярізацією Ridge та Lasso
source("gen_add_model.R")            # Файл з функцією для моделі GAM
source("all_models.R")               # Файл з функцією для роботи з усіма моделями

# Записуємо дані у змінні з кожного аркуша файлу з даними.
wb <- "F:/Pivdennyy/stat.xlsb" # Шлях до файлу з даними
con2 <- odbcConnectExcel2007(wb)
all_daily_data <- sqlFetch(con2, "По денно всі")
cor_daily_data <- sqlFetch(con2, "По денно КОР")
all_dates_data <- sqlFetch(con2, "Звітні дати всі")
cor_dates_data <- sqlFetch(con2, "Звітні дати КОР")

# Змінні зі списком із номера стовбця та його назви у файлі Excel, що 
# залишиться у змінних.
col_values_total <- list(4, "Загальний підсумок")
col_values_FCY <- list(2, "FCY")
col_values_UAH <- list(3, "UAH")
col_values_selected <- col_values_total # Записуємо необхідний для нас список

# Обробка даних, щоб залишити необхідні стовбці та змінити їх назви.
all_daily_data <- process_data(all_daily_data, 
                               col_values_selected[[1]], col_values_selected[[2]])
cor_daily_data <- process_data(cor_daily_data, 
                               col_values_selected[[1]], col_values_selected[[2]])
all_dates_data <- process_data(all_dates_data, 
                               col_values_selected[[1]], col_values_selected[[2]])
cor_dates_data <- process_data(cor_dates_data, 
                               col_values_selected[[1]], col_values_selected[[2]])

# Нормалізація даних від 0 до 1 (за умовчуванням закоментовані).
# all_daily_data <- normalize_data(all_daily_data)
# cor_daily_data <- normalize_data(cor_daily_data)
# all_dates_data <- normalize_data(all_dates_data)
# cor_dates_data <- normalize_data(cor_dates_data)

# Візуалізація отриманих даних окремо для даних з кожного аркуша.
visualize_data(all_daily_data, "По денно всі", 
               c("По денно всі", "Дата",  col_values_selected[[2]]),
               c(0.87, 0.07))
visualize_data(cor_daily_data, "По денно КОР", 
               c("По денно КОР", "Дата",  col_values_selected[[2]]),
               c(0.87, 0.07))
visualize_data(all_dates_data, "Звітні дати всі", 
               c("Звітні дати всі", "Дата",  col_values_selected[[2]]),
               c(0.87, 0.07))
visualize_data(cor_dates_data, "Звітні дати КОР", 
               c("Звітні дати КОР", "Дата",  col_values_selected[[2]]),
               c(0.87, 0.07))

# Змінна, що визначає відношення розміру train до всього набору даних, 
# за умовчуванням 0.8 (80%).
fraction <- 0.8 
# Поділ кожного набору даних на train (на якому будуватиметься модель) 
# та test (на якому робиться прогноз).
train_test_set <- create_train_test(all_daily_data, fraction)
all_daily_train <- train_test_set[[1]]
all_daily_test <- train_test_set[[2]]

train_test_set <- create_train_test(cor_daily_data, fraction)
cor_daily_train <- train_test_set[[1]]
cor_daily_test <- train_test_set[[2]]

train_test_set <- create_train_test(all_dates_data, fraction)
all_dates_train <- train_test_set[[1]]
all_dates_test <- train_test_set[[2]]

train_test_set <- create_train_test(cor_dates_data, fraction)
cor_dates_train <- train_test_set[[1]]
cor_dates_test <- train_test_set[[2]]

# Визначення останньої дати, до якої робиться прогноз (виключно)
# (має бути пізніше дат тестового набору).
last_date <- as.POSIXct('2026-01-01')

# Створюються набори даних з майбутніми (до last_date) датами 
# та значеннями прогнозу (за замовченням 0).

# Щоденні майбутні дати.
future_daily_dates <- data.frame("date" = seq(all_daily_data$date[nrow(all_daily_data)], 
                                              last_date, 
                                              by = 'days'))
future_daily_dates <- future_daily_dates[-1,,drop=F]

# Щомісячні майбутні дати.
future_monthly_dates <- data.frame("date" = seq(all_dates_data$date[nrow(all_dates_data)], 
                                                last_date, 
                                                by = 'months'))
future_monthly_dates <- future_monthly_dates[-1,,drop=F]

# Записуємо значення прогнозу за замовчуванням.
future_daily_dates$total <- 0
future_monthly_dates$total <- 0

# Об'єднуємо тестові дані з майбутніми даними.
# Це робиться, щоб зробити прогноз на тестові дані та майбутні дані разом.
# Значення прогнозу за замовчуванням вище не використовується у подальшому.
# Воно було необхідно тільки для з'єднання тестових даних з майбутніми датами.
all_daily_test <- rbind(all_daily_test, future_daily_dates)
cor_daily_test <- rbind(cor_daily_test, future_daily_dates)
all_dates_test <- rbind(all_dates_test, future_monthly_dates)
cor_dates_test <- rbind(cor_dates_test, future_monthly_dates)

# Фіксуємо seed для фіксації випадкових дій. Необхідно для отримання одного 
# й того ж результату моделі Васічека.
set.seed(411)

# Будуємо усі моделі та робимо прогноз для кожного набору даних.
all_daily <- all_models(all_daily_train, all_daily_test, future_daily_dates, 
                        week, title = "по денно всі")
cor_daily <- all_models(cor_daily_train, cor_daily_test, future_daily_dates, 
                        week, title = "по денно КОР")
all_dates <- all_models(all_dates_train, all_dates_test, future_monthly_dates, 
                        month, title = "звітні дати всі")
cor_dates <- all_models(cor_dates_train, cor_dates_test, future_monthly_dates, 
                        month, title = "звітні дати КОР")

# Формуємо Workbook, в який записуються усі обчислені дані щодо прогнозів.
# Прогноз щодо кожного набору даних зберігається на окремому аркуші.
# У кінці сформования Workbook зберігається за записаним шляхом у 
# вигляді .xlsx файлу.
OUT <- createWorkbook()

addWorksheet(OUT, "По денно всі")
addWorksheet(OUT, "По денно КОР")
addWorksheet(OUT, "Звітні дати всі")
addWorksheet(OUT, "Звітні дати КОР")

writeData(OUT, sheet = "По денно всі", x = all_daily[[1]])
writeData(OUT, sheet = "По денно КОР", x = cor_daily[[1]])
writeData(OUT, sheet = "Звітні дати всі", x = all_dates[[1]])
writeData(OUT, sheet = "Звітні дати КОР", x = cor_dates[[1]])

# saveWorkbook(OUT, "F:/Pivdennyy/result_models_future_ridge_lasso.xlsx")

# Візуалізація усіх результатів для кожного набору даних на окремих малюнках.
print(all_daily[[2]])
print(cor_daily[[2]])
print(all_dates[[2]])
print(cor_dates[[2]])
