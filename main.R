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

source("process_data.R")
source("visualize_data.R")
source("vasicek.R")
source("linear_trend_seasonality.R")
source("regularization_models.R")
source("gen_add_model.R")
source("all_models.R")

wb <- "F:/Pivdennyy/stat.xlsb"
con2 <- odbcConnectExcel2007(wb)
all_daily_data <- sqlFetch(con2, "По денно всі")
cor_daily_data <- sqlFetch(con2, "По денно КОР")
all_dates_data <- sqlFetch(con2, "Звітні дати всі")
cor_dates_data <- sqlFetch(con2, "Звітні дати КОР")

col_values_total <- list(4, "Загальний підсумок")
col_values_FCY <- list(2, "FCY")
col_values_UAH <- list(3, "UAH")
col_values_selected <- col_values_total

all_daily_data <- process_data(all_daily_data, 
                               col_values_selected[[1]], col_values_selected[[2]])
cor_daily_data <- process_data(cor_daily_data, 
                               col_values_selected[[1]], col_values_selected[[2]])
all_dates_data <- process_data(all_dates_data, 
                               col_values_selected[[1]], col_values_selected[[2]])
cor_dates_data <- process_data(cor_dates_data, 
                               col_values_selected[[1]], col_values_selected[[2]])

# all_daily_data <- normalize_data(all_daily_data)
# cor_daily_data <- normalize_data(cor_daily_data)
# all_dates_data <- normalize_data(all_dates_data)
# cor_dates_data <- normalize_data(cor_dates_data)

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

fraction <- 0.9
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

last_date <- as.POSIXct('2026-01-01')
future_daily_dates <- data.frame("date" = seq(all_daily_data$date[nrow(all_daily_data)], 
                                              last_date, 
                                              by = 'days'))
future_daily_dates <- future_daily_dates[-1,,drop=F]

future_monthly_dates <- data.frame("date" = seq(all_dates_data$date[nrow(all_dates_data)], 
                                                last_date, 
                                                by = 'months'))
future_monthly_dates <- future_monthly_dates[-1,,drop=F]

future_daily_dates$total <- 0
future_monthly_dates$total <- 0

all_daily_test <- rbind(all_daily_test, future_daily_dates)
cor_daily_test <- rbind(cor_daily_test, future_daily_dates)
all_dates_test <- rbind(all_dates_test, future_monthly_dates)
cor_dates_test <- rbind(cor_dates_test, future_monthly_dates)

set.seed(411)
all_daily <- all_models(all_daily_train, all_daily_test, future_daily_dates, 
                        week, title = "по денно всі")
cor_daily <- all_models(cor_daily_train, cor_daily_test, future_daily_dates, 
                        week, title = "по денно КОР")
all_dates <- all_models(all_dates_train, all_dates_test, future_monthly_dates, 
                        month, title = "звітні дати всі")
cor_dates <- all_models(cor_dates_train, cor_dates_test, future_monthly_dates, 
                        month, title = "звітні дати КОР")

OUT <- createWorkbook()

addWorksheet(OUT, "По денно всі")
addWorksheet(OUT, "По денно КОР")
addWorksheet(OUT, "Звітні дати всі")
addWorksheet(OUT, "Звітні дати КОР")

writeData(OUT, sheet = "По денно всі", x = all_daily[[1]])
writeData(OUT, sheet = "По денно КОР", x = cor_daily[[1]])
writeData(OUT, sheet = "Звітні дати всі", x = all_dates[[1]])
writeData(OUT, sheet = "Звітні дати КОР", x = cor_dates[[1]])

# saveWorkbook(OUT, "F:/Pivdennyy/result_models_future_FCY.xlsx")

print(all_daily[[2]])
print(cor_daily[[2]])
print(all_dates[[2]])
print(cor_dates[[2]])
