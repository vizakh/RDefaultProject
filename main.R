library(dplyr)
library(lubridate)
library(xts)
library(ggplot2)
library(mgcv)
library(forecast)
library(RODBC)
library(gsubfn)

source("process_data.R")
source("visualize_data.R")
source("vasicek.R")
source("linear_trend_seasonality.R")
source("poly_trend_seasonality.R")
source("gen_add_model.R")
source("ets_model.R")

wb <- "F:/Pivdennyy/stat.xlsb"
con2 <- odbcConnectExcel2007(wb)
all_daily_data <- sqlFetch(con2, "По денно всі")
cor_daily_data <- sqlFetch(con2, "По денно КОР")
all_dates_data <- sqlFetch(con2, "Звітні дати всі")
cor_dates_data <- sqlFetch(con2, "Звітні дати КОР")

all_daily_data <- process_daily_data(all_daily_data)
cor_daily_data <- process_daily_data(cor_daily_data)
all_dates_data <- process_daily_data(all_dates_data)
cor_dates_data <- process_daily_data(cor_dates_data)

visualize_data(all_daily_data, "По денно всі", 
               c("По денно всі", "Дата", "Підсумок"),
               c(0.87, 0.07))
visualize_data(cor_daily_data, "По денно КОР", 
               c("По денно КОР", "Дата", "Підсумок"),
               c(0.87, 0.07))
visualize_data(all_dates_data, "Звітні дати всі", 
               c("Звітні дати всі", "Дата", "Підсумок"),
               c(0.87, 0.07))
visualize_data(cor_dates_data, "Звітні дати КОР", 
               c("Звітні дати КОР", "Дата", "Підсумок"),
               c(0.87, 0.07))

train_test_set <- create_train_test(all_daily_data)
all_daily_train <- train_test_set[[1]]
all_daily_test <- train_test_set[[2]]

# Модель Васічека---------------------------------------------------------------------------------
set.seed(411)
VasicekModel(all_daily_train, all_daily_test, 
             title = "Модель Васічека", legend_pos = c(0.16, 0.88))

# Лінійна модель з трендом та сезонністю----------------------------------------------------------
linear_trend_seasonality(all_daily_train, all_daily_test, week, 
                         title = "Лінійна регресія з трендом та сезонністю", 
                         legend_pos = c(0.2, 0.85))

# Поліноміальна модель з трендом та сезонністю----------------------------------------------------
poly_trend_seasonality(all_daily_train, all_daily_test, week, 
                       title = "Поліноміальна регресія з трендом та сезонністю", 
                       legend_pos = c(0.2, 0.85))

# General Additive Model (GAM)--------------------------------------------------------------------
gen_add_model(all_daily_train, all_daily_test, week,
              title = "GAM модель",
              legend_pos = c(0.2, 0.85))

