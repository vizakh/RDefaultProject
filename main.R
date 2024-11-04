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
library(tidyverse)

source("process_data.R")
source("visualize_data.R")
source("vasicek.R")
source("linear_trend_seasonality.R")
source("poly_trend_seasonality.R")
source("gen_add_model.R")
source("all_models.R")

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

last_date <- as.POSIXct('2025-12-31')
future_daily_dates <- data.frame("date" = seq(all_daily_data$date[nrow(all_daily_data)], 
                                              last_date, 
                                              by = 'days'))
future_monthly_dates <- data.frame("date" = seq(all_daily_data$date[nrow(all_daily_data)], 
                                                last_date, 
                                                by = 'months'))

set.seed(411)
all_daily <- all_models(all_daily_data, future_daily_dates, 
                        week, title = "по денно всі")
cor_daily <- all_models(cor_daily_data, future_daily_dates, 
                        week, title = "по денно КОР")
all_dates <- all_models(all_dates_data, future_monthly_dates,
                        month, title = "звітні дати всі")
cor_dates <- all_models(cor_dates_data, future_monthly_dates,
                        month, title = "звітні дати КОР")

OUT <- createWorkbook()

addWorksheet(OUT, "По денно всі")
addWorksheet(OUT, "По денно КОР")
addWorksheet(OUT, "Звітні дати всі")
addWorksheet(OUT, "Звітні дати КОР")

writeData(OUT, sheet = "По денно всі", x = numeric_all_daily_results)
writeData(OUT, sheet = "По денно КОР", x = numeric_cor_daily_results)
writeData(OUT, sheet = "Звітні дати всі", x = numeric_all_dates_results)
writeData(OUT, sheet = "Звітні дати КОР", x = numeric_cor_dates_results)

saveWorkbook(OUT, "F:/Pivdennyy/result_models_future.xlsx")

print(all_daily[[2]])
print(cor_daily[[2]])
print(all_dates[[2]])
print(cor_dates[[2]])
