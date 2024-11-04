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

source("process_data.R")
source("visualize_data.R")
source("vasicek.R")
source("linear_trend_seasonality.R")
source("poly_trend_seasonality.R")
source("gen_add_model.R")

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

# visualize_data(all_daily_data, "По денно всі", 
#                c("По денно всі", "Дата", "Підсумок"),
#                c(0.87, 0.07))
# visualize_data(cor_daily_data, "По денно КОР", 
#                c("По денно КОР", "Дата", "Підсумок"),
#                c(0.87, 0.07))
# visualize_data(all_dates_data, "Звітні дати всі", 
#                c("Звітні дати всі", "Дата", "Підсумок"),
#                c(0.87, 0.07))
# visualize_data(cor_dates_data, "Звітні дати КОР", 
#                c("Звітні дати КОР", "Дата", "Підсумок"),
#                c(0.87, 0.07))

train_test_set <- create_train_test(all_daily_data, 1)
all_daily_train <- train_test_set[[1]]
all_daily_test <- train_test_set[[2]]

train_test_set <- create_train_test(cor_daily_data, 1)
cor_daily_train <- train_test_set[[1]]
cor_daily_test <- train_test_set[[2]]

train_test_set <- create_train_test(all_dates_data, 1)
all_dates_train <- train_test_set[[1]]
all_dates_test <- train_test_set[[2]]

train_test_set <- create_train_test(cor_dates_data, 1)
cor_dates_train <- train_test_set[[1]]
cor_dates_test <- train_test_set[[2]]

future_daily_dates <- data.frame("date" = seq(all_daily_data$date[nrow(all_daily_data)], 
                                     as.POSIXct('2025-12-31'), 
                                     by = 'days'))
future_monthly_dates <- data.frame("date" = seq(all_daily_data$date[nrow(all_daily_data)], 
                                       as.POSIXct('2025-12-31'), 
                                       by = 'months'))

# Модель Васічека---------------------------------------------------------------------------------
set.seed(411)
all_daily_vasicek <- VasicekModel(all_daily_data, future_daily_dates, 
             title = "Модель Васічека (по денно всі)", legend_pos = c(0.2, 0.85))
cor_daily_vasicek <- VasicekModel(cor_daily_data, future_daily_dates, 
             title = "Модель Васічека (по денно КОР)", legend_pos = c(0.2, 0.85))
all_dates_vasicek <- VasicekModel(all_dates_data, future_monthly_dates, 
             title = "Модель Васічека (Звітні дати всі)", legend_pos = c(0.2, 0.85))
cor_dates_vasicek <- VasicekModel(cor_dates_data, future_monthly_dates, 
             title = "Модель Васічека (Звітні дати КОР)", legend_pos = c(0.2, 0.85))

all_daily_vasicek_plot <- all_daily_vasicek[[2]]
all_daily_vasicek_values <- all_daily_vasicek[[1]]
all_daily_vasicek_dates <- all_daily_vasicek[[3]]

cor_daily_vasicek_plot <- cor_daily_vasicek[[2]]
cor_daily_vasicek_values <- cor_daily_vasicek[[1]]
cor_daily_vasicek_dates <- cor_daily_vasicek[[3]]

all_dates_vasicek_plot <- all_dates_vasicek[[2]]
all_dates_vasicek_values <- all_dates_vasicek[[1]]
all_dates_vasicek_dates <- all_dates_vasicek[[3]]

cor_dates_vasicek_plot <- cor_dates_vasicek[[2]]
cor_dates_vasicek_values <- cor_dates_vasicek[[1]]
cor_dates_vasicek_dates <- cor_dates_vasicek[[3]]

# Лінійна модель з трендом та сезонністю----------------------------------------------------------
all_daily_linear <- linear_trend_seasonality(all_daily_data, future_daily_dates, week, 
                          title = "Лінійна регресія з трендом та сезонністю (по денно всі)", 
                          legend_pos = c(0.2, 0.85))
cor_daily_linear <- linear_trend_seasonality(cor_daily_data, future_daily_dates, week, 
                          title = "Лінійна регресія з трендом та сезонністю (по денно КОР)", 
                          legend_pos = c(0.2, 0.85))
all_dates_linear <- linear_trend_seasonality(all_dates_data, future_monthly_dates, month, 
                          title = "Лінійна регресія з трендом та сезонністю (Звітні дати всі)", 
                          legend_pos = c(0.2, 0.85))
cor_dates_linear <- linear_trend_seasonality(cor_dates_data, future_monthly_dates, month, 
                          title = "Лінійна регресія з трендом та сезонністю (Звітні дати КОР)", 
                          legend_pos = c(0.2, 0.85))

all_daily_linear_plot <- all_daily_linear[[2]]
all_daily_linear_values <- all_daily_linear[[1]]
all_daily_linear_dates <- all_daily_linear[[3]]

cor_daily_linear_plot <- cor_daily_linear[[2]]
cor_daily_linear_values <- cor_daily_linear[[1]]
cor_daily_linear_dates <- cor_daily_linear[[3]]

all_dates_linear_plot <- all_dates_linear[[2]]
all_dates_linear_values <- all_dates_linear[[1]]
all_dates_linear_dates <- all_dates_linear[[3]]

cor_dates_linear_plot <- cor_dates_linear[[2]]
cor_dates_linear_values <- cor_dates_linear[[1]]
cor_dates_linear_dates <- cor_dates_linear[[3]]

# Поліноміальна модель з трендом та сезонністю----------------------------------------------------
all_daily_poly <- poly_trend_seasonality(all_daily_data, future_daily_dates, week, 
                         title = "Поліноміальна регресія з трендом та сезонністю (по денно всі)", 
                         legend_pos = c(0.2, 0.85))
cor_daily_poly <- poly_trend_seasonality(cor_daily_data, future_daily_dates, week, 
                         title = "Поліноміальна регресія з трендом та сезонністю (по денно КОР)", 
                         legend_pos = c(0.2, 0.85))
all_dates_poly <- poly_trend_seasonality(all_dates_data, future_monthly_dates, month, 
                         title = "Поліноміальна регресія з трендом та сезонністю (Звітні дати всі)", 
                         legend_pos = c(0.2, 0.85))
cor_dates_poly <- poly_trend_seasonality(cor_dates_data, future_monthly_dates, month, 
                         title = "Поліноміальна регресія з трендом та сезонністю (Звітні дати КОР)", 
                         legend_pos = c(0.2, 0.85))

all_daily_poly_plot <- all_daily_poly[[2]]
all_daily_poly_values <- all_daily_poly[[1]]
all_daily_poly_dates <- all_daily_poly[[3]]

cor_daily_poly_plot <- cor_daily_poly[[2]]
cor_daily_poly_values <- cor_daily_poly[[1]]
cor_daily_poly_dates <- cor_daily_poly[[3]]

all_dates_poly_plot <- all_dates_poly[[2]]
all_dates_poly_values <- all_dates_poly[[1]]
all_dates_poly_dates <- all_dates_poly[[3]]

cor_dates_poly_plot <- cor_dates_poly[[2]]
cor_dates_poly_values <- cor_dates_poly[[1]]
cor_dates_poly_dates <- cor_dates_poly[[3]]

# General Additive Model (GAM)--------------------------------------------------------------------
all_daily_gam <- gen_add_model(all_daily_data, future_daily_dates, week,
                        title = "GAM модель (по денно всі)",
                        legend_pos = c(0.2, 0.85))
cor_daily_gam <- gen_add_model(cor_daily_data, future_daily_dates, week, 
                        title = "GAM модель (по денно КОР)", 
                        legend_pos = c(0.2, 0.85))
all_dates_gam <- gen_add_model(all_dates_data, future_monthly_dates, month, 
                        title = "GAM модель (Звітні дати всі)", 
                        legend_pos = c(0.2, 0.85))
cor_dates_gam <- gen_add_model(cor_dates_data, future_monthly_dates, month, 
                        title = "GAM модель (Звітні дати КОР)", 
                        legend_pos = c(0.2, 0.85))

all_daily_gam_plot <- all_daily_gam[[2]]
all_daily_gam_values <- all_daily_gam[[1]]
all_daily_gam_dates <- all_daily_gam[[3]]

cor_daily_gam_plot <- cor_daily_gam[[2]]
cor_daily_gam_values <- cor_daily_gam[[1]]
cor_daily_gam_dates <- cor_daily_gam[[3]]

all_dates_gam_plot <- all_dates_gam[[2]]
all_dates_gam_values <- all_dates_gam[[1]]
all_dates_gam_dates <- all_dates_gam[[3]]

cor_dates_gam_plot <- cor_dates_gam[[2]]
cor_dates_gam_values <- cor_dates_gam[[1]]
cor_dates_gam_dates <- cor_dates_gam[[3]]

all_daily_values <- list(all_daily_vasicek_dates, all_daily_vasicek_values, 
                         all_daily_linear_values, 
                         all_daily_poly_values, all_daily_gam_values)
cor_daily_values <- list(cor_daily_vasicek_dates, cor_daily_vasicek_values,
                         cor_daily_linear_values, 
                         cor_daily_poly_values, cor_daily_gam_values)
all_dates_values <- list(all_dates_vasicek_dates, all_dates_vasicek_values,
                         all_dates_linear_values, 
                         all_dates_poly_values, all_dates_gam_values)
cor_dates_values <- list(cor_dates_vasicek_dates, cor_dates_vasicek_values,
                         cor_dates_linear_values, 
                         cor_dates_poly_values, cor_dates_gam_values)

numeric_all_daily_results <- data.frame("Дата" = all_daily_values[[1]],
                                        "Васічек" = all_daily_values[[2]], 
                                        "Лінійна" = all_daily_values[[3]],
                                        "Поліноміальна" = all_daily_values[[4]], 
                                        "GAM" = all_daily_values[[5]])
numeric_cor_daily_results <- data.frame("Дата" = cor_daily_values[[1]],
                                        "Васічек" = cor_daily_values[[2]], 
                                        "Лінійна" = cor_daily_values[[3]],
                                        "Поліноміальна" = cor_daily_values[[4]], 
                                        "GAM" = cor_daily_values[[5]])
numeric_all_dates_results <- data.frame("Дата" = all_dates_values[[1]],
                                        "Васічек" = all_dates_values[[2]], 
                                        "Лінійна" = all_dates_values[[3]],
                                        "Поліноміальна" = all_dates_values[[4]], 
                                        "GAM" = all_dates_values[[5]])
numeric_cor_dates_results <- data.frame("Дата" = cor_dates_values[[1]],
                                        "Васічек" = cor_dates_values[[2]], 
                                        "Лінійна" = cor_dates_values[[3]],
                                        "Поліноміальна" = cor_dates_values[[4]], 
                                        "GAM" = cor_dates_values[[5]])

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

all_daily_plots <- all_daily_vasicek_plot + all_daily_linear_plot + all_daily_poly_plot + all_daily_gam_plot
cor_daily_plots <- cor_daily_vasicek_plot + cor_daily_linear_plot + cor_daily_poly_plot + cor_daily_gam_plot
all_dates_plots <- all_dates_vasicek_plot + all_dates_linear_plot + all_dates_poly_plot + all_dates_gam_plot
cor_dates_plots <- cor_dates_vasicek_plot + cor_dates_linear_plot + cor_dates_poly_plot + cor_dates_gam_plot

print(all_daily_plots)
print(cor_daily_plots)
print(all_dates_plots)
print(cor_dates_plots)
