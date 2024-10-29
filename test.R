library(magrittr)
library(dplyr)
library(lubridate)
library(ggplot2)

data <- read.table('F:/Pivdennyy/train.csv', header = TRUE, sep = ',',
                   colClasses = c("character", "NULL", "NULL", "numeric"))
print(data)

data$Date <- ymd(data$Date)
str(data)

data <- data %>%
  group_by(Date) %>%
  summarise(number_sold = sum(number_sold))

ggplot(data, aes(x = Date, y = number_sold)) +
  geom_line() + scale_x_date(date_labels = "%m-%Y")
print(data)

data = data[!(data$Date < as.POSIXct("2016-01-01")),]
ggplot(data, aes(x = Date, y = number_sold)) +
  geom_line() + scale_x_date(date_labels = "%m-%Y")


