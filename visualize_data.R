# Функція для візуалізації загального набору даних. Приймає в якості аргументів:
# data - набір даних
# data_legend_name - назва набору для легенди на графіку
# data_labs - масив з 3 значень: назви графіку та 2 вісей
# legend_pos - масив з 2 цілочисельних значень для задання координат легенди 
#              на графіку
visualize_data <- function(data, data_legend_name, data_labs, legend_pos) {
  ggplot(data, aes(x = date)) +
    geom_line(aes(y = total, color = data_legend_name)) +
    labs(title = data_labs[1],
         x = data_labs[2],
         y = data_labs[3]) +
    scale_color_manual(values = c("black")) +
    theme(legend.position = legend_pos,
          legend.background = element_rect(fill = "white", color = "black"),
          legend.title = element_blank())
}