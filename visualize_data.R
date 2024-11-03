visualize_data <- function(data, data_legend_name, data_labs, legend_pos) {
  ggplot(data, aes(x = Date)) +
    geom_line(aes(y = number_sold, color = data_legend_name)) +
    labs(title = data_labs[1],
         x = data_labs[2],
         y = data_labs[3]) +
    scale_color_manual(values = c("black")) +
    theme(legend.position = legend_pos,
          legend.background = element_rect(fill = "white", color = "black"),
          legend.title = element_blank())
}