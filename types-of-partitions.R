library(ggplot2)
library(gridExtra)

quadratic_function <- function(x) {
  return(4 - 9/10*x^2)
}

x_vals <- seq(-0.5, 2.5, length.out = 100)
y_vals <- quadratic_function(x_vals)
curve_data <- data.frame(x = x_vals, y = y_vals)

create_bar_data <- function(x_points) {
  left_x <- x_points[-length(x_points)]
  right_x <- x_points[-1]
  heights <- quadratic_function(left_x)
  
  data.frame(xmin = left_x, xmax = right_x, ymin = 0, ymax = heights)
}

uniform_x <- seq(0, 2, length.out = 4)
nonuniform_x <- c(0, 0.7, 0.9, 2)
uniform_bars <- create_bar_data(uniform_x)
nonuniform_bars <- create_bar_data(nonuniform_x)


create_axis_plot <- function(title_text, bars, aspect_ratio = 1) {
  shaded_curve_data <- subset(curve_data, x >= 0 & x <= 2)
  
  ggplot() +
    geom_segment(aes(x = -0.5, xend = 2.5, y = 0, yend = 0), 
                 arrow = arrow(length = unit(0.2, "inches")), linewidth = 1.5) +
    geom_segment(aes(x = 0, xend = 0, y = -0.5, yend = 4.5), 
                 arrow = arrow(length = unit(0.2, "inches")), linewidth = 1.5) +
    
    annotate("text", x = 2.6, y = -0.2, label = "x", size = 8, fontface = "bold") +
    annotate("text", x = -0.2, y = 4.5, label = "y", size = 8, fontface = "bold") +
    
    
    geom_rect(data = bars, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              fill = "purple", color = "darkorchid4", alpha = 0.3) +
    
    geom_line(data = curve_data, aes(x = x, y = y), color = "blue", linewidth = 2) +
    
    geom_ribbon(data = shaded_curve_data, aes(x = x, ymin = 0, ymax = y), 
                fill = "darkblue", alpha = 0.3) +
    
    xlim(-0.5, 2.7) + ylim(-0.5, 4.5) +

    coord_fixed(ratio = aspect_ratio) +
    ggtitle(title_text) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
          )
}

p1 <- create_axis_plot("Uniform subdivisions", uniform_bars, aspect_ratio = 0.5)
p2 <- create_axis_plot("Nonuniform subdivisions", nonuniform_bars, aspect_ratio = 0.5)

grid.arrange(p1, p2, ncol = 2)
