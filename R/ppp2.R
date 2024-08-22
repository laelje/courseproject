#' Build pretty barplots fast 
#' 
#' A function to produce barplots using ggplot2 with theme, colors, and text
#'
#' @param data a data frame
#' @param x_var a categorical variable in data
#' @param colors character vector of colors names for x_var
#'
#' @return A pretty colored barplot with counts printed on the plot
#' 
#' @import ggplot2
#' @import dplyr
#' @import ggpubr
#' 
#' @export
#'
#' @examples
#' my_colors <- c("#337495", "#002a33", "#2f5a69", "#143b44")
#' ppp_barplot(data = dplyr::starwars, x_var = "sex", colors = my_colors)
ppp_barplot <- function(data, x_var, colors) {
  data_filtered <- subset(data, !is.na(data[[x_var]]))
  ggplot(data_filtered, aes_string(x = x_var, fill = x_var)) +
    geom_bar(alpha = 0.5) +
    geom_text(stat = "count", aes(label = sprintf("%d (%.1f%%)", ..count.., (..count..)/sum(..count..) * 100)), vjust = -0.5) +
    labs(title = paste("Count of", x_var), 
         x = x_var,
         y = "count") +
    theme_minimal(base_size = 16) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors)
}