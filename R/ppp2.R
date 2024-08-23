#' Build pretty bar plots fast 
#' 
#' A function to produce bar plots using ggplot2 with theme, colors, and counts (percentages)
#'
#' @param data data frame
#' @param x_svar categorical variable in data
#' @param colors character vector of color names of same length as x_var
#'
#' @return A pretty colored bar plot with counts and percentages printed on the plot
#' 
#' @import ggplot2
#' @import dplyr
#' @import ggpubr
#' 
#' @export
#'
#' @examples
#' my_colors <- c("#E09F3E", "#9E2A2B", "#540B0E", "#335C67")
#' ppp_barplot(data = dplyr::starwars, x_var = "sex", colors = my_colors)
#' ppp_barplot(data = iris, x_var = "Species", colors = c("#337495", "#002a33", "#2f5a69"))
ppp_barplot <- function(data, x_var, colors) {
  data_filtered <- subset(data, !is.na(data[[x_var]]))
  ggplot(data_filtered, aes(x = !!sym(x_var), fill = !!sym(x_var))) +
    geom_bar() +
    geom_text(stat = "count", aes(label = sprintf("%d (%.1f%%)", after_stat(count), (after_stat(count))/sum(after_stat(count)) * 100), vjust = -0.5)) +
    labs(title = paste("Count of", x_var), 
         x = x_var,
         y = "count") +
    theme_minimal(base_size = 16) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors)
}