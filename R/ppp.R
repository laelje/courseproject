#' Build pretty boxplots fast 
#'
#' A function to produce boxplots using ggplot2 with theme, colors, and statistics
#'
#' @param data a data frame
#' @param x_var a categorical variable in data
#' @param y_var a numeric variable in data
#' @param comparisons list of groups to compare
#' @param colors character vector of colors names for x_var
#' @param method statistical method
#'
#' @return A pretty colored boxplot with stats printed on the plot
#'
#' @import ggplot2
#' @import dplyr
#' @import ggpubr
#' 
#' @export
#'
#' @examples
#' my_comparisons <- list(c("female", "hermaphroditic"), c("female", "male"), c("female", "none"))
#' my_colors <- c("#337495", "#002a33", "#2f5a69", "#143b44")
#' my_method = "wilcox.test"
#' ppp_boxplot(data = starwars,
#'             x_var = "sex", 
#'             y_var = "height", 
#'             comparisons = my_comparisons, 
#'             colors = my_colors, 
#'             method = my_method)
ppp_boxplot <- function(data, x_var, y_var, comparisons, colors, method) {
  data_filtered <- subset(data, !is.na(data[[x_var]]))
  ggplot(data_filtered, aes_string(x = x_var, y = y_var, fill = x_var)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.5) +
    geom_jitter(aes_string(color = x_var), alpha = 0.7, show.legend = FALSE) +
    labs(title = paste("Plot of", y_var, "by", x_var), 
         x = x_var,
         y = y_var) +
    theme_minimal(base_size = 16) +
    stat_compare_means(method = method, comparisons = comparisons) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors)
}







