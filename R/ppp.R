#' Build pretty box plots fast 
#'
#' A function to produce box plots using ggplot2 with theme, colors, and statistics
#'
#' @param data data frame
#' @param x_var categorical variable in data
#' @param y_var numeric variable in data
#' @param comparisons list of groups to compare
#' @param colors character vector of color names of same length as x_var
#' @param method statistical method
#'
#' @return A pretty colored box plot with stats printed on the plot
#'
#' @import ggplot2
#' @import dplyr
#' @import ggpubr
#' 
#' @export
#'
#' @examples
#' my_comparisons <- list(c("female", "hermaphroditic"), c("female", "male"), c("female", "none"))
#' my_colors <- c("#E09F3E", "#9E2A2B", "#540B0E", "#335C67")
#' my_method <- "wilcox.test"
#' ppp_boxplot(data = dplyr::starwars,
#'             x_var = "sex", 
#'             y_var = "height", 
#'             comparisons = my_comparisons, 
#'             colors = my_colors, 
#'             method = my_method)
ppp_boxplot <- function(data, x_var, y_var, comparisons, colors, method) {
  data_filtered <- subset(data, !is.na(data[[x_var]]))
  ggplot(data_filtered, aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(x_var))) +
    geom_boxplot(outlier.shape = NA, alpha=0.8) +
    geom_jitter(aes(color = !!sym(x_var)), show.legend = FALSE) +
    labs(title = paste("Plot of", y_var, "by", x_var), 
         x = x_var,
         y = y_var) +
    theme_minimal(base_size = 16) +
    stat_compare_means(method = method, comparisons = comparisons) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors)
}




