#' Build pretty box plots fast 
#'
#' A function to produce box plots using ggplot2 with theme, colors, and statistics
#'
#' @param data data frame containing the variables to be plotted and analyzed
#' @param x_var categorical variable in the data frame that will be used for grouping on the x-axis
#' @param y_var numeric variable in the data frame that will be plotted on the y-axis
#' @param comparisons list specifying the pairs of categories in x_var to be compared statistically
#' @param colors character vector of color names, where the number of colors provided must match the number of unique categories in the x_var variable
#' @param method statistical method used for comparing the groups specified in comparisons (e.g., "wilcox.test", "t.test")
#'
#' @return visually appealing, colored box plot of y_var grouped by x_var, with statistical comparisons of the specified groups (comparisons) displayed on the plot, using the specified method. The plot is customized with colors from the provided colors vector
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
#' ppp_boxplot(data = iris, 
#'             x_var = "Species", 
#'             y_var = "Sepal.Length", 
#'             comparisons = list(c("setosa", "versicolor"), c("setosa", "virginica")),
#'             colors = c("#337495", "#002a33", "#2f5a69"),
#'             method = "wilcox.test")
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