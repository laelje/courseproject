test_that("ppp_boxplot works with another data set and produces plot", {
  plot <- ppp_boxplot(data = iris, 
                       x_var = "Species", 
                       y_var = "Sepal.Length", 
                       comparisons = list(c("setosa", "versicolor"), c("setosa", "virginica")),
                       colors = c("#337495", "#002a33", "#2f5a69"),
                       method = "wilcox.test")
  expect_s3_class(plot, "ggplot")
})
