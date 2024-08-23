test_that("ppp_barplot works with another data set and produces plot", {
  plot <- ppp_barplot(data = iris, 
                      x_var = "Species", 
                      colors = c("#337495", "#002a33", "#2f5a69"))
  expect_s3_class(plot, "ggplot")
})
