# Based on the 'entropy chart' found in Chapter 3 of Data Science for Business
# Segments should be a list leaf nodes containing the target variable for each segment
# This versions computes the 'numeric entropy' using the variance function
entropy_chart <- function(segments, y.height = 1) {
  seg_n <- sapply(segments, length)
  seg_proportion <- seg_n / sum(seg_n)
  seg_var <- sapply(segments, var)
  seg_var[seg_n == 0] <- 0

  # Adjust the y.height if necessary 
  # if (max(seg_var) > y.height) y.height <- round(max(seg_var) + .5, 0)

  barplot(height = seg_var, width = seg_proportion, space = 0, ylim=c(0, y.height))
}

