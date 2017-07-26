# Entropy function for a class of items where x is a vector of elements from a given class
# The vector x should only contain the target variable from a segmentation of the data
entropy <- function(x) {
  n <- length(unique(x))  # The number of classes in the current segment
  if (n == 1) return(0)   # This is a 'pure' segment
  tbl <- prop.table(table(x))
  -1 * sum(sapply(tbl, function(p) p * log(p, n)), na.rm = TRUE)
}

# Based on the 'entropy chart' found in Chapter 3 of Data Science for Business
# Segments is a list of leaf nodes containing _ONLY_ the target variable for each segment
#
# By default, 'numeric entropy' is computed using a version of the variance function 
# that returns 0 for segments of length 1
#
# Use the entropy function above to compute class entropy
entropy_chart <- function(segments, entropy_f = function(x) if (length(x) == 1) 0 else var(x), y.height = 1) {
  seg_len <- sapply(segments, length)
  if (any(seg_len == 0)) {
    warning("Empty segments have been removed from the entropy chart function")
    segments <- segments[seg_len > 0]
    seg_len <- seg_len[seg_len > 0]
  }
  
  # Compute the entropy and warn/adjust the y.height if necessary
  entropy_measure <- sapply(segments, entropy_f)
  if (max(entropy_measure) > y.height) {
    warning("Entropy measured exceeds the y.height value;  Adjusting y.height upwards")
    y.height <- round(max(entropy_measure) + .5, 0)
  }

  barplot(height = entropy_measure, width = seg_len / sum(seg_len), space = 0, ylim=c(0, y.height))
}
