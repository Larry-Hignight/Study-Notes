entropy_chart <- function(segments, y.height = 1) {
  seg_n <- sapply(segments, length)
  seg_proportion <- seg_n / sum(seg_n)
  seg_var <- sapply(segments, var)
  seg_var[seg_n == 0] <- 0

  # Adjust the y.height if necessary 
#  if (max(seg_var) > y.height) y.height <- round(max(seg_var) + .5, 0)

  barplot(height = seg_var,
          width = seg_proportion,
	  space = 0,
	  ylim=c(0, y.height))

  offset <- c(0, cumsum(seg_proportion[-length(seg_proportion)]))
            + (seg_proportion / 2)
  text(x = offset, y = y.height / 2, labels = names(segments))
}

