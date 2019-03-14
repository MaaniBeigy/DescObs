x <- c(
    0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
    4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
)
SampleQuantiles$new(x)$qx()
percentile_95 <-  SampleQuantiles$new(x, na.rm = TRUE, digits = 2, probs = 0.95)
percentile_95$qx()
percentile_75 <-  SampleQuantiles$new(x, na.rm = TRUE, digits = 3, probs = 0.75)
percentile_75$qx()
R6::is.R6(percentile_95)

