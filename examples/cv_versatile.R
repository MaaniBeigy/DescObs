x <- c(
    0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
    4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
)
cv_versatile(x)
cv_versatile(x, correction = TRUE)
cv_versatile(x, na.rm = TRUE, digits = 3, method = "kelley", correction = TRUE)
cv_versatile(x, na.rm = TRUE, method = "mahmoudvand_hassani", correction = TRUE)
cv_versatile(x, na.rm = TRUE, digits = 3, method = "shortest_length", correction = TRUE)
cv_versatile(x, na.rm = TRUE, digits = 3, method = "norm", correction = TRUE)
cv_versatile(x, na.rm = TRUE, digits = 3, method = "all", correction = TRUE)

