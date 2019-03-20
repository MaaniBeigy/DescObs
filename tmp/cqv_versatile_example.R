x <- c(
    0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
    4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
)
cqv_versatile(x)
cqv_versatile(x, na.rm = TRUE, digits = 2)
cqv_versatile(x, na.rm = TRUE, digits = 2, method = "bonett")
cqv_versatile(x, na.rm = TRUE, digits = 2, method = "norm")
cqv_versatile(x, na.rm = TRUE, digits = 2, method = "basic")
cqv_versatile(x, na.rm = TRUE, digits = 2, method = "perc")
cqv_versatile(x, na.rm = TRUE, digits = 2, method = "bca")
cqv_versatile(x, na.rm = TRUE, digits = 2, method = "all")

